/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.migration;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.sqlite.JDBC;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.migration.map.AnchorMapper;
import com.ils.blt.migration.map.ClassNameMapper;
import com.ils.blt.migration.map.ConnectionMapper;
import com.ils.blt.migration.map.ProcedureMapper;
import com.ils.blt.migration.map.PropertyMapper;
import com.ils.blt.migration.map.PropertyValueMapper;
import com.ils.blt.migration.map.PythonPropertyMapper;
import com.ils.blt.migration.map.TagMapper;

public class Migrator {
	private final static String TAG = "Migrator";
	private static final String USAGE = "Usage: migrator <database>";
	@SuppressWarnings("unused")
	private final static JDBC driver = new JDBC(); // Force driver to be loaded
	private final static int MINX = 50;              // Allow whitespace around diagram.
	private final static int MINY = 50;
	private final static double SCALE_FACTOR = 1.25; // Scale G2 to Ignition positions
	private final RootClass root;
	private boolean ok = true;                     // Allows us to short circuit processing
	private G2Application g2application = null;    // G2 Application read from JSON
	private G2Diagram g2diagram = null;            // G2 Diagram read from JSON
	private SerializableApplication application = null;   // The result
	private SerializableDiagram diagram = null;           // An alternate result (obsolete)
	private final AnchorMapper anchorMapper;
	private final ClassNameMapper classMapper;
	private final ProcedureMapper procedureMapper;
	private final PythonPropertyMapper pythonPropertyMapper;
	private final ConnectionMapper connectionMapper;
	private final PropertyMapper propertyMapper;
	private final PropertyValueMapper propertyValueMapper;
	private final TagMapper tagMapper;


	 
	public Migrator(RootClass rc) {
		this.root = rc;
		anchorMapper = new AnchorMapper();
		classMapper = new ClassNameMapper();
		connectionMapper = new ConnectionMapper();
		procedureMapper = new ProcedureMapper();
		propertyMapper = new PropertyMapper();
		propertyValueMapper = new PropertyValueMapper();
		pythonPropertyMapper = new PythonPropertyMapper();
		tagMapper = new TagMapper();
	}
	
	public void processDatabase(String path) {
		String connectPath = "jdbc:sqlite:"+path;

		// Read database to generate conversion maps
		Connection connection = null;
		try {
			connection = DriverManager.getConnection(connectPath);
			anchorMapper.createMap(connection);
			classMapper.createMap(connection);
			procedureMapper.createMap(connection);
			propertyMapper.createMap(connection);
			propertyValueMapper.createMap(connection);
			pythonPropertyMapper.createMap(connection);
			tagMapper.createMap(connection);
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(TAG+": "+e.getMessage());
			ok = false;
		}
		finally {
			try {
				if(connection != null)
					connection.close();
			} 
			catch(SQLException e) {
				// connection close failed.
				System.err.println(TAG+": "+e.getMessage());
			}
		}
	}
	/**
	 * Read standard input. Convert into G2 Diagram
	 */
	public void processFileInput(Path infile) {
		if( !ok ) return;
		try {
			byte[] bytes = Files.readAllBytes(infile);
			convertToG2(bytes);
		}
		catch( IOException ioe) {
			System.err.println(String.format("%s.processFileInput: IOException (%s)",TAG,ioe.getLocalizedMessage())); 
			ok = false;
		}
	}
	/**
	 * Read standard input. Convert into G2 Diagram
	 */
	public void processStandardInput() {
		if( !ok ) return;

		// Read of stdin is expected to be from a re-directed file. 
		// We gobble the whole thing here. Scrub out CR
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		StringBuffer input = new StringBuffer();
		String s = null;
		try{
			while ((s = in.readLine()) != null && s.length() != 0) {
				s = s.replaceAll("\r", "");
				input.append(s);
			}
		}
		catch(IOException ignore) {}

		// Now convert into G2
		byte[] bytes = input.toString().getBytes();
		convertToG2(bytes);

	}

	private void convertToG2(byte[] bytes) {
		// Now convert into G2
		try {
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(JsonParser.Feature.ALLOW_COMMENTS, true);
			mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
			mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_CONTROL_CHARS, true);
			if( root==RootClass.APPLICATION) {
				g2application = mapper.readValue(new String(bytes), G2Application.class);
				if( g2application==null ) {
					System.err.println(TAG+": Failed to deserialize input application");
					ok = false;
				}
			}
			else {
				g2diagram = mapper.readValue(new String(bytes), G2Diagram.class);
				if( g2diagram==null ) {
					System.err.println(TAG+": Failed to deserialize input diagram");
					ok = false;
				}
			}
		}
		catch( IOException ioe) {
			System.err.println(String.format("%s: IOException (%s)",TAG,ioe.getLocalizedMessage())); 
			ok = false;
		}
		catch(Exception ex) {
			System.err.println(String.format("%s: Deserialization exception (%s)",TAG,ex.getMessage()));
			ok = false;
		}
	}
	/**
	 * Convert from G2 objects into a BLTView diagram
	 */
	public void migrateApplication() {
		if( !ok ) return;
		
		application = createSerializableApplication(g2application);
		connectionMapper.createConnections();
		connectionMapper.reconcileUnresolvedConnections();
		postProcess(application);
		
	}
	/**
	 * The root is a diagram. Convert from G2 objects into
	 * Convert from G2 objects into a BLTView diagram
	 */
	public void migrateDiagram() {
		if( !ok ) return;
		
		diagram = createSerializableDiagram(g2diagram);
	}
	
	private SerializableApplication createSerializableApplication(G2Application g2a) {
		SerializableApplication sa = new SerializableApplication();
		sa.setName(g2a.getName());
		sa.setId(UUID.nameUUIDFromBytes(g2a.getUuid().getBytes()));
		int familyCount = g2a.getFamilies().length;
		int index = 0;
		SerializableFamily[] families = new SerializableFamily[familyCount];
		for(G2Family fam:g2a.getFamilies()) {
			SerializableFamily sf = createSerializableFamily(fam);
			families[index] = sf;
			index++;
		}
		sa.setFamilies(families);
		return sa;
	}
	
	private SerializableFamily createSerializableFamily(G2Family g2f) {
		SerializableFamily sf = new SerializableFamily();
		sf.setName(g2f.getName());
		sf.setId(UUID.nameUUIDFromBytes(g2f.getUuid().getBytes()));
		int diagramCount = 0;
		// We have run into some empty diagrams in the G2 exports
		// Cull these out.
		for(G2Diagram diag:g2f.getProblems()) {
			if( diag.getName()!=null && diag.getName().length()>0 ) {
				diagramCount++;;
			}
		}
		int index = 0;
		SerializableDiagram[] diagrams = new SerializableDiagram[diagramCount];
		for(G2Diagram diag:g2f.getProblems()) {
			if( diag.getName()!=null && diag.getName().length()>0 ) {
				SerializableDiagram sd = createSerializableDiagram(diag);
				diagrams[index] = sd;
				index++;
			}
		}
		sf.setDiagrams(diagrams);
		return sf;
	}
	
	private SerializableDiagram createSerializableDiagram(G2Diagram g2d) {
		SerializableDiagram sd = new SerializableDiagram();
		sd.setName(g2d.getName());
		sd.setId(UUID.nameUUIDFromBytes(g2d.getName().getBytes()));  // Name is unique
		// Create the blocks before worrying about connections
		int blockCount = g2d.getBlocks().length;
		//System.err.println(String.format("%s: Block count = %d",TAG,blockCount));
		SerializableBlock[] blocks = new SerializableBlock[blockCount];
		
		// Compute positioning factors
		// So far this is just translation
		// For y value - G2 measures from the bottom, Ignition from the top.
		int minx = Integer.MAX_VALUE;
		int miny = Integer.MAX_VALUE;
		int maxx = Integer.MIN_VALUE;
		int maxy = Integer.MIN_VALUE;
		for( G2Block g2block:g2d.getBlocks()) {
			if(g2block.getX()<minx) minx = g2block.getX();
			if(g2block.getY()<miny) miny = g2block.getY();
			if(g2block.getX()>maxx) maxx = g2block.getX();
			if(g2block.getY()>maxy) maxy = g2block.getY();
		}
		double xoffset = MINX - minx*SCALE_FACTOR;  // Right margin
		double yoffset = MINY + maxy*SCALE_FACTOR;  // Top margin
		int index=0;
		for( G2Block g2block:g2d.getBlocks()) {
			SerializableBlock block = new SerializableBlock();
			block.setId(UUID.nameUUIDFromBytes(g2block.getUuid().getBytes()));
			block.setOriginalId(UUID.nameUUIDFromBytes(g2block.getUuid().getBytes()));
			block.setName(g2block.getName());
			block.setX((int)(xoffset + g2block.getX()*SCALE_FACTOR));
			block.setY((int)(yoffset - g2block.getY()*SCALE_FACTOR));
			classMapper.setClassName(g2block, block);
			pythonPropertyMapper.setPrototypeAttributes(block);
			pythonPropertyMapper.setProperties(block);
			propertyMapper.setProperties(g2block,block);
			anchorMapper.updateAnchorNames(g2block);   // Must precede connectionMapper
			connectionMapper.setAnchors(g2block,block);
			performSpecialHandlingOnBlock(g2block,block);
			// Need to set values here ...
			procedureMapper.setPythonModuleNames(block);
			tagMapper.setTagPaths(block);
			blocks[index]=block;
			index++;
		}
		sd.setBlocks(blocks);
		
		// Finally we analyze the diagram as a whole to deduce connections and partial connections
		connectionMapper.createConnectionSegments(g2d, sd);
		return sd;
	}

	
	/**
	 * Handle special cases that aren't as simple as a lookup. It helps to have the G2Block for reference.
	 */
	private void performSpecialHandlingOnBlock(G2Block g2Block,SerializableBlock block) {
		// 1) In G2 Connection Posts are bi-directional. We translate all
		//    of them to be outputs. They may actually be inputs.
		if( block.getClassName().endsWith("SinkConnection") ) {
			block.setBackground(new Color(127,127,127).getRGB()); // Dark gray
			block.setNameDisplayed(true);
			block.setNameOffsetX(25);
			block.setNameOffsetY(45);
			SerializableAnchor[] anchors = block.getAnchors();
			for(SerializableAnchor anc:anchors) {
				if( anc.getDirection().equals(AnchorDirection.OUTGOING)) {
					block.setClassName("com.ils.block.SourceConnection");
					break;
				}
			}
		}	
		// 2) G2 Moving Average handles both time and sample count.
		// We have special classes for these.
		else if( block.getClassName().startsWith("com.ils.block.MovingAverage")) {
			// Check the G2 Property "sampleType". 
			for(G2Property g2prop:g2Block.getProperties()) {
				if( g2prop.getName().equalsIgnoreCase("sampleType" )) {
					String val = g2prop.getValue().toString();
					if( val.equalsIgnoreCase("fixed" )) block.setClassName("com.ils.block.MovingAverage");
					else if( val.equalsIgnoreCase("point" )) block.setClassName("com.ils.block.MovingAverageSample");
					else if( val.equalsIgnoreCase("time" )) block.setClassName("com.ils.block.MovingAverageTime");
					else {
						System.err.println(String.format("%s: Perform specialhandling. MovingAverage sample type %s not recognized",TAG,val));
					}
				}
			}
		}
		// 3)  Input and output blocks are typed in G2
		else if( g2Block.getClassName().equalsIgnoreCase("GDL-NUMERIC-ENTRY-POINT") ||
				g2Block.getClassName().equalsIgnoreCase("GDL-DATA-PATH-CONNECTION-POST") ) {
			for( SerializableAnchor anchor:block.getAnchors()) {
				anchor.setConnectionType(ConnectionType.DATA);
			}
		}
		else if( g2Block.getClassName().equalsIgnoreCase("GDL-SYMBOLIC-ENTRY-POINT") ) {
			for( SerializableAnchor anchor:block.getAnchors()) {
				anchor.setConnectionType(ConnectionType.TEXT);
			}
		}
		else if( g2Block.getClassName().equalsIgnoreCase("GDL-INFERENCE-PATH-CONNECTION-POST") ) {
			for( SerializableAnchor anchor:block.getAnchors()) {
				anchor.setConnectionType(ConnectionType.TRUTHVALUE);
			}
		}
		// PersistenceGate - convert minutes to seconds
		else if( block.getClassName().startsWith("com.ils.block.PersistenceGate")) { 
			for(BlockProperty prop:block.getProperties()) {
				if( prop.getName().equalsIgnoreCase("TimeWindow" )) {
					try {
						double val = Double.parseDouble(prop.getValue().toString());
						prop.setValue(new Double(val*60));
					} 
					catch(NumberFormatException nfe) {
						System.err.println(String.format("%s: PersistenceGate delay is not a number (%s)",TAG,nfe.getMessage()));
					}
				}
			}
		}
		
		// Modify property values if appropriate
		if( block.getProperties()!=null ) {
			for(BlockProperty prop:block.getProperties()) {
				String newValue = propertyValueMapper.getPropertyValueForIgnition(prop.getName(),prop.getValue().toString());
				if( newValue!=null) prop.setValue(newValue);
			}
		}
		// Add anchors, if needed. The migration doesn't add anchors if there is no connection.
		anchorMapper.augmentAnchors(block);
		
	}
	/**
	 * Handle cleanup on the diagram as a whole.
	 * @param diagram
	 */
	private void performSpecialHandlingOnDiagram(SerializableDiagram sdiag) {
		// Remove redundant blocks
		List<SerializableBlock> blocksToDelete = new ArrayList<>();
		for( SerializableBlock block:sdiag.getBlocks()) {
			if(block.getClassName().startsWith("com.ils.block.Junction")) {
				SerializableAnchor[] anchors = block.getAnchors();
				if( anchors.length==2 && 
				    !anchors[0].getDirection().equals(anchors[1].getDirection()) ) {
					
					SerializableConnection incxn = null;
					SerializableConnection outcxn= null;
					for(SerializableConnection scxn:sdiag.getConnections()) {
						if( scxn==null ) {
							System.err.println(String.format("%s.NULL connection in diagram %s",TAG,sdiag.getName()));
							continue;
						}
						if(scxn.getBeginBlock()==null ) continue;   // Dangling connection
						if(scxn.getEndBlock()==null )   continue;
						if(scxn.getBeginBlock().equals(block.getId())) outcxn = scxn;
						else if(scxn.getEndBlock().equals(block.getId())) incxn = scxn;
					}
					if( incxn!=null && outcxn!=null) {
						//System.err.println(String.format("%s.JUNCTION %s: in connection %s",TAG,block.getName(),incxn.toString()));
						//System.err.println(String.format("%s.JUNCTION %s: out connection %s",TAG,block.getName(),incxn.toString()));
						// Make two connections into one, delete block
						incxn.setEndBlock(outcxn.getEndBlock());
						incxn.setEndAnchor(outcxn.getEndAnchor());
						if( !outcxn.getType().equals(ConnectionType.ANY)) incxn.setType(outcxn.getType());
						//System.err.println(String.format("%s.JUNCTION new connection %s",TAG,incxn.toString()));
						blocksToDelete.add(block);
						sdiag.removeConnection(outcxn);
					}
					else {
						System.err.println(String.format("%s: %s - unable to remove extraneous Junction (%s)",TAG,sdiag.getName(),block.getName()));
					}
				}
			}
		}
		
		for(SerializableBlock block:blocksToDelete) {
			sdiag.removeBlock(block);
		}
		
		// Look for input blocks connected to Inhibit blocks. Convert to LabData
		for( SerializableBlock block:sdiag.getBlocks()) {
			if(block.getClassName().startsWith("com.ils.block.Input")) {
				SerializableConnection incxn = null;
				SerializableConnection outcxn= null;
				for(SerializableConnection scxn:sdiag.getConnections()) {
					if( scxn==null ) {
						continue;
					}
					if(scxn.getBeginBlock()==null ) continue;   // Dangling connection
					if(scxn.getEndBlock()==null )   continue;
					if(scxn.getBeginBlock().equals(block.getId())) {
						UUID downstream = scxn.getEndBlock();
						for( SerializableBlock downstreamblock:sdiag.getBlocks()) {
							if( downstreamblock.getClassName().startsWith("com.ils.block.Inhibitor") &&
									downstream.equals(downstreamblock.getId() ) ) {
								// Found it!
								block.setClassName("com.ils.block.LabData");
								block.setEmbeddedLabel("Lab Data");
								block.setEmbeddedFontSize(16);
								block.setStyle(BlockStyle.ARROW);
								block.setPreferredHeight(50);
								block.setPreferredWidth(70);
								block.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_ROSE);
								
								BlockProperty[] properties = block.getProperties();
								BlockProperty[] newProperties = new BlockProperty[properties.length+1];
								int i = 0;
								for(BlockProperty bp:properties) {
									if( bp.getName().equalsIgnoreCase("TagPath")) {
										bp.setName("ValueTagPath");
										BlockProperty newProp = new BlockProperty();
										newProp.setName("TimeTagPath");
										String path = bp.getBinding();
										int pos = path.lastIndexOf("/");
										if(pos>0) {
											path = path.substring(0,pos+1);
											path = path + "sampleTime";
										}
										newProp.setBinding(path);
										newProp.setBindingType(bp.getBindingType());
										newProp.setEditable(bp.isEditable());
										newProp.setType(bp.getType());
										newProperties[properties.length] = newProp;
									}
									newProperties[i] = bp;
									i++;
								}
								block.setProperties(newProperties);
							}
						}
					}
					
				}
			}
		}
	}
	/**
	 * Perform any special processing after application is created.
	 * Currently this method finds all diagrams and cleans those up ... 
	 * We assume there are no intervening folders.
	 */
	private void postProcess(SerializableApplication app) {
		SerializableFamily[] fams = app.getFamilies();
		for(SerializableFamily fam:fams) {
			SerializableDiagram[] diags = fam.getDiagrams();
			for(SerializableDiagram sd:diags) {
				performSpecialHandlingOnDiagram(sd);
			}			
		}
	}
	/**
	 * Write the BLT View Objects to std out
	 */
	public void createOutput() {
		if( !ok ) return;
		
		ObjectMapper mapper = new ObjectMapper();
		
		try{ 
			String json = "";
			if( root==RootClass.APPLICATION) {
				json = mapper.writeValueAsString(application);
			}
			// diagram
			else {
				json = mapper.writeValueAsString(diagram);
			}
			 
			System.out.println(json);
		}
		catch(JsonProcessingException jpe) {
			System.err.println(String.format("%s: Unable to serialize migrated %s",TAG,root.toString()));
		}
	}
	
	/**
	 * Entry point for the application. 
	 * Usage: Migrator <databasepath> 
	 * 
	 * NOTE: For Windows, specify path as: C:/home/work/migrate.db
	 *       For Mac/Linux:    /home/work/migrate.db
	 * We automatically adjust windows path, if specified with backslashes.
	 * 
	 * @param args command-line arguments
	 */
	public static void main(String[] args) {
			
		// Look for database path as an argument
		if( args.length == 0) {
			System.err.println(USAGE);
			System.exit(1);
		}
		// Some of the embedded jars use log4j - redirect to std error. Log level is system property "log.level"
		ConsoleAppender appender = new ConsoleAppender(new PatternLayout(PatternLayout.TTCC_CONVERSION_PATTERN),"System.err");
		BasicConfigurator.configure(appender);
		String levelString = System.getProperty("log.level");
		Level level = Level.WARN;
		if( levelString!=null) level = Level.toLevel(levelString);
        Logger.getRootLogger().setLevel(level); //set log level
        
        RootClass root = RootClass.APPLICATION;
        String rootClass = System.getProperty("root.class");   // Application, Problem
		if( rootClass!=null) {
			try {
				root = RootClass.valueOf(rootClass);
			}
			catch(IllegalArgumentException iae) {
				System.err.println(String.format("%s: Unknown root.class (%s)",TAG,iae.getMessage()));
			}
		}
      
		Migrator m = new Migrator(root);
		String dbpath = args[0];
		// In case we've been fed a Windows path, convert
		// We're expecting an absolute path.
		dbpath = dbpath.replace("\\", "/");
		try {
			m.processDatabase(dbpath);
			if(args.length>1) {
				String filepath = args[1];
				filepath.replace("\\", "/");
				Path inpath = Paths.get(filepath);
				m.processFileInput(inpath);
			}
			else {
				m.processStandardInput();
			}
			if(root.equals(RootClass.APPLICATION) ) {
				m.migrateApplication();
			}
			// NOTE: This is obsolete (and probably buggy).
			else if(root.equals(RootClass.DIAGRAM)) {
				m.migrateDiagram();
			}
			m.createOutput();
		}
		catch(Exception ex) {
			System.err.println(String.format("%s.main: UncaughtException (%s)",TAG,ex.getMessage()));
			ex.printStackTrace(System.err);
		}
	}

}
