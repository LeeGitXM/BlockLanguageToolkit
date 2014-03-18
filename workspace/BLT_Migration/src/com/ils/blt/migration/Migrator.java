/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.migration;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.sqlite.JDBC;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.migration.map.ClassAttributeMapper;
import com.ils.blt.migration.map.ClassNameMapper;
import com.ils.blt.migration.map.ConnectionMapper;

public class Migrator {
	private final static String TAG = "Migrator";
	private static final String USAGE = "Usage: migrator <database>";
	@SuppressWarnings("unused")
	private final static JDBC driver = new JDBC(); // Force driver to be loaded
	private boolean ok = true;                     // Allows us to short circuit processing
	private G2Diagram g2diagram = null;                  // G2 Diagram read from JSON
	private SerializableDiagram diagram = null;    // The result
	private final ClassNameMapper classMapper;
	private final ClassAttributeMapper attributeMapper;
	private final ConnectionMapper connectionMapper;

	 
	public Migrator() {
		classMapper = new ClassNameMapper();
		attributeMapper = new ClassAttributeMapper();
		connectionMapper = new ConnectionMapper();
	}
	
	public void processDatabase(String path) {
		String connectPath = "jdbc:sqlite:"+path;

		// Read database to generate conversion maps
		Connection connection = null;
		try {
			connection = DriverManager.getConnection(connectPath);
			classMapper.createMap(connection);
			attributeMapper.createMap(connection);
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(TAG+e.getMessage());
			ok = false;
		}
		finally {
			try {
				if(connection != null)
					connection.close();
			} 
			catch(SQLException e) {
				// connection close failed.
				System.err.println(TAG+e.getMessage());
			}
		}
	}
	
	/**
	 * Read standard input. Convert into G2 Diagram
	 */
	public void processInput() {
		if( !ok ) return;
		
		// Read of stdin is expected to be from a re-directed file. 
		// We gobble the whole thing here.
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		StringBuffer input = new StringBuffer();
		String s = null;
		try{
			while ((s = in.readLine()) != null && s.length() != 0) {
				input.append(s);
			}
		}
		catch(IOException ignore) {}
		
		// Now convert into a G2 Diagram
		try {
			byte[] bytes = input.toString().getBytes();
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(JsonParser.Feature.ALLOW_COMMENTS, true);
			mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
			g2diagram = mapper.readValue(new String(bytes), G2Diagram.class);
			if( g2diagram==null ) {
				System.err.println(TAG+": Failed to deserialize input");
				ok = false;
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
	 * Convert from G2 objects into BLTView objects
	 */
	public void migrate() {
		if( !ok ) return;
		
		diagram = new SerializableDiagram();
		diagram.setName(g2diagram.getName());
		List<SerializableBlock> blocks = new ArrayList<SerializableBlock>();
		for( G2Block g2block:g2diagram.getBlocks()) {
			SerializableBlock block = new SerializableBlock();
			block.setId(g2block.getId());
			block.setOriginalId(g2block.getId());
			block.setLabel(g2block.getLabel());
			block.setX(g2block.getX());
			block.setY(g2block.getY());
			classMapper.setClassName(g2block, block);
			attributeMapper.setClassAttributes(block);
			connectionMapper.setAnchors(g2block,block);
			blocks.add(block);
			diagram.setBlocks(blocks.toArray(new SerializableBlock[blocks.size()]));
		}
		
		// Finally we analyze the diagram as a whole to deduce connections
		connectionMapper.createConnections(g2diagram, diagram);
	}
	
	/**
	 * Write the BLT View Objects to std out
	 */
	public void createOutput() {
		if( !ok ) return;
		
		ObjectMapper mapper = new ObjectMapper();
		try{ 
			String json = mapper.writeValueAsString(diagram);
			System.out.println(json);
		}
		catch(JsonProcessingException jpe) {
			System.err.println(TAG+": Unable to serialize migrated diagram");
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
			System.out.println(USAGE);
			System.exit(1);
		}
		// Some of the embedded jars use log4j - redirect to std error. Log level is system property "log.level"
		ConsoleAppender appender = new ConsoleAppender(new PatternLayout(PatternLayout.TTCC_CONVERSION_PATTERN),"System.err");
		BasicConfigurator.configure(appender);
		String levelString = System.getProperty("log.level");
		Level level = Level.WARN;
		if( levelString!=null) level = Level.toLevel(levelString);
        Logger.getRootLogger().setLevel(level); //set log level
      
		Migrator m = new Migrator();
		String path = args[0];
		// In case we've been fed a Windows path, convert
		path = path.replace("\\", "/");
		m.processDatabase(path);
		m.processInput();
		m.migrate();
		m.createOutput();
	}

}
