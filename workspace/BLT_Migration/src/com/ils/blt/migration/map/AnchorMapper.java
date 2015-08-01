package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.migration.G2Anchor;
import com.ils.blt.migration.G2Block;

/**
 * Fix the naming of stubs (anchors) to correspond to the names
 * expected by the Ignition objects.
 */
public class AnchorMapper {
	private final String TAG = "AnchorMapper";
	private final Map<String,String> annotationMap;  // Lookup by G2 class and port
	private final Map<String,String> portMap;        // Lookup by G2 class and port
	private final Map<String,List<SerializableAnchor>> anchorMap;
	/** 
	 * Constructor: 
	 */
	public AnchorMapper() {
		annotationMap = new HashMap<>();
		portMap = new HashMap<>();
		anchorMap = new HashMap<>();
	}

	/**
	 * For all classes, perform a database lookup to map attribute names.
	 * Key by Ignition name.
	 * 
	 * @param cxn open database connection
	 */
	public void createMap(Connection cxn) {
		// Read the database to create the map.
		ResultSet rs = null;
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

			rs = statement.executeQuery("select * from BltAnchorMap");
			while(rs.next())
			{
				String g2Class = rs.getString("G2Class");
				String g2Port  = rs.getString("G2Port");
				String iPort   = rs.getString("Port");
				String annote  = rs.getString("Annotation");
				
				// The key is G2Class:G2Port (uppercase to make case insensitive)
				String key = makeKey(g2Class,g2Port);
				portMap.put(key,iPort);
				if( annote!=null && annote.length()>0 ) {
					annotationMap.put(key,annote);
				}
			}
			rs.close();
			
			String SQL = "select * from BltPythonAnchorMap";
			rs = statement.executeQuery(SQL);
			while(rs.next())
			{
				String className = rs.getString("IgnitionClass");
				List<SerializableAnchor> anchors = anchorMap.get(className);
				if( anchors==null ) {
					anchors = new ArrayList<>();
					anchorMap.put(className, anchors);
				}
				String port   = rs.getString("Port");
				String cType  = rs.getString("ConnectionType");
				String direction = rs.getString("Direction");
				String annote  = rs.getString("Annotation");
				SerializableAnchor sa = new SerializableAnchor();
				sa.setAnnotation(annote);
				sa.setConnectionType(ConnectionType.valueOf(cType.toUpperCase()));
				sa.setDirection(AnchorDirection.valueOf(direction.toUpperCase()));
				sa.setDisplay(port);
				anchors.add(sa);
			}
			rs.close();
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(TAG+".createMap: "+e.getMessage());
		}
		finally {
			if( rs!=null) {
				try { rs.close(); } catch(SQLException ignore) {}
			}
		}
	}

	public void augmentAnchors(SerializableBlock block) {
		// Make a lookup table of the current anchors.
		Map<String,SerializableAnchor> blockAnchorMap = new HashMap<>();
		for(SerializableAnchor anchor:block.getAnchors()) {
			blockAnchorMap.put(anchor.getDisplay(), anchor);
		}
		// Now search the database for others
		List<SerializableAnchor> blockAnchorList = anchorMap.get(block.getClassName());
		if( blockAnchorList==null) return;  // Nothing to change
		boolean newPort = false;
		for(SerializableAnchor sa:blockAnchorList) {
			if( blockAnchorMap.get(sa.getDisplay())==null) {
				SerializableAnchor newAnchor = sa.clone();
				newAnchor.setParentId(block.getId());
				//System.err.println(TAG+".augmentAnchors: "+block.getClassName()+":"+newAnchor.getDisplay());
				blockAnchorMap.put(sa.getDisplay(),newAnchor);
				newPort = true;
			}
		}
		if( newPort==false ) return;  // No change
		Collection<SerializableAnchor> anchorList = blockAnchorMap.values();
		SerializableAnchor[] anchors = anchorList.toArray(new SerializableAnchor[anchorList.size()]);
		block.setAnchors(anchors);
	}
	/**
	 * Perform a table lookup of desired port name corresponding to the G2 class
	 * and currently assigned port name (same as the G2 name). Update the G2block 
	 * so that the proper port name can be used before anchor points and connections
	 * are generated.
	 * 
	 * @param g2block the G2 block from which the ignition block was derived
	 */
	public void updateAnchorNames(G2Block g2block) {
		String className = g2block.getClassName();
		for(G2Anchor anchor:g2block.getConnections()) {
			String key = makeKey(className,anchor.getPort());
			String port = portMap.get(key);
			if( port!=null) {
				anchor.setPort(port);
				String annote = annotationMap.get(key); 
				anchor.setAnnotation(annote);
			}
			// If the G2 name is null, then we'll just use a default name by direction
			else if(anchor.getPort()==null || anchor.getPort().equalsIgnoreCase("NONE")) {
				if( anchor.getAnchorDirection().equals(AnchorDirection.INCOMING)) {
					anchor.setPort(BlockConstants.IN_PORT_NAME); 
				}
				else  {
					anchor.setPort(BlockConstants.OUT_PORT_NAME); 
				}
			}
			else {
				System.err.println(TAG+".updateAnchorNames: Port name lookup failed for "+g2block.getName()+"("+className+") on port ("+anchor.getPort()+")");
			}
		}
	}
	
	private String makeKey(String clss,String port) {
		String key = clss+":"+port;
		return key.toUpperCase();
	}

}
	

