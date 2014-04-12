package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import com.ils.blt.migration.G2Anchor;
import com.ils.blt.migration.G2Block;

/**
 * Fix the naming of stubs (anchors) to correspond to the names
 * expected by the Ignition objects.
 */
public class AnchorMapper {
	private final String TAG = "AnchorMapper";
	private final Map<String,String> portMap;     // Lookup by G2 class and port
	/** 
	 * Constructor: 
	 */
	public AnchorMapper() {
		portMap = new HashMap<String,String>();
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

			rs = statement.executeQuery("select * from AnchorMap");
			while(rs.next())
			{
				String g2Class = rs.getString("G2Class");
				String g2Port  = rs.getString("G2Port");
				String iPort   = rs.getString("Port");
				
				// The key is G2Class:G2Port (uppercase to make case insensitive)
				String key = makeKey(g2Class,g2Port);
				portMap.put(key,iPort);
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
			if(port!=null) anchor.setPort(port);
			else {
				System.err.println(TAG+".updateAnchorNames: Port name lookup failed for "+className+" ("+port+")");
			}
		}
	}
	
	private String makeKey(String clss,String port) {
		String key = clss+":"+port;
		return key.toUpperCase();
	}

}
	

