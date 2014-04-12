package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.migration.G2Block;

/**
 * Convert a G2 classname into a BLT classname
 */
public class TagMapper {
	private static final String TAG = "ClassNameMapper";
	private static final String UNDEFINED_NAME = "com.ils.block.UNDEFINED";
	private final Map<String,String> classMap;     // Lookup by G2 classname
	/** 
	 * Constructor: 
	 */
	public TagMapper() {
		classMap = new HashMap<String,String>();
	}
	
	
	/**
	 * Perform a database lookup to create a map of G2
	 * block names to Ignition blocks.
	 * @param cxn open database connection
	 */
	public void createMap(Connection cxn) {
		ResultSet rs = null;
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.
			
			rs = statement.executeQuery("select * from ClassMap");
			while(rs.next())
			{
				String g2 = rs.getString("G2Class");
				String ignition = rs.getString("IgnitionClass");
				classMap.put(g2, ignition);
			}
			rs.close();
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(e.getMessage());
		}
		finally {
			if( rs!=null) {
				try { rs.close(); } catch(SQLException ignore) {}
			}
		}
	}
	
	/**
	 * Use our map to get the Ignition class name. Set the discovered
	 * name in the ignition block object. On error, print a warning
	 * message and insert a default class name. (This allows us to
	 * continue processing and collect all the errors at once).
	 * 
	 * @param g2block incoming G2 block
	 * @param iblock outgoing Ignition equivalent
	 */
	public void setClassName(G2Block g2block,SerializableBlock iblock) {
		String cname = classMap.get(g2block.getClassName());
		if( cname==null) {
			cname = UNDEFINED_NAME;
			System.err.println(TAG+".setClassName: "+g2block.getClassName()+" has no Ignition equivalent");
		}
		iblock.setClassName(cname);
	}
	
	
}
