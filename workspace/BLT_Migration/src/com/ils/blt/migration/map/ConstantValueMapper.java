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
 * Set values of properties based purely on the G2 Class.
 * These properties are set to read-only.
 */
public class ConstantValueMapper {
	private final String TAG = "ConstantValueMapper";
	private final Map<String,String> propertyMap;     // Lookup by G2 property name
	/** 
	 * Constructor: 
	 */
	public ConstantValueMapper() {
		propertyMap = new HashMap<String,String>();
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

			rs = statement.executeQuery("select * from ClassPropertiesMap");
			while(rs.next())
			{
				String g2Property = rs.getString("G2Name");
				String iProperty = rs.getString("IgnitionName");
				
				propertyMap.put(g2Property,iProperty);
			}
			rs.close();
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(TAG+"createMap: "+e.getMessage());
		}
		finally {
			if( rs!=null) {
				try { rs.close(); } catch(SQLException ignore) {}
			}
		}
	}

	/**
	 * Perform a database lookup of class name. Set the discovered
	 * name in the ignition block object. On error print a warning
	 * message and insert a default class name. (This allows us to
	 * continue processing and collect all the errors at once).
	 * 
	 * We also set other attributes that can be deduced from the name,
	 * in particular:
	 * 
	 * @param iblock outgoing Ignition equivalent
	 */
	public void setPropertyValues(G2Block g2Block,SerializableBlock iblock) {
		
	}

}
	

