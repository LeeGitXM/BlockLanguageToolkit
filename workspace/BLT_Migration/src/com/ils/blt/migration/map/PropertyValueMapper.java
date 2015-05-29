package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Copy property values from G2 blocks into Ignition. Use the 
 * database as a lookup to map property names between the two systems.
 */
public class PropertyValueMapper {
	private final String TAG = "PropertyValueMapper";
	private final Map<String,String> propertyValueMap;  // Lookup by Ignition property, G2value
	/** 
	 * Constructor: 
	 */
	public PropertyValueMapper() {
		propertyValueMap = new HashMap<>();
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

			rs = statement.executeQuery("select * from PropertyValueMap");
			while(rs.next())
			{
				String iProperty = rs.getString("Property");
				String g2Value = rs.getString("G2Value");
				String iValue = rs.getString("IgnitionValue");
				String key = makePropertyMapKey(iProperty,g2Value.toLowerCase());
				propertyValueMap.put(key,iValue);

			}
			rs.close();
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(TAG+".createMap "+e.getMessage());
		}
		finally {
			if( rs!=null) {
				try { rs.close(); } catch(SQLException ignore) {}
			}
		}
	}

	/**
	 * Munge a G2 property value into an ignition equivalent. If there is
	 * no modification found, indicate by returning null.
	 * @param propertyName
	 * @param g2Value the originally supplied value from a G2 export.
	 * @return
	 */
	public String getPropertyValueForIgnition(String propertyName,String g2Value) {
		String result = null;
		String key = makePropertyMapKey(propertyName,g2Value.toLowerCase());
		String modified = propertyValueMap.get(key);
		if( modified!=null ) result = modified;
		return result;
	}
	
	/**
	 * Create the key for lookup in the property map. Simply
	 * concatenate the property name and value. The key
	 * is case-insensitive.
	 */
	private String makePropertyMapKey(String pname,String val) {
		String key = pname.toUpperCase()+":"+val.toUpperCase();
		return key;
	}

}
	

