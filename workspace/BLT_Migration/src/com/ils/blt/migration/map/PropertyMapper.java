package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.migration.G2Block;
import com.ils.blt.migration.G2Property;

/**
 * Copy property values from G2 blocks into Ignition. Use the 
 * database as a lookup to map property names between the two systems.
 */
public class PropertyMapper {
	private final String TAG = "PropertyMapper";
	private final Map<String,String> propertyMap;     // Lookup by G2 property name
	/** 
	 * Constructor: 
	 */
	public PropertyMapper() {
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

			rs = statement.executeQuery("select * from PropertyMap");
			while(rs.next())
			{
				String g2Class = rs.getString("G2Class");
				String g2Property = rs.getString("G2Property");
				String iProperty = rs.getString("Name");
				String key = makePropertyMapKey(g2Class,g2Property);
				propertyMap.put(key,iProperty);
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
	 * Perform a table lookup of class name. Set the discovered
	 * name in the ignition block object. On error print a warning
	 * message and insert a default class name. (This allows us to
	 * continue processing and collect all the errors at once).
	 * 
	 * We also set other attributes that can be deduced from the name,
	 * in particular:
	 * 
	 * @param g2Bock block created from G2
	 * @param iblock Ignition equivalent derived from the G2 block
	 */
	public void setProperties(G2Block g2Block,SerializableBlock iblock) {
		if( g2Block.getProperties()==null) return;  // Block has no properties
		for(G2Property g2property:g2Block.getProperties()) {
			String key = makePropertyMapKey(g2Block.getClassName(),g2property.getName());
			String propName = propertyMap.get(key);
			if( propName!=null ) {
				BlockProperty[] props = iblock.getProperties();
				if( props!=null) {
					for(BlockProperty bp: props ) {
						if( bp.getName().equalsIgnoreCase(propName) ) {
							//System.err.println(TAG+".setProperties match = "+key+", value = "+g2property.getValue());
							if(g2property.getValue()!=null && !g2property.getValue().toString().equalsIgnoreCase("none")) {
								bp.setValue(g2property.getValue());
							}
							break;
						}
					}
				}
				else {
					iblock.setProperties(new BlockProperty[0]);
				}
				
			}
			else {
				System.err.println(TAG+".setProperties "+key+" is undefined");
			}
		}
	}
	
	/**
	 * Create the key for lookup in the property map. Simply
	 * concatenate the class name and the property. The key
	 * is case-insensitive.
	 */
	private String makePropertyMapKey(String cname, String pname) {
		String key = cname.toUpperCase()+":"+pname.toUpperCase();
		return key;
	}

}
	

