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
				String iProperty = rs.getString("Property");
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
	 * In addition we make custom value conversions here.
	 * 
	 * @param g2Bock block created from G2
	 * @param iblock Ignition equivalent derived from the G2 block
	 */
	public void setProperties(G2Block g2Block,SerializableBlock iblock) {
		if( g2Block.getProperties()==null) return;  // Block has no properties
		for(G2Property g2property:g2Block.getProperties()) {
			String key = makePropertyMapKey(g2Block.getClassName(),g2property.getName());
			String propName = propertyMap.get(key);
			// The map contains an empty string if the property is to be intentionally ignored
			if( propName!=null && propName.length()>0 ) {
				BlockProperty[] props = iblock.getProperties();
				if( props!=null) {
					boolean found = false;
					for(BlockProperty bp: props ) {
						if( bp.getName().equalsIgnoreCase(propName) ) {
							found = true;
							//System.err.println(TAG+".setProperties "+key+"="+bp.getName()+", value is "+g2property.getValue());
							if(g2property.getValue()!=null && !g2property.getValue().toString().equalsIgnoreCase("none")) {
								// Time conversion --- if the name contains "minutes", we convert to seconds
								if( g2property.getName().endsWith("InMinutes") ||
									g2property.getName().contains("TimeMin")	) {
									try {
										double dbl = Double.parseDouble(g2property.getValue().toString());
										bp.setValue(new Double(dbl*60.));
									}
									catch (NumberFormatException nfe) {
										System.err.println(String.format("%s.setProperties: Converting %s to seconds (%s)",TAG,g2property.getValue().toString(),nfe.getMessage()));
									}
								}
								else {
									bp.setValue(g2property.getValue());
								}
							}
							break;
						}
					}
					if( !found ) {
						System.err.println(TAG+".setProperties: No ignition property "+propName+" for "+g2Block.getClassName()+"."+g2property.getName()+" ("+g2property.getValue().toString()+")");
					}
				}
				else {
					iblock.setProperties(new BlockProperty[0]);
				}
				
			}
			else if( propName==null ) {
				System.err.println(TAG+".setProperties "+g2Block.getClassName()+"."+g2property.getName()+" is not defined in the properties map");
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
	

