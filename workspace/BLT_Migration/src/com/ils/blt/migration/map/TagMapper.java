package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.serializable.SerializableBlock;

/**
 * Convert a GSI names into Ignition Tag Paths
 */
public class TagMapper {
	private static final String TAG = "TagMapper";
	private static String TAG_PROVIDER_PREF_KEY = "TagProvider";
	private final Map<String,String> preferences;
	private final Map<String,String> tagMap;     // Lookup by G2 classname
	/** 
	 * Constructor: 
	 */
	public TagMapper() {
		preferences = new HashMap<>();
		tagMap = new HashMap<String,String>();
	}
	
	
	/**
	 * Perform a database lookup to create a map of G2
	 * block names to Ignition blocks. We create an additional
	 * map of preferences - in particular we're after the tag 
	 * provider name.
	 * @param cxn open database connection
	 */
	public void createMap(Connection cxn) {
		ResultSet rs = null;
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.
			
			rs = statement.executeQuery("select * from TagMap");
			while(rs.next())
			{
				String gsi = rs.getString("GSIName");
				String tagPath = rs.getString("TagPath");
				tagMap.put(gsi, tagPath.trim());
				//System.err.println(TAG+".setTagPaths: MAP "+gsi+":"+tagPath);
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
		
		// Now the preferences ...
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.
			
			rs = statement.executeQuery("select * from PreferenceMap");
			while(rs.next())
			{
				String name = rs.getString("Name");
				String value = rs.getString("Value");
				preferences.put(name,value);
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
	 * Use our map to get the Ignition tag paths. Search the block's properties for any that 
	 * are TAG. Convert the value via our map and set it in the binding.
	 *
	 * @param iblock Ignition block
	 */
	public void setTagPaths(SerializableBlock iblock) {
		//System.err.println(TAG+".setTagPaths Block = "+iblock.getName()+"("+iblock.getClassName()+")");
		if( iblock.getProperties()!=null)  {   // No properties, nothing to do
			for(BlockProperty bp:iblock.getProperties()) {
				if(bp.getName()==null || bp.getName().length()==0 ) {
					System.err.println(TAG+".setTagPaths: No-name tag for block "+iblock.getName()+" ("+iblock.getClassName()+")");
					continue;
				}
				if( bp.getBindingType().equals(BindingType.TAG_MONITOR) ||
					bp.getBindingType().equals(BindingType.TAG_READ) ||
					bp.getBindingType().equals(BindingType.TAG_WRITE)) {
					if( bp.getValue()!=null ) {
						String unmapped = bp.getValue().toString();
						// In the case of a source or sink, the name of the block correlates to the path
						if( unmapped==null || unmapped.length()==0) unmapped = iblock.getName();
						String mapped = tagMap.get(unmapped.trim());
						if( mapped!=null) {
							bp.setBinding(setProvider(mapped));
							//System.err.println(TAG+".setTagPaths BINDING "+iblock.getName()+"("+bp.getName()+") = "+mapped);
							bp.setValue("");  // Clear the value because we're bound to a tag
						}
						else {
							System.err.println(TAG+".setTagPaths "+iblock.getName()+"("+iblock.getClassName()+"):"+bp.getName()+"="+unmapped+" is not mapped to a tag path");
						}
					}
					else {
						System.err.println(TAG+".setTagPaths value is not set for use as a tag path");
					}
				}
				// Most likely a parameter ...lookup by the block name
				else if(bp.getBindingType().equals(BindingType.TAG_READWRITE) ) {
					String unmapped = iblock.getName();
					String mapped = tagMap.get(unmapped.trim());
					if( mapped!=null) {
						bp.setBinding(setProvider(mapped));
						//System.err.println(TAG+".setTagPaths PARAM BINDING "+iblock.getName()+"("+bp.getName()+") = "+mapped+":"+bp.hashCode());
						bp.setValue("");  // Clear the value because we're bound to a tag
					}
					else {
						System.err.println(TAG+".setTagPaths Parameter "+iblock.getName()+" ("+iblock.getClassName()+"):"+bp.getName()+"="+unmapped+" is not mapped to a tag path");
					}
				}
			}
		}
	}
	
	// The tag path starts with []. Fill this with the configured provider
	// name from our preferences.
	private String setProvider(String path) {
		String result = path;
		int index = path.indexOf(']');
		if( index>0 ) path = path.substring(index+1);
		result = String.format("[%s]%s", preferences.get(TAG_PROVIDER_PREF_KEY),path);
		//System.err.println(TAG+".setProvider="+result);
		return result;
	}
}
