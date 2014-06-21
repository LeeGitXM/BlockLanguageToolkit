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
	private final Map<String,String> tagMap;     // Lookup by G2 classname
	/** 
	 * Constructor: 
	 */
	public TagMapper() {
		tagMap = new HashMap<String,String>();
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
			
			rs = statement.executeQuery("select * from TagMap");
			while(rs.next())
			{
				String gsi = rs.getString("GSIName");
				String tagPath = rs.getString("TagPath");
				tagMap.put(gsi, tagPath.trim());
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
		if( iblock.getProperties()!=null)  {   // No properties, nothing to do
			for(BlockProperty bp:iblock.getProperties()) {
				//System.out.println(TAG+".setTagPaths: "+bp.getName()+", binding = "+bp.getBinding());
				if( bp.getBindingType().equals(BindingType.TAG_READ) ||
					bp.getBindingType().equals(BindingType.TAG_WRITE)) {
					if( bp.getValue()!=null ) {
						String unmapped = bp.getValue().toString();
						String mapped = tagMap.get(unmapped.trim());
						if( mapped!=null) {
							bp.setBinding(mapped);
							bp.setValue("");  // Clear the value because we're bound to a tag
						}
						else {
							System.err.println(TAG+".setTagPaths "+unmapped+" is not mapped to a tag path");
						}
					}
					else {
						System.err.println(TAG+".setTagPaths value is not set for use as a tag path");
					}
					
					
				}
			}
		}
	}
}
