package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import com.ils.block.common.BindingType;
import com.ils.block.common.BlockProperty;
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
				tagMap.put(gsi, tagPath);
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
		for(BlockProperty bp:iblock.getProperties()) {
			if( bp.getBinding().equals(BindingType.TAG.name()) ) {
				String unmapped = bp.getValue().toString();
				String mapped = tagMap.get(unmapped);
				if( mapped!=null) {
					bp.setValue(mapped);
				}
				else {
					System.err.println(TAG+".setTagPaths "+unmapped+" is not mapped to a tag path");
				}
			}
		}

	}
}
