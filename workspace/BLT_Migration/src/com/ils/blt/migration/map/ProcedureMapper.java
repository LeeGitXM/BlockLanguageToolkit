package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.serializable.SerializableBlock;

/**
 * Map G2 Procedure names to Python module paths.
 */
public class ProcedureMapper {
	private final String TAG = "ProcedureMapper";
	private final Map<String,String> procedureMap;     // Lookup by G2 property name
	private ResultSet rs;
	/** 
	 * Constructor: 
	 */
	public ProcedureMapper() {
		procedureMap = new HashMap<String,String>();
	}

	/**
	 * For all classes, perform a database lookup to map attribute names.
	 * Key by Ignition name.
	 * 
	 * @param cxn open database connection
	 */
	public void createMap(Connection cxn) {
		rs = null;
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

			rs = statement.executeQuery("select * from ProcedureMap");
			while(rs.next())
			{
				String g2Procedure = rs.getString("G2Procedure");
				String pythonModule = rs.getString("IgnitionProcedure");
				
				procedureMap.put(g2Procedure.toLowerCase(),pythonModule);
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
	 * Use our map to get the Ignition tag paths. Search the block's properties for any that 
	 * are PROCEDURE. Convert the value via our map and change the property type to STRING.
	 *
	 * @param iblock Ignition block
	 */
	public void setPythonModuleNames(SerializableBlock iblock) {
		BlockProperty[] properties = iblock.getProperties();
		if( properties!=null)  {   // No properties, nothing to do
			for(BlockProperty bp:properties) {
				if(bp.getName()==null || bp.getName().length()==0 ) {
					System.err.println(TAG+".setPythonModuleNames: No name on a block property in "+iblock.getName()+" ("+iblock.getClassName()+") - ignored");
					continue;
				}
				// Look at "script" attributes to convert python.
				// Note: The script property type is unused.
				if( bp.getType().equals(PropertyType.SCRIPTREF) ||
				    bp.getName().equalsIgnoreCase("script")         ) {
					//System.err.println(TAG+".setPythonModuleNames: Convert "+iblock.getName()+"."+bp.getName()+" ("+bp.getValue()+")");
					String unmapped = bp.getValue().toString().toLowerCase();
					String converted = procedureMap.get(unmapped.trim());
					if( converted!=null) {
						bp.setValue(converted);  
					}
					else {
						System.err.println(TAG+".setPythonModuleNames "+iblock.getName()+" ("+iblock.getClassName()+"):"+bp.getName()+";"+unmapped+" is not mapped to a python module");
					}
				}
			}
		}
	}
}
	

