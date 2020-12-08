package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.common.ClassList;

/**
 * Given a block class create the proper set of properties.
 * 
 * For those blocks that are Java, we get the properties directly
 * from object instances, for Python-implemented blocks, we 
 * get the information from the database.
 */
public class PythonPropertyMapper {
	private final String TAG = "PythonPropertyMapper";
	private final Map<String,Map<String,String>> descriptorMap; // Lookup by className
	private final Map<String,BlockProperty[]> propertyArrayMap;       // Lookup by className
	private final UtilityFunctions fcns;
	/** 
	 * Constructor: 
	 */
	public PythonPropertyMapper() {
		descriptorMap = new HashMap<>();
		propertyArrayMap = new HashMap<String,BlockProperty[]>();
		fcns = new UtilityFunctions();
	}

	/**
	 * Instantiate each of the Java-implemented blocks. Extract 
	 * class-dependent fixed attributes (mostly rendering parameters).
	 * 
	 * For Python-implemented class, perform a database lookup.
	 * These actions in combination produce a map of class-dependent
	 * attributes, given an Ignition class name.
	 * 
	 * @param cxn open database connection
	 */
	public void createMap(Connection cxn) {
		// First iterate through all Java classes
		// The blocks implemented in Java have been copied into the migration jar during its build
		ClassList cl = new ClassList();
		List<Class<?>> classes = cl.getAnnotatedClasses("blt-migration", ExecutableBlock.class,"com/ils/block/");
		for( Class<?> cls:classes) {
			try {
				//System.err.println("found class: "+cls.getCanonicalName());
				Object obj = cls.newInstance();
				
				if( obj instanceof ProcessBlock ) {
					ProcessBlock block = (ProcessBlock)obj;
					PalettePrototype bp = block.getBlockPrototype();
					BlockDescriptor bd = bp.getBlockDescriptor();
					Map<String,String> descriptors = new HashMap<>(); 
					descriptors.put(BLTProperties.PALETTE_BLOCK_STYLE,bd.getStyle().name());
					descriptors.put(BLTProperties.PALETTE_EDITOR_CLASS,bd.getEditorClass());
					descriptors.put(BLTProperties.PALETTE_ICON_PATH,bp.getPaletteIconPath());
					descriptors.put(BLTProperties.PALETTE_LABEL,bp.getPaletteLabel());
					descriptors.put(BLTProperties.PALETTE_NAME_DISPLAYED,(bd.isNameDisplayed()?"true":"false"));
					descriptors.put(BLTProperties.PALETTE_NAME_OFFSET_X,String.valueOf(bd.getNameOffsetX()));
					descriptors.put(BLTProperties.PALETTE_NAME_OFFSET_Y,String.valueOf(bd.getNameOffsetY()));
//					descriptors.put(BLTProperties.PALETTE_RECEIVE_ENABLED,(bd.isReceiveEnabled()?"true":"false"));
//					descriptors.put(BLTProperties.PALETTE_TRANSMIT_ENABLED,(bd.isTransmitEnabled()?"true":"false"));
					descriptors.put(BLTProperties.PALETTE_TOOLTIP,bp.getTooltipText());
					descriptors.put(BLTProperties.PALETTE_TAB_NAME,bp.getTabName());
					descriptors.put(BLTProperties.PALETTE_VIEW_BACKGROUND,String.valueOf(bd.getBackground()));
					descriptors.put(BLTProperties.PALETTE_VIEW_BLOCK_ICON,bd.getIconPath());
					descriptors.put(BLTProperties.PALETTE_VIEW_FONT_SIZE,String.valueOf(bd.getEmbeddedFontSize()));
					descriptors.put(BLTProperties.PALETTE_VIEW_HEIGHT,String.valueOf(bd.getPreferredHeight()));
					descriptors.put(BLTProperties.PALETTE_VIEW_ICON,bd.getEmbeddedIcon());
					descriptors.put(BLTProperties.PALETTE_VIEW_LABEL,bd.getEmbeddedLabel());
					descriptors.put(BLTProperties.PALETTE_VIEW_WIDTH,String.valueOf(bd.getPreferredWidth()));

					descriptorMap.put(bd.getBlockClass(),descriptors);
					BlockProperty[] properties = block.getProperties();
					propertyArrayMap.put(bd.getBlockClass(),properties);
				}
				else {
					System.err.println(String.format("%s.create: Class %s not a ProcessBlock",TAG,cls.getName()));
				}
			} 
			catch (InstantiationException ie) {
				System.err.println(String.format("%s.createMap: Exception instantiating block (%s)",TAG,ie.getLocalizedMessage()));
			} 
			catch (IllegalAccessException iae) {
				System.err.println(String.format("%s.createMap: Access exception (%s)",TAG,iae.getMessage()));
			}
			catch (Exception ex) {
				System.err.println(String.format("%s.createMap: Runtime exception (%s)",TAG,ex.getMessage()));
				ex.printStackTrace(System.err);
			}
		}
		// Now fill-in view attributes with any classes known to
		// the database. Hopefully these correspond to Python classes.
		ResultSet rs = null;
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

			rs = statement.executeQuery("select * from BltPythonPrototypes");
			while(rs.next())
			{
				String blockClass = rs.getString("BlockClass");   // Python path
				String key = rs.getString("Key");
				String value = rs.getString("Value");
				
				Map<String,String> descriptors = descriptorMap.get(blockClass);
				if(descriptors == null) {
					descriptors = new HashMap<>();
					descriptorMap.put(blockClass, descriptors);
					
				}
				descriptors.put(key, value);
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
		// Repeat for properties that are defined for this block, no matter what.
		rs = null;
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

			rs = statement.executeQuery("select * from BltPythonBlockProperties");
			// We really need arrays. Use lists as we iterate through database.
			Map<String,List<BlockProperty>> propertyListMap = new HashMap<String,List<BlockProperty>>();
			while(rs.next())
			{
				String className = rs.getString("BlockClass");
				List<BlockProperty> pl = propertyListMap.get(className);
				if( pl==null ) {
					pl = new ArrayList<BlockProperty>();
					propertyListMap.put(className, pl);
				}
				BlockProperty property = new BlockProperty();
				property.setName(rs.getString("PropertyName"));
				PropertyType pt = PropertyType.OBJECT;
				String typeName = rs.getString("PropertyType");
				try {
					pt = PropertyType.valueOf(typeName.toUpperCase());
					property.setType(pt);
				}
				catch(IllegalArgumentException iae) {
					System.err.println(TAG+": Illegal value of type ("+typeName+") for class "+className);
				}
				long editable = rs.getLong("Editable"); 
				if(rs.wasNull()) editable = 0;
				property.setEditable(editable>0);

				String text = rs.getString("Value");
				if( text!=null && !text.equalsIgnoreCase("NONE")) {
					if(pt.equals(PropertyType.BOOLEAN)) {
						property.setValue(new Boolean(text.equalsIgnoreCase("true")));
					}
					else {
						property.setValue(text);
					}
				}
				
				pl.add(property);
			}
			rs.close();
			// Convert lists to properties
			for( String className:propertyListMap.keySet() ) {
				List<BlockProperty> bplist = propertyListMap.get(className);
				BlockProperty[] bpa = (BlockProperty[])bplist.toArray(new BlockProperty[bplist.size()]);
				propertyArrayMap.put(className, bpa);
			}
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(TAG+": "+e.getMessage());
		}
		finally {
			if( rs!=null) {
				try { rs.close(); } catch(SQLException ignore) {}
			}
		}
	}

	/**
	 * Perform a table lookup by class name. Set the discovered
	 * name in the ignition block object. On error print a warning
	 * message and insert a default class name. (This allows us to
	 * continue processing and collect all the errors at once).
	 * 
	 * We also set the class attributes that exist for all blocks,
	 * in particular, the palette prototype definition. Not all
	 * prototype attributes apply to a block inside the diagram
	 * (Some are purely for the palette).
	 * 
	 * @param iblock outgoing Ignition equivalent
	 */
	public void setPrototypeAttributes(SerializableBlock iblock) {
		String cname = iblock.getClassName();
		if( cname!=null) {
			Map<String,String> descriptors = descriptorMap.get(cname); 
			if( descriptors!=null ) {
				for(String key:descriptors.keySet()) {
					String value = descriptors.get(key);
					try {
						if( key.equalsIgnoreCase(BLTProperties.PALETTE_BLOCK_STYLE) ) {
							iblock.setStyle(BlockStyle.valueOf(value.toUpperCase()));
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_EDITOR_CLASS) ) {
							iblock.setEditorClass(value);
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_ICON_PATH) ) {
							;  // Palette icon (?)
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_LABEL) ) {
							;
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_NAME_DISPLAYED) ) {
							iblock.setNameDisplayed(value.equalsIgnoreCase("true"));
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_NAME_OFFSET_X) ) {
							iblock.setNameOffsetX(fcns.coerceToInteger(value));
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_NAME_OFFSET_Y) ) {
							iblock.setNameOffsetY(fcns.coerceToInteger(value));
						}
//						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_RECEIVE_ENABLED) ) {
//							iblock.setReceiveEnabled(value.equalsIgnoreCase("true"));
//						}
//						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_TRANSMIT_ENABLED) ) {
//							iblock.setTransmitEnabled(value.equalsIgnoreCase("true"));
//						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_TOOLTIP) ) {
							;
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_TAB_NAME) ) {
							;
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_VIEW_BACKGROUND) ) {
							iblock.setBackground(fcns.coerceToInteger(value));
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_VIEW_BLOCK_ICON) ) {
							iblock.setIconPath(value);
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_VIEW_FONT_SIZE) ) {
							iblock.setEmbeddedFontSize(fcns.coerceToInteger(value));
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_VIEW_HEIGHT) ) {
							iblock.setPreferredHeight(fcns.coerceToInteger(value));
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_VIEW_ICON) ) {
							iblock.setEmbeddedIcon(value);
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_VIEW_LABEL) ) {
							iblock.setEmbeddedLabel(value);
						}
						else if( key.equalsIgnoreCase(BLTProperties.PALETTE_VIEW_WIDTH) ) {
							iblock.setPreferredWidth(fcns.coerceToInteger(value));
						}

						else {
							System.err.println(TAG+".setPrototypeAttributes: Unknown prototype attribute "+key);
						}
					}
					catch(Exception ex) {
						System.err.println(TAG+".setPrototypeAttributes: Illegal conversion of "+key+"="+value);
					}

				}

			}
			else {
				System.err.println(TAG+".setPrototypeAttributes: Unknown class for prototype "+cname);
			}
		}
		else {
			System.err.println(TAG+".setPrototypeAttributes: Block has no class");
		}
	}
	
	/**
	 * Perform a table lookup by class name. Set an appropriate properties
	 * array for the block.
	 * 
	 * @param iblock outgoing Ignition equivalent
	 */
	public void setProperties(SerializableBlock iblock) {
		String cname = iblock.getClassName();
		if( propertyArrayMap.get(cname)!=null ) {
			BlockProperty[] propertiesTemplate = propertyArrayMap.get(cname);
			BlockProperty[] properties = new BlockProperty[propertiesTemplate.length];
			int index = 0;
			for(BlockProperty bp:propertiesTemplate) {
				properties[index] = bp.clone();
				index++;
			}
			iblock.setProperties(properties);
		}
		else {
			System.err.println(TAG+".setProperties: No properties found for class "+cname);
			//System.err.println(TAG+".setProperties: ---- dump map ---\n "+properties);
		}
	}
}
	

