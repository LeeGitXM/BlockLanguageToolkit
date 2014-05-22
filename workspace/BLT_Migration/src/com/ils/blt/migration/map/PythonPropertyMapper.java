package com.ils.blt.migration.map;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.block.ProcessBlock;
import com.ils.block.annotation.ExecutableBlock;
import com.ils.block.common.BlockDescriptor;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.BlockStyle;
import com.ils.block.common.PalettePrototype;
import com.ils.block.common.PropertyType;
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
	private final String TAG = "PropertyMapper";
	private final Map<String,PrototypeSet> prototypeSetMap;       // Lookup by className
	private final Map<String,BlockProperty[]> propertyArrayMap;    // Lookup by className
	/** 
	 * Constructor: 
	 */
	public PythonPropertyMapper() {
		prototypeSetMap = new HashMap<String,PrototypeSet>();
		propertyArrayMap = new HashMap<String,BlockProperty[]>();
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
					PrototypeSet ps = new PrototypeSet(bd.getStyle(),bd.getEmbeddedIcon(),bd.getEmbeddedLabel(),bd.getIconPath(),
																bd.getEmbeddedFontSize(),bd.getPreferredWidth(),bd.getPreferredHeight());
					prototypeSetMap.put(bd.getBlockClass(),ps);
					BlockProperty[] properties = block.getProperties();
					propertyArrayMap.put(bd.getBlockClass(),properties);
				}
				else {
					System.err.println(String.format("%s: Class %s not a ProcessBlock",TAG,cls.getName()));
				}
			} 
			catch (InstantiationException ie) {
				System.err.println(String.format("%s: Exception instantiating block (%s)",TAG,ie.getLocalizedMessage()));
			} 
			catch (IllegalAccessException iae) {
				System.err.println(String.format("%s: Access exception (%s)",TAG,iae.getMessage()));
			}
			catch (Exception ex) {
				System.err.println(String.format("%s: Runtime exception (%s)",TAG,ex.getMessage()));
			}
		}
		// Now fill-in view attributes with any classes known to
		// the database. Hopefully these correspond to Python classes.
		ResultSet rs = null;
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

			rs = statement.executeQuery("select * from PythonPrototypes");
			while(rs.next())
			{
				String blockClass = rs.getString("BlockClass");
				String styleName = rs.getString("Style");
				BlockStyle style = BlockStyle.SQUARE;
				try {
					style = BlockStyle.valueOf(styleName.toUpperCase());
				}
				catch(IllegalArgumentException iae) {
					System.err.println(TAG+": Illegal value of style ("+styleName+") for class "+blockClass);
				}
				String eIcon = rs.getString("EmbeddedIconPath");
				String eLabel = rs.getString("EmbeddedLabel");
				String iPath = rs.getString("IconPath");
				int fontSize = (int)rs.getLong("FontSize");
				int width = (int)rs.getLong("Width");
				int height = (int)rs.getLong("Height");

				PrototypeSet ps = new PrototypeSet(style,eIcon,eLabel,iPath,fontSize,width,height);
				prototypeSetMap.put(blockClass,ps);
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

			rs = statement.executeQuery("select * from PythonBlockProperties");
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
	 * in particular, the palette prototype definition.
	 * 
	 * @param iblock outgoing Ignition equivalent
	 */
	public void setPrototypeAttributes(SerializableBlock iblock) {
		String cname = iblock.getClassName();
		if( cname!=null) {
			PrototypeSet protoset = prototypeSetMap.get(cname);
			if( protoset!=null ) {
				iblock.setEmbeddedIcon(protoset.getEmbeddedIcon());
				iblock.setEmbeddedLabel(protoset.getEmbeddedLabel());
				iblock.setEmbeddedFontSize(protoset.getEmbeddedFontSize() );
				iblock.setIconPath(protoset.getIconPath());
				iblock.setPreferredHeight(protoset.getPreferredHeight());
				iblock.setPreferredWidth(protoset.getPreferredWidth());
				iblock.setStyle(protoset.getStyle());
			}
			else {
				System.err.println(TAG+".setPrototypeAttributes: No map entry for "+cname);
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
		BlockProperty[] properties = propertyArrayMap.get(cname);
		if( properties!=null ) {
			iblock.setProperties(properties);
		}
		else {
			System.err.println(TAG+".setProperties: No properties found for class "+cname);
			System.err.println(TAG+".setProperties: ---- dump map ---\n "+properties);
		}
	}


	/**
	 * These are the palette prototype attributes; constant for all instances of a class.
	 */
	private class PrototypeSet {
		private final BlockStyle style;
		private final String embeddedIcon;       // 32x32 icon to place in block in designer
		private final String embeddedLabel;      // Label place in block in designer
		private final int    embeddedFontSize;; // Pointsin font for embedded label
		private final String iconPath;           // Path to icon that is the entire block
		private final int preferredHeight;
		private final int preferredWidth; 
		public PrototypeSet( BlockStyle s,String eIcon,String eLabel,String iPath,int eFont,int pWidth,int pHeight) {
			this.style = s;
			this.embeddedIcon = eIcon;
			this.embeddedLabel = eLabel;
			this.iconPath = iPath;
			this.embeddedFontSize = eFont;
			this.preferredWidth = pWidth;
			this.preferredHeight = pHeight;
		}
		public BlockStyle getStyle() { return this.style; }
		public String getEmbeddedIcon() {return embeddedIcon;}
		public String getEmbeddedLabel() {return embeddedLabel;}
		public int getEmbeddedFontSize() {return embeddedFontSize;}
		public String getIconPath() {return iconPath;}
		public int getPreferredHeight() {return preferredHeight;}
		public int getPreferredWidth() {return preferredWidth;}
	}
}
	

