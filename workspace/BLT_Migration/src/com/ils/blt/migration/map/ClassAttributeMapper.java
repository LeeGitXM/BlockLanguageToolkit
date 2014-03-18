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
 * Given a block class, set those attributes that are purely
 * dependent on that class, e.g. style of block when drawn.
 * 
 * For those blocks that are Java, we get the attributes directly
 * from object instances, for Python-implemented blocks, we 
 * get the information from the database.
 */
public class ClassAttributeMapper {
	private final String TAG = "ClassAttributeMapper";
	private final Map<String,ClassAttributeSet> attributeMap;     // Lookup by className
	private final Map<String,BlockProperty[]> propertyArrayMap;    // Lookup by className
	/** 
	 * Constructor: 
	 */
	public ClassAttributeMapper() {
		attributeMap = new HashMap<String,ClassAttributeSet>();
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
					ClassAttributeSet as = new ClassAttributeSet(bd.getStyle(),bd.getEmbeddedIcon(),bd.getEmbeddedLabel(),bd.getIconPath(),
																bd.getEmbeddedFontSize(),bd.getPreferredWidth(),bd.getPreferredHeight());
					attributeMap.put(bd.getBlockClass(),as);
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

			rs = statement.executeQuery("select * from ViewAttributesMap");
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

				ClassAttributeSet as = new ClassAttributeSet(style,eIcon,eLabel,iPath,fontSize,width,height);
				attributeMap.put(blockClass,as);
			}
			rs.close();
		}
		catch(SQLException e) {
			// if the error message is "out of memory", 
			// it probably means no database file is found
			System.err.println(TAG+": ViewAttributesMap "+e.getMessage());
		}
		finally {
			if( rs!=null) {
				try { rs.close(); } catch(SQLException ignore) {}
			}
		}
		// Repeat for property attributes
		rs = null;
		try {
			Statement statement = cxn.createStatement();
			statement.setQueryTimeout(30);  // set timeout to 30 sec.

			rs = statement.executeQuery("select * from BlockProperties");
			// We really need arrays. Use lists as we iterate through database.
			Map<String,List<BlockProperty>> propertyListMap = new HashMap<String,List<BlockProperty>>();
			while(rs.next())
			{
				String className = rs.getString("BlockClass");
				List<BlockProperty> pl = propertyListMap.get(className);
				if( pl==null ) pl = new ArrayList<BlockProperty>();
				BlockProperty property = new BlockProperty();
				property.setName(rs.getString("PropertyName"));
				PropertyType pt = PropertyType.OBJECT;
				String typeName = rs.getString("Type");
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
				double max = rs.getDouble("Maximum");
				if(!rs.wasNull()) property.setMaximum(max);
				double min = rs.getDouble("Minimum");
				if(!rs.wasNull()) property.setMinimum(min);
				pl.add(property);
			}
			rs.close();
			// Convert lists to attributes
			for( String className:propertyListMap.keySet() ) {
				List<BlockProperty> bplist = propertyListMap.get(className);
				BlockProperty[] bpa = (BlockProperty[])bplist.toArray();
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
	public void setClassAttributes(SerializableBlock iblock) {
		String cname = iblock.getClassName();
		if( cname!=null) {
			ClassAttributeSet fas = attributeMap.get(cname);
			if( fas!=null ) {
				iblock.setEmbeddedIcon(fas.getEmbeddedIcon());
				iblock.setEmbeddedLabel(fas.getEmbeddedLabel());
				iblock.setEmbeddedFontSize(fas.getEmbeddedFontSize() );
				iblock.setIconPath(fas.getIconPath());
				iblock.setPreferredHeight(fas.getPreferredHeight());
				iblock.setPreferredWidth(fas.getPreferredWidth());
				iblock.setStyle(fas.getStyle());
			}
			else {
				System.err.println(TAG+": No map entry for "+cname);
			}
		}
		else {
			System.err.println(TAG+": Block has no class");
		}
	}


	/**
	 * These are attributes that are constant for all instances of a class.
	 */
	private class ClassAttributeSet {
		private final BlockStyle style;
		private final String embeddedIcon;       // 32x32 icon to place in block in designer
		private final String embeddedLabel;      // Label place in block in designer
		private final int    embeddedFontSize;; // Pointsin font for embedded label
		private final String iconPath;           // Path to icon that is the entire block
		private final int preferredHeight;
		private final int preferredWidth; 
		public ClassAttributeSet( BlockStyle s,String eIcon,String eLabel,String iPath,int eFont,int pWidth,int pHeight) {
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
	

