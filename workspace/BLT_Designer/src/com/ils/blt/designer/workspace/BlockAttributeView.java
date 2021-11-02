package com.ils.blt.designer.workspace;

import java.awt.Point;

import javax.swing.event.ChangeEvent;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.designer.NotificationHandler;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockListener;

/**
 * This is a special class that extends a ProcessBlockView to create a version that
 * displays a property value of a reference block.
 */
public class BlockAttributeView extends ProcessBlockView implements BlockListener, NotificationChangeListener {
	private static final String CLSS = "BlockAttributeView";
	public static final int ATTRIBUTE_DISPLAY_SEPARATION  = 30; // Default y separation
	public static final String DEFAULT_FONT = "SansSerif";      // Font family - Serif, Dialog,Monospaced
	public static final int DEFAULT_FONT_SIZE = 12;
	public static final int DEFAULT_HEIGHT = 25;
	public static final int DEFAULT_OFFSET_X = 50;
	public static final int DEFAULT_OFFSET_Y = 25;
	public static final int DEFAULT_WIDTH = 100;
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private ProcessBlockView reference = null;
	private PropertyType propertyType = PropertyType.STRING;
	private final UtilityFunctions fncs;
	/**
	 * Constructor: Used when a new block is created from the selection dialog. 
	 *              We do not have enough information to become a listener.
	 *              Become a listener once the block and property are defined.
	 */
	public BlockAttributeView(BlockDescriptor descriptor) {
		super(descriptor);
		this.fncs = new UtilityFunctions();
		setup();  // Create properties.
		initialize();
	}
	/**
	 * Constructor used when diagram is de-serialized. Create a listener on the subject property.
	 * Note we rely on the deserializer of the diagram to set the reference block.
	 * @param sb
	 */
	public BlockAttributeView(SerializableBlock sb) {
		super(sb);
		this.fncs = new UtilityFunctions();
		initialize();
		propertyType = getPropertyType(getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT).getValue().toString());
		startListener();
	}
	/**
	 * Add properties that are required for this class.
	 * Populate them with default values.
	 */
	private void setup() {
		setName(CLSS);
		// These properties define which block and property to display
		BlockProperty blockId = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_BLOCK_ID,"", PropertyType.STRING, false);
		setProperty(blockId);
		BlockProperty propName = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY,"Name", PropertyType.STRING, false);
		setProperty(propName);
		BlockProperty value = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE,"", PropertyType.STRING, false);
		setProperty(value);
		
		// These attributes defined how the display is configured
		BlockProperty bk = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_BACKGROUND_COLOR, "TRANSPARENT", PropertyType.COLOR,true);
		setProperty(bk);
		BlockProperty fg = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_FOREGROUND_COLOR, "BLACK", PropertyType.COLOR,true);
		setProperty(fg);
		BlockProperty height = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_HEIGHT, BlockConstants.ATTRIBUTE_DISPLAY_HEIGHT, PropertyType.INTEGER,true);
		setProperty(height);		
		BlockProperty format = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT, "%s", PropertyType.STRING,true);
		setProperty(format);
		BlockProperty fontSize = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_FONT_SIZE, 14, PropertyType.INTEGER,true);
		setProperty(fontSize);
		BlockProperty offsetX = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_X, 0, PropertyType.INTEGER,false);
		setProperty(offsetX);		
		BlockProperty offsetY = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_Y, 0, PropertyType.INTEGER,false);
		setProperty( offsetY);
		BlockProperty width = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_WIDTH, BlockConstants.ATTRIBUTE_DISPLAY_WIDTH, PropertyType.INTEGER,true);
		setProperty(width);
	}
	
	/**
	 *  Initialize property listeners. These are steps necessary with either constructor.
	 */
	private void initialize() {
		BlockProperty h = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_HEIGHT);
		String key = NotificationKey.keyForProperty(getBlockId(), h.getName());
		notificationHandler.addNotificationChangeListener(key,CLSS,this);
		setPreferredHeight(fncs.coerceToInteger(h.getValue().toString()));
		BlockProperty w = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_WIDTH);
		key = NotificationKey.keyForProperty(getBlockId(), w.getName());
		notificationHandler.addNotificationChangeListener(key,CLSS,this);;
		setPreferredWidth(fncs.coerceToInteger(w.getValue().toString()));
	}
	public String getBlockId() { return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_BLOCK_ID).getValue().toString(); }
	public void setBlockId(String id) { 
		BlockProperty blockId = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_BLOCK_ID);
		BlockProperty propName = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY);
		blockId.setValue(id);
		if( !blockId.getValue().toString().isEmpty() &&
			!propName.getValue().toString().isEmpty() ) {
			startListener();
		}
	}
	// For these properties, do not worry about case insensitivity.
	// Fetching directory from the map is more efficient.
	public BlockProperty getProperty(String nam) {
		return propertyMap.get(nam);
	}
	
	public String getPropName()  {return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY).getValue().toString(); }
	public void setPropName(String name) {
		BlockProperty blockId = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_BLOCK_ID);
		BlockProperty propName = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY);
		propName.setValue(name);
		if( !blockId.getValue().toString().isEmpty() &&
			!propName.getValue().toString().isEmpty() ) {
				startListener();
		}
	}
	private PropertyType getPropertyType(String format) {
		PropertyType type = PropertyType.OBJECT;  // Unknown format
		if( format.matches(".*%[0-9]*[.]?[0-9]*s.*") ) {
			type=PropertyType.STRING;
		}
		else if( format.matches(".*%[0-9]*d.*") ) {
			type=PropertyType.INTEGER;
		}
		else if( format.matches(".*%[0-9]*[.]?[0-9]*f.*") ) {
			type=PropertyType.DOUBLE;
		}
		return type;
	}
	// Return a string based on the value, property type and format
	public String getValue() {
		String format = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT).getValue().toString();
		String value = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE).getValue().toString(); 
		if( !value.isEmpty() ) {
			try {
				if( propertyType==PropertyType.DOUBLE) {
					value = String.format(format, fncs.coerceToDouble(value));
				}
				else if( propertyType==PropertyType.INTEGER) {
					value = String.format(format, fncs.coerceToInteger(value));
				}
				else {
					value = String.format(format,value);
				}
			}
			catch(Exception ex) {
				log.warnf("%s.getValue: error formatting %s with %s as %s (%s)",CLSS,
						value,format,propertyType.name(),ex.getMessage());
			}
		}
		return value;
	}
	// When we change the value, we need to change the label
	public void setValue(String val) { getProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE).setValue(val); }
	@Override
	public String getBackgroundColor() { return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_BACKGROUND_COLOR).getValue().toString(); } 
	public int getFontSize() { return fncs.parseInteger(getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FONT_SIZE).getValue().toString()); }
	public String getForegroundColor() { return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FOREGROUND_COLOR).getValue().toString(); }  
	public String getFormat()    { return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT).getValue().toString(); } 
	// Determine property type and check for validity
	public void setFormat(String lbl) { 
		propertyType = getPropertyType(lbl);
		if(propertyType.equals(PropertyType.OBJECT)) {
			getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT).setValue("%s"); 
		}
		else {
			getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT).setValue(lbl);
		}
	}
	public int getOffsetX () { return fncs.parseInteger(getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_X).getValue().toString()); }
	public void setOffsetX(int offset) { getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_X).setValue(offset); }
	public int getOffsetY () { return fncs.parseInteger(getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_Y).getValue().toString()); }
	public void setOffsetY(int offset) { getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_Y).setValue(offset); }
	/**.
	 * @return the block referenced by the display
	 */
	public ProcessBlockView getReferenceBlock() {  return this.reference; }
	public void setReferenceBlock(ProcessBlockView ref) { 
		this.reference=ref;
		reference.addBlockListener(this);
	}
	/**
	 * Start listening to the value of the indicated property block.
	 * Also listen for block movement on self and reference block
	 */
	public void startListener() {
		String key = NotificationKey.keyForProperty(getBlockId(), getPropName());
		notificationHandler.addNotificationChangeListener(key,CLSS,this);
		addBlockListener(this);
	}
	
	// ======================================= Notification Change Listener ===================================
	@Override
	public void bindingChange(String name,String binding) {}
	@Override
	public void diagramStateChange(long resId, String state) {}
	// We get this when another entity changes a property. We just need to re-display.
	@Override
	public void nameChange(String name) {
		if( getPropName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_NAME)) {
			setValue(name); 
		}
	}
	@Override
	public void propertyChange(String pname,Object value) {
		log.infof("%s.propertyChange: - %s new value (%s)",CLSS,pname,value);
		if( pname.equalsIgnoreCase(getPropName())) {
			setValue(value.toString());
		}
		else if( pname.equalsIgnoreCase(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT)) {
			setFormat(value.toString());
		}
		else if( pname.equalsIgnoreCase(BlockConstants.ATTRIBUTE_PROPERTY_HEIGHT)) {
			setPreferredHeight(fncs.coerceToInteger(value.toString()));
		}
		else if( pname.equalsIgnoreCase(BlockConstants.ATTRIBUTE_PROPERTY_WIDTH)) {
			setPreferredWidth(fncs.coerceToInteger(value.toString()));
		}
	}
	
	@Override
	public void stateChanged(ChangeEvent e) {
	}

	/**
	 * We are listening on a particular property, so the value change must be the right one.
	 */
	@Override
	public void valueChange(final QualifiedValue qv) {
		log.infof("%s.valueChange: - %s new value (%s)",CLSS,getPropName(),qv.getValue().toString());
		setValue(qv.getValue().toString());	
	}
	// ======================================= Block Listener ==================================
	@Override
	public void blockMoved(Block blk) {
		// If this block has moved, change the offsets appropriately
		if(blk.getId().equals(this.getId() ) ) {
			if(reference!=null) {
				int dx = getLocation().x - reference.getLocation().x ;
				setOffsetX(dx);
				int dy = getLocation().y - reference.getLocation().y ;
				setOffsetY(dy);
			}
			else {
				log.warnf("%s.blockMoved: - self move,reference block is null",CLSS);
			}
		}
		// Reference block has moved
		else {
			setLocation(new Point(reference.getLocation().x+getOffsetX(),reference.getLocation().y+getOffsetY()));
		}
		
	}
	@Override
	public void blockUIChanged(Block blk) {}
}

