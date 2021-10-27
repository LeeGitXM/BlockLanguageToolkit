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
	 * @param sb
	 */
	public BlockAttributeView(SerializableBlock sb) {
		super(sb);
		this.fncs = new UtilityFunctions();
		initialize();
		
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
	// For these ptoperties, do not worry about case insensitivity.
	// Fetching directoy from the map is more efficient.
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
	public String getValue()  {return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE).getValue().toString(); }
	// When we change the value, we need to change the label
	public void setValue(String val) { getProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE).setValue(val); }
	@Override
	public String getBackgroundColor() { return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_BACKGROUND_COLOR).getValue().toString(); } 
	public int getFontSize() { return fncs.parseInteger(getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FONT_SIZE).getValue().toString()); }
	public String getForegroundColor() { return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FOREGROUND_COLOR).getValue().toString(); }  
	public String getFormat()    { return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT).getValue().toString(); } 
	public void setFormat(String lbl) { getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT).setValue(lbl); }
	public int getOffsetX () { return fncs.parseInteger(getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_X).getValue().toString()); }
	public void setOffsetX(int offset) { getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_X).setValue(offset); }
	public int getOffsetY () { return fncs.parseInteger(getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_Y).getValue().toString()); }
	public void setOffsetY(int offset) { getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_Y).setValue(offset); }
	public ProcessBlockView getReferenceBlock() { return this.reference; }
	public void setReferenceBlock(ProcessBlockView ref) { this.reference=ref; }
	/**
	 * Start listening to the value of the indicated property block
	 */
	public void startListener() {
		String key = NotificationKey.keyForProperty(getBlockId(), getPropName());
		notificationHandler.addNotificationChangeListener(key,CLSS,this);
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
		if( pname.equalsIgnoreCase(BlockConstants.ATTRIBUTE_PROPERTY_WIDTH)) {
			setPreferredWidth(fncs.coerceToInteger(value.toString()));
		}
		else if( pname.equalsIgnoreCase(BlockConstants.ATTRIBUTE_PROPERTY_HEIGHT)) {
			setPreferredHeight(fncs.coerceToInteger(value.toString()));
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
			int dx = reference.getLocation().x - getLocation().x;
			setOffsetX(dx);
			int dy = reference.getLocation().y - getLocation().y;
			setOffsetY(dy);
		}
		// Reference block has moved
		else {
			setLocation(new Point(reference.getLocation().x+getOffsetX(),reference.getLocation().y+getOffsetY()));
		}
		
	}
	@Override
	public void blockUIChanged(Block blk) {}
}

