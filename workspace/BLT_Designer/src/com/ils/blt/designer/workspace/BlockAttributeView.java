package com.ils.blt.designer.workspace;

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

/**
 * This is a special class that extends a ProcessBlockView to create a version that
 * displays a property value of a reference block.
 */
public class BlockAttributeView extends ProcessBlockView implements NotificationChangeListener {
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
	private BlockProperty backgroundColor = null;
	private BlockProperty blockId = null;
	private BlockProperty fontSize = null;
	private BlockProperty foregroundColor = null;
	private BlockProperty height = null;
	private BlockProperty format = null;
	private BlockProperty offsetX = null;
	private BlockProperty offsetY = null;
	private BlockProperty propName = null;
	private BlockProperty width = null;
	private BlockProperty value = null;
	private final UtilityFunctions fncs;
	/**
	 * Constructor: Used when a new block is created from the selection dialog. 
	 *              We do not have enough information to become a listener.
	 *              Become a listener once the block and property are defined.
	 */
	public BlockAttributeView(BlockDescriptor descriptor) {
		super(descriptor);
		this.fncs = new UtilityFunctions();
		initialize();  // Create properties.
	}
	/**
	 * Constructor used when diagram is de-serialized. Create a listener on the subject property.
	 * @param sb
	 */
	public BlockAttributeView(SerializableBlock sb) {
		super(sb);
		this.fncs = new UtilityFunctions();
		backgroundColor = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_BACKGROUND_COLOR);
		blockId = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_BLOCK_ID);
		fontSize = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FONT_SIZE);
		backgroundColor = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FOREGROUND_COLOR);
		height = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_HEIGHT);
		format = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT);
		offsetX = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_X);
		offsetY = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_Y);
		propName= getProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY);
		width = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_WIDTH);
		value   = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE);

		
		startListener();
	}
	/**
	 * Add properties that are required for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName(CLSS);
		// These properties define which block and property to display
		blockId = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_BLOCK_ID,"", PropertyType.STRING, false);
		setProperty(blockId);
		propName = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY,"Name", PropertyType.STRING, false);
		setProperty(propName);
		value = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE,"", PropertyType.STRING, false);
		setProperty(value);
		
		// These attributes defined how the display is configured
		backgroundColor = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_BACKGROUND_COLOR, "TRANSPARENT", PropertyType.COLOR,true);
		setProperty(backgroundColor);
		foregroundColor = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_FOREGROUND_COLOR, "BLACK", PropertyType.COLOR,true);
		setProperty(foregroundColor);
		height = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_HEIGHT, BlockConstants.ATTRIBUTE_DISPLAY_HEIGHT, PropertyType.INTEGER,true);
		setProperty(height);		
		format = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT, "%s", PropertyType.STRING,true);
		setProperty(format);
		fontSize = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_FONT_SIZE, 10, PropertyType.INTEGER,true);
		setProperty(fontSize);
		offsetX = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_X, 0, PropertyType.INTEGER,false);
		setProperty(offsetX);		
		offsetY = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_Y, 0, PropertyType.INTEGER,false);
		setProperty( offsetY);
		width = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_WIDTH, BlockConstants.ATTRIBUTE_DISPLAY_WIDTH, PropertyType.INTEGER,true);
		setProperty(width);
	}
	
	public String getBlockId() { return this.blockId.getValue().toString(); }
	public void setBlockId(String id) { 
		this.blockId.setValue(id);
		if( !blockId.getValue().toString().isEmpty() &&
			!propName.getValue().toString().isEmpty() ) {
			startListener();
		}
	}
	
	public String getPropName()  {return this.propName.getValue().toString(); }
	public void setPropName(String name) { 
		this.propName.setValue(name);
		if( !blockId.getValue().toString().isEmpty() &&
			!propName.getValue().toString().isEmpty() ) {
				startListener();
		}
	}
	public String getValue()  {return this.value.getValue().toString(); }
	// When we change the value, we need to change the label
	public void setValue(String val) { this.value.setValue(val); }

	public String getBackgroundColor() { return backgroundColor.getValue().toString(); } 
	public int getFontSize() { return fncs.parseInteger(fontSize.getValue().toString()); }
	public String getForegroundColor() { return foregroundColor.getValue().toString(); }  
	public String getFormat()    { return format.getValue().toString(); }
	public void setFormat(String lbl) { this.format.setValue(lbl); }
	public int getOffsetX () { return fncs.parseInteger(offsetX.getValue().toString()); }
	public void setOffsetX(int offset) { this.offsetX.setValue(offset); }
	public int getOffsetY () { return fncs.parseInteger(offsetY.getValue().toString()); }
	public void setOffsetY(int offset) { this.offsetY.setValue(offset); }
	public int getPreferredHeight ()  { return fncs.parseInteger(height.getValue().toString()); }
	public int getPreferredWidth ()   { return fncs.parseInteger(width.getValue().toString()); }
	public ProcessBlockView getReferenceBlock() { return this.reference; }
	public void setReferenceBlock(ProcessBlockView ref) { this.reference=ref; }
	/**
	 * Start listening to the value of the indicated property block
	 */
	public void startListener() {
		String key = NotificationKey.keyForProperty(blockId.getValue().toString(), propName.getValue().toString());
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
	public void propertyChange(String pname,Object value) {}
	@Override
	public void stateChanged(ChangeEvent e) {}

	/**
	 * We are listening on a particular property, so the value change must be the right one.
	 */
	@Override
	public void valueChange(final QualifiedValue qv) {
		log.infof("%s.valueChange: - %s new value (%s)",CLSS,getPropName(),qv.getValue().toString());
		setValue(qv.getValue().toString());	
	}
}

