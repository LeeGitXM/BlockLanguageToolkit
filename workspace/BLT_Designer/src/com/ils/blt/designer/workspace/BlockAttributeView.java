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
	private BlockProperty blockId = null;
	private BlockProperty propName = null;
	private BlockProperty value = null;
	private final UtilityFunctions fncs;
	/**
	 * Constructor: Used when a new block is created from the selection dialog. 
	 *              We do not have enough information to become a listener.
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
		blockId = getProperty(BlockConstants.ATTRIBUTE_DISPLAY_BLOCK_ID);
		propName= getProperty(BlockConstants.ATTRIBUTE_DISPLAY_PROPERTY);
		value   = getProperty(BlockConstants.ATTRIBUTE_DISPLAY_VALUE);
	}
	/**
	 * Add properties that are required for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName(CLSS);
		// These two properties define which property to display
		blockId = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_BLOCK_ID,"", PropertyType.STRING, false);
		setProperty(blockId);
		propName = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_PROPERTY,"Name", PropertyType.STRING, false);
		setProperty(propName);
		value = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_VALUE,"", PropertyType.STRING, false);
		setProperty(value);
		
		// These attributes defined how the display is configured
		BlockProperty property = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_WIDTH, Integer.valueOf(DEFAULT_WIDTH), PropertyType.INTEGER,true);
		setProperty(property);		
		property = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_HEIGHT, Integer.valueOf(DEFAULT_HEIGHT), PropertyType.INTEGER,true);
		setProperty(property);		
		property = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_BACKGROUND_COLOR, "TRANSPARENT", PropertyType.COLOR,true);
		setProperty(property);
		property = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_X, Integer.valueOf(DEFAULT_WIDTH), PropertyType.INTEGER,true);
		setProperty(property);		
		property = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_Y, Integer.valueOf(DEFAULT_HEIGHT), PropertyType.INTEGER,true);
		setProperty(property);
	}
	
	public String getBlockId() { return this.blockId.getValue().toString(); }
	public void setBlockId(String id) { this.blockId.setValue(id); }
	public String getPropName()  {return this.propName.getValue().toString(); }
	public void setPropName(String name) { this.propName.setValue(name); }
	public String getValue()  {return this.value.getValue().toString(); }
	public void setValue(String val) { this.value.setValue(val); }

	public int getOffsetX () { return fncs.parseInteger(this.getProperty(BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_X).getValue().toString()); }
	public int getOffsetY () { return fncs.parseInteger(this.getProperty(BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_Y).getValue().toString()); }
	
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

