package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NotificationHandler;
import com.inductiveautomation.ignition.client.tags.model.ClientTagManager;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.tags.model.TagPath;
import com.inductiveautomation.ignition.common.tags.model.event.TagChangeEvent;
import com.inductiveautomation.ignition.common.tags.model.event.TagChangeListener;
import com.inductiveautomation.ignition.common.tags.paths.parser.TagPathParser;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockListener;

/**
 * This is a special class that extends a ProcessBlockView to create a version that
 * displays a property value of a reference block.
 */
public class BlockAttributeView extends ProcessBlockView implements BlockListener, NotificationChangeListener,TagChangeListener {
	private static final String CLSS = "BlockAttributeView";
	private final static boolean DEBUG = false;
	public static final int ATTRIBUTE_DISPLAY_SEPARATION  = 30; // Default y separation
	public static final String DEFAULT_FONT = "SansSerif";      // Font family - Serif, Dialog,Monospaced
	public static final int DEFAULT_FONT_SIZE = 12;
	public static final int DEFAULT_HEIGHT = 25;
	public static final int DEFAULT_OFFSET_X = 50;
	public static final int DEFAULT_OFFSET_Y = 25;
	public static final int DEFAULT_WIDTH = 100;
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private ProcessBlockView reference = null;
	private String binding = null;  // Current binding, if any
	private PropertyType propertyType = PropertyType.STRING;
	private final UtilityFunctions fncs;
	/**
	 * Constructor: Used when a new block is created from the selection dialog. 
	 *              We do not have enough information to become a listener.
	 *              Become a listener once the block and property are defined.
	 */
	public BlockAttributeView(BlockDescriptor descriptor,String referenceBlockId) {
		super(descriptor);
		this.fncs = new UtilityFunctions();
		setup();  // Create property map.
		setBlockId(referenceBlockId);
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
		BlockProperty bp = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT);
		if( bp!=null && bp.getValue()!=null ) {
			propertyType = getPropertyType(bp.getValue().toString());
		}
		else {
			propertyType = PropertyType.OBJECT;  // Unknown
		}
	}
	/**
	 * For the attribute view, the properties come second hand from the reference block,
	 * not the gateway.
	 */
	@Override
	public void initProperties(ProjectResourceId parent) {
		initialize();
	}
	/**
	 * Add properties that are required for this class.
	 * Populate them with default values.
	 */
	private void setup() {
		createPseudoRandomName();  // Sets name without triggering listeners
		// These properties define which block and property to display
		BlockProperty bid = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_BLOCK_ID,"", PropertyType.STRING, false);
		setProperty(bid);
		BlockProperty propName = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY,"Name", PropertyType.STRING, false);
		setProperty(propName);
		BlockProperty propValue = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE,"", PropertyType.STRING, false);
		setProperty(propValue);
		
		// These attributes defined how the display is configured
		BlockProperty bk = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_BACKGROUND_COLOR, BLTProperties.TRANSPARENT, PropertyType.COLOR,true);
		setProperty(bk);
		BlockProperty fg = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_FOREGROUND_COLOR, BLTProperties.BLACK, PropertyType.COLOR,true);
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
	 *  These allow dynamic changes from the property editor.
	 *  NOTE: The id string of the reference block must be set before this method is called.
	 */
	private void initialize() {
		String referenceBlockId = getBlockId();
		if( referenceBlockId!=null && !referenceBlockId.isEmpty()) {
			BlockProperty h = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_HEIGHT);
			String key = NotificationKey.keyForProperty(referenceBlockId, h.getName());
			notificationHandler.addNotificationChangeListener(key,CLSS,this);
			setPreferredHeight(fncs.coerceToInteger(h.getValue().toString()));
			BlockProperty w = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_WIDTH);
			key = NotificationKey.keyForProperty(referenceBlockId, w.getName());
			notificationHandler.addNotificationChangeListener(key,CLSS,this);
			setPreferredWidth(fncs.coerceToInteger(w.getValue().toString()));
		}
	}
 	public String getBlockId() { 
 		return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_BLOCK_ID).getValue().toString(); 
 	}
	public void setBlockId(String id) { 
		BlockProperty bid = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_BLOCK_ID);
		bid.setValue(id);
	}
	// For these properties, do not worry about case insensitivity.
	// Fetching directory from the map is more efficient.
	public BlockProperty getProperty(String nam) {
		return propertyMap.get(nam);
	}
	
	// This is the property we are tracking
	public String getPropertyName()  {return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY).getValue().toString(); }
	public void setPropertyName(String name) {
		BlockProperty propName = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY);
		propName.setValue(name);
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
	// Return a string based on the value, property type and format.
	// The VALUE property is where the raw value is stored.
	public String getValue() {
		String format = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_FORMAT).getValue().toString();
		String value = getProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE).getValue().toString(); 
		if( binding!=null && !binding.isEmpty()) {
			value = readTag(binding);
		}
		if( value!=null && !value.isEmpty() ) {
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
	public void setValue(String val) { 
		getProperty(BlockConstants.ATTRIBUTE_PROPERTY_VALUE).setValue(val); 
	}
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
	
	@Override
	public void setName(String text) { 
		this.name = text;;
	}
	/**
	 * Create property change listeners for the AttributeDisplayBlock itself 
	 */
	@Override
	public void startup () {
		for(BlockProperty prop:getProperties()) {
			String key = NotificationKey.keyForProperty(getId().toString(), prop.getName());
			notificationHandler.initializePropertyValueNotification(key,prop.getValue());
			notificationHandler.addNotificationChangeListener(key,CLSS, this);
		}
	}
	
	/**
	 * Start listening to the value of the indicated property block. Both reference block and
	 * property name must be set prior to call. Listening on the name is aspecial case. 
	 * Also listen for block movement on self and reference block
	 */
	public void startListener() {
		String pname = getPropertyName();
		if( pname.endsWith(BlockConstants.BLOCK_PROPERTY_NAME)) {
			String key = NotificationKey.keyForBlockName(getBlockId());
			notificationHandler.addNotificationChangeListener(key,CLSS,this);
		}
		else {
			String key = NotificationKey.keyForProperty(getBlockId(), pname);
			notificationHandler.addNotificationChangeListener(key,CLSS,this);
			// If the property is bound to a tag, listen on that tag
			BlockProperty valueProp = reference.getProperty(getPropertyName());
			if(valueProp!=null && valueProp.getBindingType().equals(BindingType.TAG_MONITOR)) {
				binding = fncs.coerceToString(valueProp.getBinding()); 
				subscribeToTagPath(binding);
			}
		}
		addBlockListener(this);
	}
	
	private String readTag(String path) {
		String result = null;
		if( path==null || path.length()==0 ) return result;  // Fail silently for no binding
		ClientTagManager tmgr = BLTDesignerHook.getContext().getTagManager();
		try {
			TagPath tp = TagPathParser.parse(path);
			List<TagPath> paths = new ArrayList<>();
			paths.add(tp);
			CompletableFuture<List<QualifiedValue>> futures = tmgr.readAsync(paths);
			List<QualifiedValue> results = futures.get();
			result = results.get(0).getValue().toString();
		}
		catch(InterruptedException ie) {
			log.errorf("%s.readTag interrupted error for %s (%s)",CLSS,path,ie.getMessage());
		}
		catch(ExecutionException ee) {
			log.errorf("%s.readTag execution error for %s (%s)",CLSS,path,ee.getMessage());
		}
		catch(IOException ioe) {
			log.errorf("%s.readTag tag path parse error for %s (%s)",CLSS,path,ioe.getMessage());
		}
		return result;
	}
	
	// Subscribe to a tag. This will fail if the tag path is unset or illegal.
	// The provider has been set in the panel constructor.
	private void subscribeToTagPath(String path) {
		if( path==null || path.length()==0 ) return;  // Fail silently for path not set
		ClientTagManager tmgr = BLTDesignerHook.getContext().getTagManager();
		try {
			TagPath tp = TagPathParser.parse(path);
			tmgr.subscribeAsync(tp, this);
		}
		catch(IOException ioe) {
			log.errorf("%s.subscribeToTagPath tag path parse error for %s (%s)",CLSS,path,ioe.getMessage());
		}
	}
	private void unsubscribeToTagPath(String path) {
		if( path==null || path.length()==0 ) return;  // Fail silently for path not set
		ClientTagManager tmgr = BLTDesignerHook.getContext().getTagManager();
		try {
			TagPath tp = TagPathParser.parse(path);
			
			tmgr.unsubscribeAsync(tp, this);
		}
		catch(IOException ioe) {
			log.errorf("%s.unsubscribeToTagPath tag path parse error for %s (%s)",CLSS,path,ioe.getMessage());
		}
	}
	
	/**
	 * Remove tag subscriptions
	 */
	public void shutdown () {
		super.shutdown();
		removeBlockListener(this);
		unsubscribeToTagPath(binding);
	}

	// ======================================= Notification Change Listener ===================================
	// If the binding changes we need to change our tag listener.
	@Override
	public void bindingChange(String name,String tagPath) {
		unsubscribeToTagPath(binding);
		this.binding = tagPath;
		subscribeToTagPath(binding);
	}
	@Override
	public void diagramStateChange(String path, String state) {}
	// We never need to set our own name. We should be listening on the reference block.
	@Override
	public void nameChange(String bname) {
		if( getPropertyName().equals(BlockConstants.BLOCK_PROPERTY_NAME)) {
			setValue(bname);
		}
	}
	@Override
	public void propertyChange(String pname,Object value) {
		log.debugf("%s.propertyChange: - %s new value (%s)",CLSS,pname,value);
		if( pname.equalsIgnoreCase(getPropertyName())) {
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
		log.debugf("%s.stateChanged: - ",CLSS);
	}

	/**
	 * We are listening on a particular property, so the value change must be the right one.
	 */
	@Override
	public void valueChange(final QualifiedValue qv) {
		log.debugf("%s.valueChange: - %s new value (%s)",CLSS,getPropertyName(),qv.getValue().toString());
		setValue(qv.getValue().toString());
		SwingUtilities.invokeLater(new WorkspaceRepainter());
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
	
	// ======================================= Tag Change Listener ==================================

	@Override
	public void tagChanged(TagChangeEvent event) {
		if( event!=null && event.getValue()!=null && event.getValue().getValue()!=null ) {
			String value = event.getValue().getValue().toString();
			log.debugf("%s.tagChanged: - %s new value (%s)",CLSS,event.getTagPath().toStringFull(),event.getValue().toString());
			setValue(value);
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
		else {
			// Tag or path is null
			log.warnf("%s.tagChanged: Received an empty value",CLSS);
		}		
	}
}

