package com.ils.blt.designer.workspace;

import java.awt.Color;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;

import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.workspace.ui.AbstractBlockUIView;
import com.ils.blt.designer.workspace.ui.UIFactory;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.AbstractBlock;

/**
 * This is the class that describes each block as it appears in a
 * diagram in the Designer. Different block shapes and characteristics
 * are provided by swapping out different UI rendering classes.
 * 
 * Note: The "dirtiness" of the block is used simply for darkening
 *       the icon on the screen.
 * Note: We are added as a property change listener by the ProcessDiagram 
 *       whenever the diagram is displayed. On notification, we just repaint
 *       the UI.
 * Note: initUI is called from the AbstractBlock constructor which is called
 *       when the diagram is opened.
 */
public class ProcessBlockView extends AbstractBlock implements ChangeListener,Cloneable, NotificationChangeListener {
	private static final String CLSS = "ProcessBlockView";
	private static final boolean DEBUG = false;
	private final static Random random = new Random();
	private Map<String,ProcessAnchorDescriptor> anchors;
	private final EventListenerList listenerList;
	private final NotificationHandler handler = NotificationHandler.getInstance();
	private GeneralPurposeDataContainer auxiliaryData = new GeneralPurposeDataContainer();
	private final ChangeEvent changeEvent;
	private int background = Color.white.getRGB();
	private String className;
	private boolean dirty = false;   // A newly created block is clean because we initially sync with the gateway
	private String editorClass = null; // Class name of custom editor for this block
	private int    embeddedFontSize = WorkspaceConstants.DEFAULT_EMBEDDED_FONT_SIZE; // Size of font for interior label
	private String embeddedIcon="";               // 32x32 icon to place in block in designer
	private String embeddedLabel="";              // Label place in block in designer
	private final UIFactory factory = new UIFactory() ;
	private String iconPath="";                   // Path to icon that is the entire block
	private boolean ctypeEditable=false;          // Can we globally change our connection types
	private boolean locked = false; 
	private Point location = new Point(0,0);
	protected final LoggerEx log; 
	protected String name;
	private int preferredHeight = 0;              // Size the view to "natural" size
	private int preferredWidth  = 0;              // Size the view to "natural" size
	private String backgroundColor  = "GREY";
	protected Map<String,BlockProperty> propertyMap = new HashMap<String,BlockProperty>();
	private TruthValue state = TruthValue.UNSET;
	private String badgeChar = null;
	private String statusText;                    // Auxiliary text to display
	private UUID subworkspaceId = null;           // Encapsulated diagram if encapsulation block
	private BlockStyle style = BlockStyle.SQUARE;
	private AbstractBlockUIView ui = null;
	private UUID uuid = null;
	
	/**
	 * Constructor: Used when a new block is created from the palette. 
	 *              Create a pseudo-random name.
	 */
	public ProcessBlockView(BlockDescriptor descriptor) {
		this.listenerList = new EventListenerList();
		this.changeEvent  = new ChangeEvent(this);
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.uuid = UUID.randomUUID();
		this.background = descriptor.getBackground();
		this.className = descriptor.getBlockClass();
		this.ctypeEditable = descriptor.isCtypeEditable();
		this.editorClass = descriptor.getEditorClass();
		this.embeddedIcon = descriptor.getEmbeddedIcon();
		this.embeddedLabel= descriptor.getEmbeddedLabel();
		this.embeddedFontSize= descriptor.getEmbeddedFontSize();
		this.iconPath = descriptor.getIconPath();
		this.preferredHeight = descriptor.getPreferredHeight();
		this.preferredWidth = descriptor.getPreferredWidth();
		this.state = TruthValue.UNSET;
		this.statusText = "";
		this.style = descriptor.getStyle();
		this.badgeChar      = descriptor.getBadgeChar();
		this.anchors = new HashMap<>();
		int order = 0;
		for( AnchorPrototype ap:descriptor.getAnchors() ) {
			log.debugf("%s: Creating anchor descriptor %s", CLSS,ap.getName());
			ap.setSortOrder(order);  // let's preserve the order these were in.
			ProcessAnchorDescriptor pad = new ProcessAnchorDescriptor((ap.getAnchorDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
					ap.getConnectionType(),UUID.randomUUID(),ap.getName(),ap.getAnnotation(),ap.getHint(),ap.isMultiple(), ap.getSortOrder());
			pad.setHidden(ap.isHidden());
			order++;
			anchors.put(ap.getName(), pad);
		}
		if(DEBUG)log.infof("%s: Created %s (%s) view from descriptor (%d anchors)", CLSS, className, style.toString(),anchors.size());
		createPseudoRandomName();
	}
	
	/**
	 * Used by the clone() method
	 */
	public ProcessBlockView() {
		this.listenerList = new EventListenerList();
		this.changeEvent  = new ChangeEvent(this);
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.uuid = UUID.randomUUID();
	}
	
	public ProcessBlockView(SerializableBlock sb) {
		this.listenerList = new EventListenerList();
		this.changeEvent  = new ChangeEvent(this);
		this.uuid = sb.getId();
		this.auxiliaryData = sb.getAuxiliaryData();
		this.background = sb.getBackground();
		this.className = sb.getClassName();
		this.ctypeEditable  = false;
		this.dirty = sb.isDirty();
		this.editorClass = sb.getEditorClass();
		this.embeddedIcon = sb.getEmbeddedIcon();
		this.embeddedLabel= sb.getEmbeddedLabel();
		this.embeddedFontSize = sb.getEmbeddedFontSize();
		this.iconPath = sb.getIconPath();
		this.locked   = sb.isLocked();
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.preferredHeight = sb.getPreferredHeight();
		this.preferredWidth = sb.getPreferredWidth();
		this.style = sb.getStyle();
		this.state = sb.getState();
		this.statusText = sb.getStatusText();
		this.badgeChar      = sb.getBadgeChar();
		this.anchors = new HashMap<>();
		if(sb.getAnchors()!=null ) {
			for( SerializableAnchor sa:sb.getAnchors() ) {
				log.debugf("%s: %s creating serializable anchor %s (%s)", CLSS,sb.getName(),sa.getDisplay(),sa.getConnectionType().name());
				ProcessAnchorDescriptor pad = new ProcessAnchorDescriptor((sa.getDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
						sa.getConnectionType(),sa.getId(),sa.getDisplay(),sa.getAnnotation(),sa.getHint(),sa.isMultiple(), sa.getSortOrder());
				pad.setHidden(sa.isHidden());
				anchors.put(sa.getDisplay(),pad);
			}
		}
		if(sb.getProperties()!=null ) {
			for(BlockProperty bp:sb.getProperties()) {
				if( bp!=null && bp.getName()!=null ) {
					log.debugf("%s: %s creating property %s", CLSS,sb.getName(),bp.getName());
					propertyMap.put(bp.getName(),bp);
				}
			} 
		}
		this.name = sb.getName();
		this.location = new Point(sb.getX(),sb.getY());
		if(DEBUG) log.infof("%s: %s created %s %s (%s) view from serializable block", CLSS, sb.getName(),className, sb.getId().toString(),style.toString());
	}
	
	// NOTE: The most recent values on each port are stored in the connection anchor point
	public SerializableAnchor convertAnchorToSerializable(ProcessAnchorDescriptor anchor) {
		SerializableAnchor result = new SerializableAnchor();
		result.setDirection(anchor.getType()==AnchorType.Origin?AnchorDirection.OUTGOING:AnchorDirection.INCOMING);
		result.setDisplay(anchor.getDisplay());
		result.setId(anchor.getId());
		result.setParentId(getId());
		result.setConnectionType(anchor.getConnectionType());
		result.setAnnotation(anchor.getAnnotation());
		result.setHidden(anchor.isHidden());
		result.setHint(anchor.getHint());
		result.setMultiple(anchor.isMultiple());
		result.setSortOrder(anchor.getSortOrder());
		return result;
	}
	
    public SerializableBlock convertToSerializable() {
		SerializableBlock result = new SerializableBlock();
		result.setId(getId());
		result.setAuxiliaryData(getAuxiliaryData());
		result.setBackground(getBackground());
		result.setClassName(getClassName());
		result.setEditorClass(getEditorClass());
		result.setEmbeddedIcon(getEmbeddedIcon());
		result.setEmbeddedLabel(getEmbeddedLabel());
		result.setEmbeddedFontSize(getEmbeddedFontSize());
		result.setIconPath(getIconPath());
		result.setLocked(isLocked());
		result.setName(getName());
		result.setPreferredHeight(getPreferredHeight());
		result.setPreferredWidth(getPreferredWidth());
		result.setState(getState());
		result.setStatusText(getStatusText());
		result.setStyle(getStyle());
		result.setBadgeChar(getBadgeChar());
		result.setX(getLocation().x);
		result.setY(getLocation().y);
		
		List<SerializableAnchor> ancs = new ArrayList<>();
		for( AnchorDescriptor anchor:getAnchors()) {
			ancs.add(convertAnchorToSerializable((ProcessAnchorDescriptor)anchor));
		}
		result.setAnchors(ancs.toArray(new SerializableAnchor[ancs.size()]));
		if( getProperties()!=null ) {
			log.tracef("%s.convertToSerializable: %s has %d properties",CLSS,getClassName(),getProperties().size());
			//log.trace(getProperties().toString());
			result.setProperties(getProperties().toArray(new BlockProperty[getProperties().size()]));
		}
		else {
			log.warnf("%s.convertToSerializable: %s has no properties",CLSS,getClassName());
		}
		
		return result;
	}
    public void addAnchor(AnchorPrototype ap) {
		ProcessAnchorDescriptor pad = new ProcessAnchorDescriptor((ap.getAnchorDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
				ap.getConnectionType(),UUID.randomUUID(),ap.getName(),ap.getAnnotation(),ap.getHint(),ap.isMultiple(), ap.getSortOrder());
		pad.setHidden(ap.isHidden());
		anchors.put(ap.getName(), pad);
    }
    /**
     * Change the connection type of all anchors to a new specified
     * type. This has an effect only if ctypeEditable is true. This
     * flag is always negative on restore from serialization. Additionally
     * the first signal input is not disturbed. Additionally control lines and 
     * broadcast ports are not changed. In the case where the
	 * type is ANY or TEXT, the method will look downstream and match any current
	 * connections.
     * @param newType
     */
    public void changeConnectorType(ConnectionType newType) {
    	if(ctypeEditable) {
    		boolean changed = false;
    		for( ProcessAnchorDescriptor anchor:getAnchors()) {
    			if( anchor.getConnectionType().equals(ConnectionType.SIGNAL)) {
    				continue;  // Signal port is not changeable
    			}
    			else if(anchor.getDisplay().equals(BlockConstants.RECEIVER_PORT_NAME) ||
    					anchor.getDisplay().equals(BlockConstants.BROADCAST_PORT_NAME) ) {
    				continue;
    			}
    			else if( !anchor.getConnectionType().equals(newType)){
    				changed = true;
    				anchor.setConnectionType(newType);
    			}
    		}
    		if( changed ) {
    			getUi().reconfigure();
    			fireStateChanged();
    		}
    	}
    }

	/*		This doesn't make a lot of sense.  Why does ignition pass
	 *      in a list of blocks selected when this only returns a single
	 *      block?  Very odd.  Not sure what to do with the rest of them
	 */
	@Override
	public Block copy(Map<UUID, UUID> arg0) {
		ProcessBlockView newBlock = null;
		for (UUID me:arg0.values()) {  
			newBlock = new ProcessBlockView(this.convertToSerializable());
			newBlock.uuid = me;
		}
		return newBlock;
	}
	
	@Override
	public Collection<AnchorPoint> getAnchorPoints() {
		if( getUi()==null ) {
			ui = factory.getUI(style, this);
		}
		return getUi().getAnchorPoints();	
	}
	
	public Collection<ProcessAnchorDescriptor> getAnchors() { 
		return anchors.values(); 
	}
	
	public GeneralPurposeDataContainer getAuxiliaryData() {
		return auxiliaryData;
	}
	public void fireBlockMoved() {
		super.fireBlockMoved();
	}
	
	public int getBackground() { 
		return background;
	}
	
	// Remove xom from th class name
	public synchronized String getClassName() { 
		String ret = new String(className);
		if(ret.toLowerCase().startsWith("xom.block.")) {
			ret = "ils." + ret.substring(4);
		}
		return ret; 
	}

	/** 
	 * Define a default drop target based on the connector's anchor point 
	 * hovering above us. For simplicity we just return the first anchor point
	 * of the right sex that is legal. If none are legal, we use the last
	 * anchor found.
	 * @param opposite
	 */
	@Override
	public AnchorPoint getDefaultDropAnchor(AnchorPoint opposite ) {
		AnchorPoint result = null;
		for(AnchorPoint ap:getAnchorPoints()) {
			if( isConnectionValid(ap,opposite)) {
				result = ap;
				break;
			}
		}
		return result;
	}
	
	public String getEditorClass() {return editorClass;}
	public int getEmbeddedFontSize() {return embeddedFontSize;}
	public String getEmbeddedIcon() {return embeddedIcon;}
	public String getEmbeddedLabel() {return embeddedLabel;}
	public String getIconPath() {return iconPath;}
	@Override
	public UUID getId() { return uuid; }
	public QualifiedValue getLastValueForPort(String port) {
		QualifiedValue qv = null;
		ProcessAnchorDescriptor pad = anchors.get(port);
		if( pad!=null ) {
			qv = pad.getLastValue();
		}
		else {
			log.warnf("%s.getLastValueForPort: %s, unknown port specified (%s)",CLSS,getName(),port);
		}
		return qv;
	}
	// Location is the upper left.
	@Override
	public Point getLocation() {
		return location;
	}
	// Simply do a linear search - this allows a case-insensitive name match
	public BlockProperty getProperty(String nam) {
		BlockProperty result = null;
		for( BlockProperty prop:getProperties()) {
			if( prop.getName().equalsIgnoreCase(nam)) {
				result = prop;
				break;
			}
		}
		return result;
	}
	public String getName() { return this.name; }
	public int getPreferredHeight() {return preferredHeight;}
	public int getPreferredWidth() {return preferredWidth;}
	public String getBackgroundColor() {return backgroundColor;}
	public Collection<BlockProperty> getProperties() { return propertyMap.values(); }
	public TruthValue getState() {return state;}
	public String getStatusText() { return statusText; }
	public BlockStyle getStyle() { return style; }
	public String getBadgeChar() { return badgeChar; }
	public UUID getSubworkspaceId() {return subworkspaceId;}
	
	/** 
	 * Get the current block property values from the Gateway. 
	 * If the block does not have a Gateway counterpart (e.g. diagram is dirty), we'll get the 
	 * default property list for the block class. 
	 * 
	 * IMPORTANT: Always do this before block is displayed.
	 * @param parent resource id of the parent diagram
	 */
	public void initProperties(ProjectResourceId parent) {
		ApplicationRequestHandler requestHandler = new ApplicationRequestHandler();
		List<BlockProperty> properties = requestHandler.getBlockProperties(getClassName(),parent,getId());
			for(BlockProperty bp:properties) {
				setProperty(bp);
			}
			log.tracef("%s.initProperties - initialize property list for %s (%d properties)",CLSS,getId().toString(),properties.size());
	}
	
	// This is called in the block-and-connector framework
	// for each block as the diagram is opened.
	@Override
	public void initUI(BlockComponent blk) {
		ui = factory.getUI(style, this);
		getUi().install(blk);
	}
	public boolean isCtypeEditable() {return ctypeEditable;}
	public boolean isDirty() {return dirty;}
	public boolean isLocked() {return locked;}
	public boolean isSignalAnchorDisplayed() {
		for(ProcessAnchorDescriptor pad:anchors.values()) {
			if(pad.getDisplay().equals(BlockConstants.SIGNAL_PORT_NAME)) {
				return !pad.isHidden();
			}
		}
		return false;
	}
	public void recordLatestValue(String port,QualifiedValue qv) {
		if( qv==null || qv.getValue()==null) return;
		log.tracef("%s.recordLatestValue: %s (%s) port %s (%s)",CLSS,getName(),getId().toString(),port,qv.getValue().toString());
		ProcessAnchorDescriptor pad = anchors.get(port);
		if( pad!=null ) {
			pad.setLastValue(qv);
		}
		else {
			log.warnf("%s.recordLatestValue: Uknown port (%s)",CLSS,port);
		}
	}
	public void setAuxiliaryData(GeneralPurposeDataContainer auxiliaryData) {this.auxiliaryData = auxiliaryData;}
	public void setCtypeEditable(boolean flag) {this.ctypeEditable = flag;}
	public void setDirty(boolean dirty) {this.dirty = dirty;} 
	public void setEditorClass(String editorClass) {this.editorClass = editorClass;}
	public void setEmbeddedFontSize(int size) {this.embeddedFontSize = size;}
	public void setEmbeddedIcon(String embeddedIcon) {this.embeddedIcon = embeddedIcon;}
	public void setEmbeddedLabel(String embeddedLabel) {this.embeddedLabel = embeddedLabel;}
	public void setIconPath(String iconPath) {this.iconPath = iconPath;}
	public void setLocked(boolean flag) {this.locked = flag;}
	public void setName(String text) { 
		if( name==null ||  !name.equals(text) ) {
			this.name = text;
			String key = NotificationKey.keyForBlockName(getId().toString());
			handler.initializeBlockNameNotification(key, name);
			fireStateChanged();
		}
	}

	@Override
	public void setLocation(Point loc) {
		location = loc;
		fireBlockMoved();
	}
	public void setPreferredHeight(int preferredHeight) {this.preferredHeight = preferredHeight;}
	public void setPreferredWidth(int preferredWidth) {this.preferredWidth = preferredWidth;}
	// Do not allow a Name property. Use the class member instead.
	public void setProperty(BlockProperty prop) { 
		if( prop!=null && prop.getName()!=null ) {
			if(prop.getName().equals(BlockConstants.BLOCK_PROPERTY_NAME)) {
				setName(prop.getValue().toString());
			}
			else {
				propertyMap.put(prop.getName(), prop);
				String key = NotificationKey.keyForProperty(getId().toString(),prop.getName());
				handler.initializePropertyValueNotification(key, prop.getValue());
				fireStateChanged();
			}
		}
		else {
			log.warnf("%s.setProperties: WARNING: attempt to set %s properties to null",CLSS,getName());
		}
	}
	public void setBackground(int b)  { this.background = b; }
	// Find the generic signal anchor and set its "hidden" property
	public void setSignalAnchorDisplayed(boolean flag) {
		for(ProcessAnchorDescriptor pad:anchors.values()) {
			if(pad.getDisplay().equals(BlockConstants.SIGNAL_PORT_NAME)) {
				pad.setHidden(!flag);
				break;
			}
		}
	}
	public void setState(TruthValue state) {if(state!=null) this.state = state;}
	public void setStatusText(String statusText) { this.statusText = statusText; }
	public void setStyle(BlockStyle s) { if( style!=null )this.style = s; }
	public void setSubworkspaceId(UUID subworkspaceId) {this.subworkspaceId = subworkspaceId;}
	
	/**
	 * Remove notification subscriptions
	 */
	public void shutdown () {
		for(BlockProperty prop:getProperties()) {
			String key = NotificationKey.keyForProperty(getId().toString(), prop.getName());
			handler.removeNotificationChangeListener(key,CLSS);
		}
	}
	/**
	 * Create a notification subscription for each property and the name
	 */
	public void startup () {
		for(BlockProperty prop:getProperties()) {
			String key = NotificationKey.keyForProperty(getId().toString(), prop.getName());
			handler.initializePropertyValueNotification(key,prop.getValue());
			if(DEBUG) log.infof("%s.startup: listening %s", CLSS, key);
			handler.addNotificationChangeListener(key,CLSS, this);
		}
		String key = NotificationKey.keyForBlockName(getId().toString());
		handler.addNotificationChangeListener(key,CLSS, this);
	}
	/**
	 * Create a name that is highly likely to be unique within the diagram.
	 * The name can be user-modified at any time. If we really need a uniqueness,
	 * use the block's UUID. Set the member directly to avoid excess notifications.
	 */
	public void createPseudoRandomName() {
		String root = className;
		int pos = className.lastIndexOf(".");
		if( pos>=0 )  root = className.substring(pos+1);
		this.name = String.format("%s-%d", root.toUpperCase(),random.nextInt(1000));
	}
	
	/**
	 * Create a name that is highly likely to be unique within the diagram.
	 * The name can be user-modified at any time. If we really need a uniqueness,
	 * use the block's UUID.
	 */
	public void createPseudoRandomNameExtension() {
		this.name = String.format("%s-%d",getName(),random.nextInt(1000));
	}
	
	/**
	 * Create a name that is highly likely to be unique within the diagram.
	 * The name can be user-modified at any time. If we really need a uniqueness,
	 * use the block's UUID.
	 */
	public void createRandomId() {
		this.uuid = UUID.randomUUID();
	}
	
	// =================== Handle Event Listeners ===================
	public void addChangeListener(ChangeListener l) {
	     listenerList.add(ChangeListener.class, l);
	 }

	 public void removeChangeListener(ChangeListener l) {
	     listenerList.remove(ChangeListener.class, l);
	 }


	 // Notify all listeners that have registered interest for
	 // notification on this event type.  The event instance
	 // is lazily created using the parameters passed into
	 // the fire method.
	 protected void fireStateChanged() {
		 if(DEBUG) log.infof("%s.fireStateChanged: %s", CLSS, getName());
		 // Guaranteed to return a non-null array
		 Object[] listnrs = listenerList.getListenerList();
		 // Process the listeners last to first, notifying
		 // those that are interested in this event
		 for (int i = listnrs.length-2; i>=0; i-=2) {
			 if (listnrs[i]==ChangeListener.class) {
				 ((ChangeListener)listnrs[i+1]).stateChanged(changeEvent);
			 }
		 }
	 }
	 
	 // Specify whether or not a drop point is valid.
	 // Reasons for invalid:
	 // 1) end anchor is already connected and max connections = 1
	 // 2) direction mismatch
	 // 3) data type mismatch
	 public static boolean isConnectionValid(AnchorPoint startingAnchor, AnchorPoint endAnchor) {
		 boolean result = true;
		 BasicAnchorPoint start = (BasicAnchorPoint)startingAnchor;
		 BasicAnchorPoint end   = (BasicAnchorPoint)endAnchor;
		 if( end.getBlock()!=null && !end.allowMultipleConnections() ) {result = false;}
		 else if( start.isConnectorOrigin()==end.isConnectorOrigin())  result = false;
		 else {
			 result = false;
			 if(start.getConnectionType().equals(end.getConnectionType()) ||
		     	start.getConnectionType().equals(ConnectionType.ANY)      ||
				end.getConnectionType().equals(ConnectionType.ANY)   ) {

				 result = true;
			 }
		 }
		 return result;
	 }

	// ====================================== Change Listener =====================================
	 /**
	  * We are about to take UI action, so execute on the event thread.
	  * This is probably in response to a change in one of the block properties or name.
	  */
	@Override
	public void stateChanged(ChangeEvent e) {
		if( getUi() != null ) {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					getUi().update();
				}
			});
		}
		
	}

	/**
	 *  Change the block anchor types to match the tag.
	 *  Tags of type String, result in no change to the block.
	 */
	public void modifyConnectionForTagChange(BlockProperty property, DataType type) {	
		String binding = property.getBinding();
		if (binding != null) {
			ConnectionType conType= determineConnectionTypeFromTagType(type);
			if( !conType.equals(ConnectionType.TEXT)) {
				changeConnectorType(conType);
			}
		}
	}

	public ConnectionType determineConnectionTypeFromTagType(DataType type) {
		ConnectionType conType = ConnectionType.ANY;
		if (type == DataType.Int1 || 
			type == DataType.Int2 ||
			type == DataType.Int4 ||
			type == DataType.Int8 ||
			type == DataType.Float4 ||
			type == DataType.Float8 ) {
			conType = ConnectionType.DATA; 
		} 
		else  if (type == DataType.String || 
			type == DataType.Text ) {
			conType = ConnectionType.TEXT; 
		} else  if (type == DataType.Boolean) { 
			conType = ConnectionType.TRUTHVALUE; 
		}
		return conType;
	}

	public AbstractBlockUIView getUi() {
		return ui;
	}

	public boolean isDiagnosis() {
		boolean ret = false;
		if (getClassName().toLowerCase().contains("finaldiagnosis") ||
			getClassName().toLowerCase().contains("sqcdiagnosis")) {
			ret = true;
		}
		return ret;
	}

	// =================================== NotificationChangeListener =====================
	@Override
	public void diagramStateChange(String path, String s) {	
	}

	@Override
	public void bindingChange(String pname,String binding) {
	}
	
	@Override
	public void nameChange(String nm) {
		setName(nm);
	}
	@Override
	public void propertyChange(String pname,Object value) {
		BlockProperty prop = getProperty(pname);
		if( prop!=null ) prop.setValue(value);
		if(DEBUG)log.infof("%s.propertyChange: %s %s=%s", CLSS,getName(),prop.getName(),value.toString());
	}
	@Override
	public void valueChange(QualifiedValue value) {	
	}

	@Override
	public void watermarkChange(String newWatermark) {
		// TODO Auto-generated method stub
		
	}
	// ================================ Cloneable =====================================
		@Override
		public ProcessBlockView clone() {
			ProcessBlockView clone = new ProcessBlockView();
			clone.background 	= getBackground();
			clone.className 	= className;
			clone.ctypeEditable = isCtypeEditable();
			clone.editorClass 	= getEditorClass();
			clone.embeddedIcon 	= getEmbeddedIcon();
			clone.embeddedLabel	= getEmbeddedLabel();
			clone.embeddedFontSize	= getEmbeddedFontSize();
			clone.iconPath 			= getIconPath();
			clone.location.x		= getLocation().x;
			clone.location.y		= getLocation().y;
			clone.preferredHeight	= getPreferredHeight();
			clone.preferredWidth 	= getPreferredWidth();
			clone.state 		= getState();
			clone.statusText 	= "";
			clone.style 		= getStyle();
			clone.badgeChar      = getBadgeChar();
			clone.anchors = new HashMap<>();
			for(String key:anchors.keySet()) {
				ProcessAnchorDescriptor pad = anchors.get(key);
				clone.anchors.put(key, pad.clone());
			}
			clone.propertyMap = new HashMap<>();
			for(String key:propertyMap.keySet()) {
				BlockProperty bp = propertyMap.get(key);
				clone.propertyMap.put(key,bp.clone());
			}
			clone.auxiliaryData= getAuxiliaryData().clone();
			return clone;
		}
}

