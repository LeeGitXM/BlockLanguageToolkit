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

import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.designer.workspace.ui.AbstractUIView;
import com.ils.blt.designer.workspace.ui.UIFactory;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
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
public class ProcessBlockView extends AbstractBlock implements ChangeListener, NotificationChangeListener {
	private static final String TAG = "ProcessBlockView";
	private final static Random random = new Random();
	private Map<String,ProcessAnchorDescriptor> anchors;
	private final EventListenerList listenerList;
	private GeneralPurposeDataContainer auxiliaryData = new GeneralPurposeDataContainer();
	private final ChangeEvent changeEvent;
	private int background = Color.white.getRGB();
	private String className;
	private boolean dirty = false;   // A newly created block is clean because we initially sync with the gateway
	private String editorClass = null; // Class name of custom editor for this block
	private boolean encapsulation = false; // Is this an encapsulation block
	private int    embeddedFontSize = WorkspaceConstants.DEFAULT_EMBEDDED_FONT_SIZE; // Size of font for interior label
	private String embeddedIcon="";               // 32x32 icon to place in block in designer
	private String embeddedLabel="";              // Label place in block in designer
	private final UIFactory factory = new UIFactory() ;
	private String iconPath="";                   // Path to icon that is the entire block
	private boolean ctypeEditable=false;          // Can we globally change our connection types
	private boolean locked = false; 
	private Point location = new Point(0,0);
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private int preferredHeight = 0;              // Size the view to "natural" size
	private int preferredWidth  = 0;              // Size the view to "natural" size
	private String backgroundColor  = "GREY";
	private Collection<BlockProperty> properties;
	private TruthValue state = TruthValue.UNSET;
	private String badgeChar = null;
	private String statusText;                    // Auxiliary text to display
	private UUID subworkspaceId = null;           // Encapsulated diagram if encapsulation block
	private BlockStyle style = BlockStyle.SQUARE;
	private AbstractUIView ui = null;
	private UUID uuid = null;
	
	/**
	 * Constructor: Used when a new block is created from the palette. 
	 *              Create a pseudo-random name.
	 */
	public ProcessBlockView(BlockDescriptor descriptor) {
		this.listenerList = new EventListenerList();
		this.changeEvent  = new ChangeEvent(this);
		this.uuid = UUID.randomUUID();
		this.background = descriptor.getBackground();
		this.className = descriptor.getBlockClass();
		this.ctypeEditable = descriptor.isCtypeEditable();
		this.editorClass = descriptor.getEditorClass();
		this.encapsulation = descriptor.isEncapsulation();
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
			log.debugf("%s: Creating anchor descriptor %s", TAG,ap.getName());
			ap.setSortOrder(order);  // let's preserve the order these were in.
			ProcessAnchorDescriptor pad = new ProcessAnchorDescriptor((ap.getAnchorDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
					ap.getConnectionType(),UUID.randomUUID(),ap.getName(),ap.getAnnotation(),ap.getHint(),ap.isMultiple(), ap.getSortOrder());
			pad.setHidden(ap.isHidden());
			order++;
			anchors.put(ap.getName(), pad);
		}
		this.properties = new ArrayList<BlockProperty>();
		log.debugf("%s: Created %s (%s) view from descriptor (%d anchors)", TAG, className, style.toString(),anchors.size());
		createPseudoRandomName();
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
		this.encapsulation = (sb.getSubworkspaceId()!=null);
		this.iconPath = sb.getIconPath();
		this.locked   = sb.isLocked();
		this.preferredHeight = sb.getPreferredHeight();
		this.preferredWidth = sb.getPreferredWidth();
		this.style = sb.getStyle();
		this.state = sb.getState();
		this.statusText = sb.getStatusText();
		this.badgeChar      = sb.getBadgeChar();
		this.subworkspaceId = sb.getSubworkspaceId();
		this.anchors = new HashMap<>();
		if(sb.getAnchors()!=null ) {
			for( SerializableAnchor sa:sb.getAnchors() ) {
				log.debugf("%s: %s creating serializable anchor %s (%s)", TAG,sb.getName(),sa.getDisplay(),sa.getConnectionType().name());
				ProcessAnchorDescriptor pad = new ProcessAnchorDescriptor((sa.getDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
						sa.getConnectionType(),sa.getId(),sa.getDisplay(),sa.getAnnotation(),sa.getHint(),sa.isMultiple(), sa.getSortOrder());
				pad.setHidden(sa.isHidden());
				anchors.put(sa.getDisplay(),pad);
			}
		}
		this.properties = new ArrayList<BlockProperty>();
		if(sb.getProperties()!=null ) {
			for(BlockProperty bp:sb.getProperties()) {
				if(bp==null) continue;
				log.debugf("%s: %s creating property %s", TAG,sb.getName(),bp.getName());
				properties.add(bp);
			} 
		}
		// this.setName(sb.getName()); // There should already be a name property
		this.location = new Point(sb.getX(),sb.getY());
		log.debugf("%s: %s created %s %s (%s) view from serializable block", TAG, sb.getName(),className, sb.getId().toString(),style.toString());
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
		result.setSubworkspaceId(subworkspaceId);
		result.setBadgeChar(getBadgeChar());
		result.setX(getLocation().x);
		result.setY(getLocation().y);
		
		List<SerializableAnchor> ancs = new ArrayList<>();
		for( AnchorDescriptor anchor:getAnchors()) {
			ancs.add(convertAnchorToSerializable((ProcessAnchorDescriptor)anchor));
		}
		result.setAnchors(ancs.toArray(new SerializableAnchor[ancs.size()]));
		if( getProperties()!=null ) {
			log.tracef("%s.convertToSerializable: %s has %d properties",TAG,getClassName(),getProperties().size());
			//log.trace(getProperties().toString());
			result.setProperties(getProperties().toArray(new BlockProperty[getProperties().size()]));
		}
		else {
			log.warnf("%s.convertToSerializable: %s has no properties",TAG,getClassName());
		}
		
		return result;
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
	
	public int getBackground() { 
		return background;
	}
	
	public synchronized String getClassName() { 
//		removeXomFromBlockName();
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
			log.warnf("%s.getLastValueForPort: %s, unknown port specified (%s)",TAG,getName(),port);
		}
		return qv;
	}
	// Location is the upper left.
	@Override
	public Point getLocation() {
		return location;
	}
	// Simply do a linear search
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
	public String getName() {
		BlockProperty nameProperty = getProperty(BlockConstants.BLOCK_PROPERTY_NAME);
		if(nameProperty==null ) {
			nameProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_NAME,"",PropertyType.STRING,true);
				properties.add(nameProperty);
		}
		return nameProperty.getValue().toString();
	}
	public int getPreferredHeight() {return preferredHeight;}
	public int getPreferredWidth() {return preferredWidth;}
	public String getBackgroundColor() {return backgroundColor;}
	public Collection<BlockProperty> getProperties() { return properties; }
	public TruthValue getState() {return state;}
	public String getStatusText() { return statusText; }
	public BlockStyle getStyle() { return style; }
	public String getBadgeChar() { return badgeChar; }
	public UUID getSubworkspaceId() {return subworkspaceId;}
	
	@Override
	public void initUI(BlockComponent blk) {
		ui = factory.getUI(style, this);
		getUi().install(blk);
	}
	public boolean isCtypeEditable() {return ctypeEditable;}
	public boolean isDirty() {return dirty;}
	public boolean isEncapsulation() {return encapsulation;}
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
		log.tracef("%s.recordLatestValue: %s (%s) port %s (%s)",TAG,getName(),getId().toString(),port,qv.getValue().toString());
		ProcessAnchorDescriptor pad = anchors.get(port);
		if( pad!=null ) {
			pad.setLastValue(qv);
		}
		else {
			log.warnf("%s.recordLatestValue: Uknown port (%s)",TAG,port);
		}
	}
	public void setAuxiliaryData(GeneralPurposeDataContainer auxiliaryData) {this.auxiliaryData = auxiliaryData;}
	public void setCtypeEditable(boolean flag) {this.ctypeEditable = flag;}
	public void setDirty(boolean dirty) {this.dirty = dirty;} 
	public void setEditorClass(String editorClass) {this.editorClass = editorClass;}
	public void setEncapsulation(boolean encapsulation) {this.encapsulation = encapsulation;}
	public void setEmbeddedFontSize(int size) {this.embeddedFontSize = size;}
	public void setEmbeddedIcon(String embeddedIcon) {this.embeddedIcon = embeddedIcon;}
	public void setEmbeddedLabel(String embeddedLabel) {this.embeddedLabel = embeddedLabel;}
	public void setIconPath(String iconPath) {this.iconPath = iconPath;}
	public void setLocked(boolean flag) {this.locked = flag;}
	public void setName(String name) {
		BlockProperty nameProperty = getProperty(BlockConstants.BLOCK_PROPERTY_NAME);
		if(nameProperty==null ) {
			nameProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_NAME,name,PropertyType.STRING,true);
			properties.add(nameProperty);
		}
		nameProperty.setValue(name);   // Notifies change listeners
		fireStateChanged(); 
	}
	@Override
	public void setLocation(Point loc) {
		location = loc;
		fireBlockMoved();
	}
	public void setPreferredHeight(int preferredHeight) {this.preferredHeight = preferredHeight;}
	public void setPreferredWidth(int preferredWidth) {this.preferredWidth = preferredWidth;}
	public void setProperties(Collection<BlockProperty> props) { 
		if( props!=null ) {
			this.properties = props; 
		}
		else {
			log.warnf("%s.setProperties: WARNING: attempt to set %s properties to null",TAG,getName());
		}
	}
//	public void setReceiveEnabled(boolean receiveEnabled) {this.receiveEnabled = receiveEnabled;}
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
	 * Create a name that is highly likely to be unique within the diagram.
	 * The name can be user-modified at any time. If we really need a uniqueness,
	 * use the block's UUID.
	 */
	public void createPseudoRandomName() {
		String root = className;
		int pos = className.lastIndexOf(".");
		if( pos>=0 )  root = className.substring(pos+1);
		setName(String.format("%s-%d", root.toUpperCase(),random.nextInt(1000)));
	}
	
	/**
	 * Create a name that is highly likely to be unique within the diagram.
	 * The name can be user-modified at any time. If we really need a uniqueness,
	 * use the block's UUID.
	 */
	public void createPseudoRandomNameExtension() {
		setName(String.format("%s-%d",getName(),random.nextInt(1000)));
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
	  * This is probably in response to a change in one of the block properties.
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
			ConnectionType conType= determineDataTypeFromTagType(type);
			if( !conType.equals(ConnectionType.TEXT)) {
				changeConnectorType(conType);
			}
		}
		
	}

	public ConnectionType determineDataTypeFromTagType(DataType type) {
		ConnectionType conType = ConnectionType.ANY;
		if (type == DataType.Int1 || 
			type == DataType.Int2 ||
			type == DataType.Int4 ||
			type == DataType.Int8 ||
			type == DataType.Float4 ||
			type == DataType.Float8 ) {
			conType = ConnectionType.DATA; 
		} else  if (type == DataType.String || 
			type == DataType.Text ) {
			conType = ConnectionType.TEXT; 
		} else  if (type == DataType.Boolean) { 
			conType = ConnectionType.TRUTHVALUE; 
		}
		return conType;
	}

	public AbstractUIView getUi() {
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
	public void diagramStateChange(long resourceId, String s) {	
	}

	@Override
	public void bindingChange(String binding) {
	}
	
	@Override
	public void nameChange(String nm) {
		setName(nm);
	}

	@Override
	public void valueChange(QualifiedValue value) {	
	}

	@Override
	public void watermarkChange(String newWatermark) {
		// TODO Auto-generated method stub
		
	}

}

