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
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.designer.workspace.ui.AbstractUIView;
import com.ils.blt.designer.workspace.ui.UIFactory;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
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
public class ProcessBlockView extends AbstractBlock implements ChangeListener {
	private static final String TAG = "ProcessBlockView";
	private final static Random random = new Random();
	private Map<String,ProcessAnchorDescriptor> anchors;
	private final EventListenerList listenerList;
	private GeneralPurposeDataContainer auxiliaryData = null;
	private final ChangeEvent changeEvent;
	private int background = Color.white.getRGB();
	private final String className;
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
	private String name = null;                   // Text to display on the block
	private boolean nameDisplayed = false;
	private int nameOffsetX = 0;     // When displayed as an attribute
	private int nameOffsetY = 0;     // When displayed as an attribute
	private Point location = new Point(0,0);
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private int preferredHeight = 0;              // Size the view to "natural" size
	private int preferredWidth  = 0;              // Size the view to "natural" size
	private Collection<BlockProperty> properties;
	private boolean receiveEnabled = false;
	private TruthValue state = TruthValue.UNSET;
	private String statusText;                    // Auxiliary text to display
	private UUID subworkspaceId = null;           // Encapsulated diagram if encapsulation block
	private BlockStyle style = BlockStyle.SQUARE;
	private boolean transmitEnabled = false;
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
		this.nameDisplayed  = descriptor.isNameDisplayed();
		this.nameOffsetX    = descriptor.getNameOffsetX();
		this.nameOffsetY    = descriptor.getNameOffsetY();
		this.receiveEnabled = descriptor.isReceiveEnabled();
		this.transmitEnabled= descriptor.isTransmitEnabled();

		this.anchors = new HashMap<>();
		for( AnchorPrototype ap:descriptor.getAnchors() ) {
			log.debugf("%s: Creating anchor descriptor %s", TAG,ap.getName());
			ProcessAnchorDescriptor pad = new ProcessAnchorDescriptor((ap.getAnchorDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
					ap.getConnectionType(),UUID.randomUUID(),ap.getName(),ap.getAnnotation(),ap.getHint(),ap.isMultiple());
			pad.setHidden(ap.isHidden());
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
		this.name = sb.getName();
		this.nameDisplayed = sb.isNameDisplayed();
		this.nameOffsetX   = sb.getNameOffsetX();
		this.nameOffsetY   = sb.getNameOffsetY();
		this.state = sb.getState();
		this.statusText = sb.getStatusText();
		this.receiveEnabled  = sb.isReceiveEnabled();
		this.transmitEnabled = sb.isTransmitEnabled();
		this.subworkspaceId = sb.getSubworkspaceId();
		this.anchors = new HashMap<>();
		if(sb.getAnchors()!=null ) {
			for( SerializableAnchor sa:sb.getAnchors() ) {
				log.debugf("%s: %s creating serializable anchor %s (%s)", TAG,sb.getName(),sa.getDisplay(),sa.getConnectionType().name());
				ProcessAnchorDescriptor pad = new ProcessAnchorDescriptor((sa.getDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
						sa.getConnectionType(),sa.getId(),sa.getDisplay(),sa.getAnnotation(),sa.getHint(),sa.isMultiple());
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
		result.setHidden(anchor.isHidden());
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
		result.setNameDisplayed(isNameDisplayed());
		result.setNameOffsetX(getNameOffsetX());
		result.setNameOffsetY(getNameOffsetY());
		result.setPreferredHeight(getPreferredHeight());
		result.setPreferredWidth(getPreferredWidth());
		result.setState(getState());
		result.setStatusText(getStatusText());
		result.setStyle(getStyle());
		result.setSubworkspaceId(subworkspaceId);
		result.setReceiveEnabled(isReceiveEnabled());
		result.setTransmitEnabled(isTransmitEnabled());
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
     * broadcast ports are not changed.
     * @param newType
     */
    public void changeConnectorType(ConnectionType newType) {
    	if(ctypeEditable) {
    		boolean foundSignal = false;
    		for( ProcessAnchorDescriptor anchor:getAnchors()) {
    			if( !foundSignal && anchor.getConnectionType().equals(ConnectionType.SIGNAL)) {
    				foundSignal = true;
    			}
    			else if(anchor.getDisplay().equals(BlockConstants.CONTROL_PORT_NAME) ||
    					anchor.getDisplay().equals(BlockConstants.BROADCAST_PORT_NAME) ) {
    				continue;
    			}
    			else {
    				anchor.setConnectionType(newType);
    			}
    		}
    		ui.reconfigure();
    		fireStateChanged();
    	}
    }

	@Override
	public Block copy(Map<UUID, UUID> arg0) {
		log.infof("%s: copy ...", TAG);
		return null;
	}
	@Override
	public Collection<AnchorPoint> getAnchorPoints() {
		if( ui==null ) ui = factory.getUI(style, this);
		return ui.getAnchorPoints();	
	}
	public Collection<ProcessAnchorDescriptor> getAnchors() { return anchors.values(); }
	public GeneralPurposeDataContainer getAuxiliaryData() {return auxiliaryData;}
	public int getBackground() { return background;}
	public String getClassName() { return className; }

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
	public String getName() {return name;}
	public int getNameOffsetX() { return nameOffsetX; }
	public int getNameOffsetY() { return nameOffsetY; }
	public int getPreferredHeight() {return preferredHeight;}
	public int getPreferredWidth() {return preferredWidth;}
	public Collection<BlockProperty> getProperties() { return properties; }
	public TruthValue getState() {return state;}
	public String getStatusText() { return statusText; }
	public BlockStyle getStyle() { return style; }
	public UUID getSubworkspaceId() {return subworkspaceId;}
	
	@Override
	public void initUI(BlockComponent blk) {
		ui = factory.getUI(style, this);
		ui.install(blk);
	}
	public boolean isCtypeEditable() {return ctypeEditable;}
	public boolean isDirty() {return dirty;}
	public boolean isEncapsulation() {return encapsulation;}
	public boolean isLocked() {return locked;}
	public boolean isNameDisplayed() { return nameDisplayed; }
	public boolean isReceiveEnabled() {return receiveEnabled;}
	public boolean isSignalAnchorDisplayed() {
		for(ProcessAnchorDescriptor pad:anchors.values()) {
			if(pad.getDisplay().equals(BlockConstants.SIGNAL_PORT_NAME)) {
				return !pad.isHidden();
			}
		}
		return false;
	}
	public boolean isTransmitEnabled() {return transmitEnabled;}
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
	public void setCtypeEditable(boolean ctypeEditable) {this.ctypeEditable = ctypeEditable;}
	public void setDirty(boolean dirty) {this.dirty = dirty;} 
	public void setEditorClass(String editorClass) {this.editorClass = editorClass;}
	public void setEncapsulation(boolean encapsulation) {this.encapsulation = encapsulation;}
	public void setEmbeddedFontSize(int size) {this.embeddedFontSize = size;}
	public void setEmbeddedIcon(String embeddedIcon) {this.embeddedIcon = embeddedIcon;}
	public void setEmbeddedLabel(String embeddedLabel) {this.embeddedLabel = embeddedLabel;}
	public void setIconPath(String iconPath) {this.iconPath = iconPath;}
	public void setLocked(boolean flag) {this.locked = flag;}
	public void setName(String label) {this.name = label;}
	public void setNameDisplayed(boolean showName) {this.nameDisplayed = showName;}
	public void setNameOffsetX(int nameOffsetX) {this.nameOffsetX = nameOffsetX;}
	public void setNameOffsetY(int nameOffsetY) {this.nameOffsetY = nameOffsetY;}
	@Override
	public void setLocation(Point loc) {
		location = loc;
		fireBlockMoved();
	}
	public void setPreferredHeight(int preferredHeight) {this.preferredHeight = preferredHeight;}
	public void setPreferredWidth(int preferredWidth) {this.preferredWidth = preferredWidth;}
	public void setProperties(Collection<BlockProperty> props) { this.properties = props; }
	public void setReceiveEnabled(boolean receiveEnabled) {this.receiveEnabled = receiveEnabled;}
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
	public void setTransmitEnabled(boolean transmitEnabled) {this.transmitEnabled = transmitEnabled;}
	
	
	
	/**
	 * Create a name that is highly likely to be unique within the diagram.
	 * The name can be user-modified at any time. If we really need a uniqueness,
	 * use the block's UUID.
	 */
	public void createPseudoRandomName() {
		String root = className;
		int pos = className.lastIndexOf(".");
		if( pos>=0 )  root = className.substring(pos+1);
		name = String.format("%s-%d", root.toUpperCase(),random.nextInt(1000));
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
		if( ui != null ) {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					ui.update();
				}
			});
		}
		
	}
}
