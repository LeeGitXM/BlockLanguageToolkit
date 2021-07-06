/**
 *   (c) 2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.AttributeDisplay;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.workspace.ui.AbstractDisplayUIView;
import com.ils.blt.designer.workspace.ui.AttributeDisplayUIView;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.AbstractBlock;

/**
 * Display a single property of the referenced block.
 * 
 *  Note: initUI is called from the AbstractBlock constructor which is called
 *       when the diagram is opened.
 */
public class AttributeDisplayView extends AbstractBlock implements Block,NotificationChangeListener {
	private static final String CLSS = "AttributeDisplay";
	private final boolean DEBUG = true;
	private Map<String,ProcessAnchorDescriptor> anchors;
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private final Collection<AnchorPoint> anchorPoints = new ArrayList<>();
	private final ProcessBlockView block;
	private final ILSLogger log;
	private final String propertyName;
	private String text = "";      // Current value
	private final UUID uuid;
	private int preferredHeight = BlockConstants.PREFERRED_ATTRIBUTE_HEIGHT;             // Size the view to "natural" size
	private int preferredWidth  = BlockConstants.PREFERRED_ATTRIBUTE_WIDTH;              // Size the view to "natural" siz
	private int offsetX;
	private int offsetY;
	private int height = BlockConstants.DEFAULT_ATTRIBUTE_HEIGHT;
	private int width  = BlockConstants.DEFAULT_ATTRIBUTE_WIDTH;
	private Point location;
	private final AbstractDisplayUIView ui;
	
	/**
	 * Constructor: Used in the Designer to create a new display
	 */
	public AttributeDisplayView(ProcessBlockView blk,String name) {
		this.block = blk;
		this.propertyName = name;
		this.uuid = UUID.randomUUID();
		this.location = new Point(block.getLocation().x + offsetX,block.getLocation().y + offsetY);
		this.ui = new AttributeDisplayUIView(this);
		this.log = LogMaker.getLogger(this);
		
		// Add a single anchor that encompasses the entire block.
		this.anchors = new HashMap<>();
		/*
		ProcessAnchorDescriptor pad = new ProcessAnchorDescriptor();
		ap.setAnchorDirection(AnchorDirection.INCOMING);
		ap.setHidden(true);
		ap.setIsMultiple(false);
		ap.setName("center");
		ap.setSortOrder(0);
		anchors.put(ap.getName(), ap);
		*/
	}
	
	/**
	 * Constructor: Used to create a view from a serialized diagram.
	 */
	public AttributeDisplayView(ProcessDiagramView diagram,AttributeDisplay ad) {
		this.block = (ProcessBlockView)diagram.getBlock(UUID.fromString(ad.getBlockId()));
		this.propertyName = ad.getPropertyName();
		this.uuid = ad.getUUID();
		this.location = new Point(ad.getX(),ad.getY());
		this.offsetX = block.getLocation().x - location.x;
		this.offsetY = block.getLocation().y - location.y;
		this.ui = new AttributeDisplayUIView(this);
		this.log = LogMaker.getLogger(this);
	}
	
	@Override
	public Collection<AnchorPoint> getAnchorPoints() {
		return this.anchorPoints;
	}
	public ProcessBlockView getBlock() { return this.block; }
	public int getPreferredHeight() { return this.preferredHeight; }
	public int getPreferredWidth() { return this.preferredWidth; }
	public String getPropertyName() { return this.propertyName; }
	public AbstractDisplayUIView getUi() {return ui;}
	public AttributeDisplay convertToSerializable() {
		AttributeDisplay result = new AttributeDisplay(block.getId().toString(),propertyName);
		result.setX(getLocation().x);
		result.setY(getLocation().y);
		return result;
	}
	public void setPreferredHeight(int preferredHeight) {this.preferredHeight = preferredHeight;}
	public void setPreferredWidth(int preferredWidth) {this.preferredWidth = preferredWidth;}
	
	// The parent diagram is displayed, start the listener.
	public void startup() {
		String key = NotificationKey.keyForProperty(block.getId().toString(), propertyName);
		if(DEBUG) log.infof("%s.startup: listening for notification %s",CLSS,key);
		notificationHandler.initializePropertyValueNotification(key,text);
		notificationHandler.addNotificationChangeListener(key,CLSS, this);
	}
	
	public void shutdown() {
		String key = NotificationKey.keyForPropertyBinding(block.getId().toString(),propertyName);
		notificationHandler.removeNotificationChangeListener(key,CLSS);
	}
	// ================================== AbstractBlock ===========================================
	/*		This doesn't make a lot of sense.  Why does ignition pass
	 *      in a list of blocks selected when this only returns a single
	 *      block?  Very odd.  Not sure what to do with the rest of them
	 */
	@Override
	public Block copy(Map<UUID, UUID> arg0) {
		AttributeDisplay newDisplay= null;
		for (UUID me:arg0.values()) {  
			newDisplay = convertToSerializable();
		}
		
		return(Block)newDisplay;
	}

	@Override
	public AnchorPoint getDefaultDropAnchor(AnchorPoint arg0) {
		return null;	
	}

	@Override
	public UUID getId() {
		return uuid;
	}

	// Location is the upper left.
	@Override
	public Point getLocation() {
		return location;
	}

	// This is called in the block-and-connector framework
	// for each block as the diagram is opened.
	@Override
	public void initUI(BlockComponent blk) {
		getUi().install(blk);
	}

	@Override
	public void setLocation(Point loc) {
		this.location = loc;
	}
	
	// ======================================= Notification Change Listener ===================================
	@Override
	public void bindingChange(String pname,String binding) {
	}
	@Override
	public void diagramStateChange(long resId, String state) {}
	@Override
	public void nameChange(String name) {}
	// This is the important callback. Update the UI with the new value
	@Override
	public void propertyChange(String pname, Object value) {
		this.text = value.toString();
	}
	// Repaint when the value changes
	@Override
	public void valueChange(final QualifiedValue value) {
		this.text = value.getValue().toString();
	}
	@Override
	public void watermarkChange(String value) {}
}