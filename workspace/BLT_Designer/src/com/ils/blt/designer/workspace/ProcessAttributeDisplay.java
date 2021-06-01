/**
 *   (c) 2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.util.Collection;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.block.AttributeDisplay;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.workspace.ui.AbstractUIView;
import com.ils.blt.designer.workspace.ui.AttributeDisplayUIView;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.AbstractBlock;

/**
 * Designer equivalents of these display a single property of another block.
 * In the Gateway these are simply "beans" and hold a few attributes.
 * 
 *  Note: initUI is called from the AbstractBlock constructor which is called
 *       when the diagram is opened.
 */
public class ProcessAttributeDisplay extends AbstractBlock implements NotificationChangeListener {
	private static final String CLSS = "AttributeDisplay";
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private final String blockId;
	private final String propertyName;
	private final UUID uuid;
	private int preferredHeight = 0;              // Size the view to "natural" size
	private int preferredWidth  = 0;              // Size the view to "natural" siz
	private int offsetX;
	private int offsetY;
	private int height;
	private int width;
	private Point location;
	private AbstractUIView ui = new AttributeDisplayUIView(this);
	
	/**
	 * Constructor: Used in the Designer to create a new display
	 */
	public ProcessAttributeDisplay(String id,String name) {
		this.blockId = id;
		this.propertyName = name;
		this.uuid = UUID.randomUUID();
		this.location = new Point(0,0);
	}
	
	/**
	 * Constructor: Used to create a view from a serialized diagram.
	 */
	public ProcessAttributeDisplay(AttributeDisplay ad) {
		this.blockId = ad.getBlockId();
		this.propertyName = ad.getPropertyName();
		this.uuid = ad.getUUID();
		this.location = new Point(ad.getX(),ad.getY());
	}
	
	public String getBlockId() { return this.blockId; }
	public int getPreferredHeight() { return this.preferredHeight; }
	public int getPreferredWidth() { return this.preferredWidth; }
	public String getPropertyName() { return this.propertyName; }
	public AbstractUIView getUi() {return ui;}
	public AttributeDisplay convertToSerializable() {
		AttributeDisplay result = new AttributeDisplay(blockId,propertyName);
		result.setX(getLocation().x);
		result.setY(getLocation().y);
		
		return result;
	}
	public void setPreferredHeight(int preferredHeight) {this.preferredHeight = preferredHeight;}
	public void setPreferredWidth(int preferredWidth) {this.preferredWidth = preferredWidth;}
	
	// The parent diagram is displayed, start the listener.
	public void startup() {
		String key = NotificationKey.keyForPropertyBinding(blockId,propertyName);
		notificationHandler.addNotificationChangeListener(key,CLSS,this);
	}
	
	public void shutdown() {
		String key = NotificationKey.keyForPropertyBinding(blockId,propertyName);
		notificationHandler.removeNotificationChangeListener(key,CLSS);
	}
	// ================================== AbstractBlock ===========================================
	/*		This doesn't make a lot of sense.  Why does ignition pass
	 *      in a list of blocks selected when this only returns a single
	 *      block?  Very odd.  Not sure what to do with the rest of them
	 */
	@Override
	public Block copy(Map<UUID, UUID> arg0) {
		ProcessAttributeDisplay newDisplay= null;
		for (UUID me:arg0.values()) {  
			newDisplay = new ProcessAttributeDisplay(this.convertToSerializable());
		}
		
		return newDisplay;
	}

	@Override
	public Collection<AnchorPoint> getAnchorPoints() {
		return null;
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
	public void bindingChange(String binding) {
	}
	@Override
	public void diagramStateChange(long resId, String state) {}
	// We get this when another entity changes a property. We just need to re-display.
	@Override
	public void nameChange(String name) {
	}
	@Override
	public void valueChange(final QualifiedValue value) {
		//log.infof("%s.valueChange: - %s new value (%s)",CLSS,property.getName(),value.getValue().toString());
		// updateUI
	}
	@Override
	public void watermarkChange(String value) {
	}


	
}