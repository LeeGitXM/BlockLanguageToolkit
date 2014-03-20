package com.ils.blt.common.serializable;


import java.awt.Rectangle;
import java.util.UUID;

import com.ils.block.common.AnchorDirection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;


/**
 * Implement a plain-old-java-object representing an anchor point
 * that is serializable via a XML or JSON serializer.
 * 
 * WARNING: Avoid the use of the Point class as it has a circular
 *          reference, making it non-serializable. Shape references
 *          Point also.
 */
public class SerializableAnchorPoint {
	private final static String TAG = "SerializableAnchorPoint";
	private AnchorDirection direction;   // 0=>Origin, 1=>Terminus
	private Object id = null;
	private UUID parentId = null;
	private int anchorX = 0;
	private int anchorY = 0;
	private int pathLeaderX = 0;
	private int pathLeaderY = 0;
	private int hotSpotX = 0;
	private int hotSpotY = 0;
	private int hotSpotHeight = 0;
	private int hotSpotWidth = 0;
	
	public SerializableAnchorPoint() {
	}
	
	public SerializableAnchorPoint (AnchorPoint anchor) {
		if(anchor.isConnectorOrigin()) this.setDirection(AnchorDirection.OUTGOING);
		else this.setDirection(AnchorDirection.INCOMING);
		this.setId(anchor.getId());
		this.setParentId(anchor.getBlock().getId());
		this.setAnchorX(anchor.getAnchor().x);
		this.setAnchorY(anchor.getAnchor().y);
		this.setHotSpot(anchor.getHotSpot().getBounds());
		this.setPathLeaderX(anchor.getPathLeader().x);
		this.setPathLeaderY(anchor.getPathLeader().y);
	}
	
	public Object getId() { return id; }
	public AnchorDirection getDirection()   { return direction; }
	public UUID getParentId() { return parentId; }
	public int getAnchorX() { return anchorX; }
	public int getAnchorY() { return anchorY; }
	public int getPathLeaderX() { return pathLeaderX; }
	public int getPathLeaderY() { return pathLeaderY; }
	public int getHotSpotX() {return hotSpotX;}
	public int getHotSpotY() {return hotSpotY;}
	public int getHotSpotWidth() {return hotSpotWidth;}
	public int getHotSpotHeight() {return hotSpotHeight;}
	
	public void setId(Object identifier) { id=identifier; }
	public void setDirection(AnchorDirection t)   { direction=t; }
	public void setParentId(UUID id) { parentId = id; };
	public void setAnchorX(int x) { anchorX = x; }
	public void setAnchorY(int y) { anchorY = y; }
	public void setPathLeaderX(int x) { pathLeaderX = x; }
	public void setPathLeaderY(int y) { pathLeaderY = y; }
	public void setHotSpotX(int hotSpotX) {this.hotSpotX = hotSpotX;}
	public void setHotSpotY(int hotSpotY) {this.hotSpotY = hotSpotY;}
	public void setHotSpotWidth(int hotSpotWidth) {this.hotSpotWidth = hotSpotWidth;}
	public void setHotSpotHeight(int hotSpotHeight) {this.hotSpotHeight = hotSpotHeight;}
	
	// Convenience method
	public void setHotSpot(Rectangle hs) { hotSpotX=hs.x; hotSpotY=hs.y; hotSpotWidth=hs.width; hotSpotHeight=hs.height; }


	@Override
	public String toString() {
		return String.format("%s: %s (%s)",TAG,id.toString(),(direction==AnchorDirection.INCOMING?"Incoming":"Outgoing"));
	}

}
