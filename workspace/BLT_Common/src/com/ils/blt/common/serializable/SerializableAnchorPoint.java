package com.ils.blt.common.serializable;


import java.awt.Point;
import java.awt.Shape;
import java.util.UUID;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;


/**
 * Implement a plain-old-java-object representing an anchor point
 * that is serializable via the Ignition XML serializer.
 */
public class SerializableAnchorPoint {
	private final static String TAG = "SerializableAnchorPoint";
	private AnchorType type;   // 0=>Origin, 1=>Terminus
	private Object id = null;
	private UUID parentId = null;
	private Point anchor;
	private Point pathLeader;
	private Shape hotSpot;
	
	public SerializableAnchorPoint() {
	}
	
	public Object getId() { return id; }
	public AnchorType getType()   { return type; }
	public UUID getParentId() { return parentId; }
	public Point getAnchor() { return anchor; }
	public Point getPathLeader() { return pathLeader; }
	public Shape getHotSpot() { return hotSpot; }
	
	public void setId(Object identifier) { id=identifier; }
	public void setType(AnchorType t)   { type=t; }
	public void setParentId(UUID id) { parentId = id; };
	public void setAnchor(Point p) { anchor = p; }
	public void setPathLeader(Point p) { pathLeader = p; }
	public void setHotSpot(Shape hs) { hotSpot = hs; }

	
	@Override
	public String toString() {
		return String.format("%s: %s (%s)",TAG,id.toString(),(type==AnchorType.Origin?"Origin":"Terminus"));
	}

}
