package com.ils.blt.common.serializable;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * Implement a plain-old-java-object representing a process block
 * that is serializable via the Ignition XML serializer.
 */
public class SerializableBlock {
	private Point location = null;
	private UUID uuid = null;
	private List<SerializableAnchor> anchors = null;
	
	public SerializableBlock() {
		this.location = new Point(0,0);
		this.anchors = new ArrayList<SerializableAnchor>();
	}
	
	public UUID getId() { return uuid; }
	public Point getLocation() { return location; }
	public void setId(UUID id) { uuid = id; }
	public void setLocation(Point p) { location=p; }
	
	public List<SerializableAnchor> getAnchors() { return anchors; }
	public void setAnchors(List<SerializableAnchor> list) {
		anchors=list;
		for(SerializableAnchor sa:anchors) {
			sa.setParentId(uuid);
		}
	}

}
