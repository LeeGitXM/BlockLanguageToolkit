package com.ils.blt.common.serializable;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.ils.block.common.BlockState;
import com.ils.block.common.BlockStyle;

/**
 * Implement a plain-old-java-object representing a process block
 * that is serializable via the Ignition XML serializer.
 */
public class SerializableBlock {
	private Point location = null;
	private UUID uuid = null;
	private String className = null;
	private BlockStyle style = BlockStyle.BASIC;
	private String label;
	private String statusText;
	private BlockState state;
	private List<SerializableAnchor> anchors = null;
	
	public SerializableBlock() {
		this.location = new Point(0,0);
		this.anchors = new ArrayList<SerializableAnchor>();
	}
	
	public String getClassName() {return className;}
	public void setClassName(String className) {this.className = className;}
	public UUID getId() { return uuid; }
	public void setId(UUID id) { uuid = id; }
	public String getLabel() { return label; }
	public void setLabel(String label) { this.label = label; }
	public Point getLocation() { return location; }
	public void setLocation(Point p) { location=p; }
	public BlockStyle getStyle() { return style; }
	public String getStatusText() { return statusText; }
	public void setStatusText(String statusText) { this.statusText = statusText; }
	public BlockState getState() { return state; }
	public void setState(BlockState state) { this.state = state; }
	public void setStyle(BlockStyle style) { this.style = style; }
	public List<SerializableAnchor> getAnchors() { return anchors; }
	public void setAnchors(List<SerializableAnchor> list) {
		anchors=list;
		for(SerializableAnchor sa:anchors) {
			sa.setParentId(uuid);
		}
	}
}
