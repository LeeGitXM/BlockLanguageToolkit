package com.ils.blt.common.serializable;

import java.awt.Point;
import java.util.UUID;

import com.ils.block.common.BlockState;
import com.ils.block.common.BlockStyle;

/**
 * Implement a plain-old-java-object representing a process block
 * that is serializable via the Ignition XML or GSON serializers.
 * Use arrays instead of Java-generics lists to make this serializable.
 */
public class SerializableBlock {
	private Point location = null;
	private UUID uuid = null;
	private String className = null;
	private BlockStyle style = BlockStyle.BASIC;
	private String label;
	private String statusText;
	private BlockState state;
	private SerializableAnchor[] anchors = null;
	
	public SerializableBlock() {
		this.location = new Point(0,0);
		this.anchors = new SerializableAnchor[0];
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
	public SerializableAnchor[] getAnchors() { return anchors; }
	public void setAnchors(SerializableAnchor[] array) {
		anchors = array;
		for(SerializableAnchor sa:anchors) {
			sa.setParentId(uuid);
		}
	}
}
