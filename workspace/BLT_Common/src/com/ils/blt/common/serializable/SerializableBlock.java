package com.ils.blt.common.serializable;

import java.util.UUID;

import com.ils.block.common.BlockProperty;
import com.ils.block.common.BlockState;
import com.ils.block.common.BlockStyle;

/**
 * Implement a plain-old-java-object representing a process block
 * that is serializable via a XML or JSON serializer.
 * 
 * Use arrays instead of Java-generics lists to make this serializable.
 * WARNING: Avoid use of Point class as it contains a circular reference
 *          to itself.
 */
public class SerializableBlock {
	private int x = 0;
	private int y = 0;
	private UUID uuid = null;
	private String className = null;
	private String embeddedIcon="";       // 32x32 icon to place in block in designer
	private String embeddedLabel="";      // Label place in block in designer
	private BlockStyle style = BlockStyle.BASIC;
	private String label;
	private String statusText;
	private BlockState state;
	private BlockProperty[] properties = null;
	private SerializableAnchor[] anchors = null;
	
	public SerializableBlock() {
		this.anchors = new SerializableAnchor[0];
	}
	
	public String getClassName() {return className;}
	public void setClassName(String className) {this.className = className;}
	public UUID getId() { return uuid; }
	public void setId(UUID id) { uuid = id; }
	public String getLabel() { return label; }
	public void setLabel(String label) { this.label = label; }
	public int getX() { return x; }
	public int getY() { return y; }
	public void setX(int xx) { this.x=xx; }
	public void setY(int yy) { this.y=yy; }
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
	public String getEmbeddedIcon() {return embeddedIcon;}
	public void setEmbeddedIcon(String embeddedIcon) {this.embeddedIcon = embeddedIcon;}
	public String getEmbeddedLabel() {return embeddedLabel;}
	public void setEmbeddedLabel(String embeddedLabel) {this.embeddedLabel = embeddedLabel;}
	public BlockProperty[] getProperties() { return properties; }
	public void setProperties(BlockProperty[] array) { this.properties = array; }

}
