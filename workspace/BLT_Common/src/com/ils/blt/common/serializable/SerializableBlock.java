package com.ils.blt.common.serializable;

import java.awt.Color;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.TruthValue;

/**
 * Implement a plain-old-java-object representing a process block
 * that is serializable via a XML or JSON serializer.
 * 
 * Use arrays instead of Java-generics lists to make this serializable.
 * WARNING: Avoid use of Point class as it contains a circular reference
 *          to itself.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SerializableBlock {
	private SerializableAnchor[] anchors = null;
	private int background = Color.white.getRGB();
	private String className = null;
	private boolean dirty = false;
	private String editorClass= null;
	private int    embeddedFontSize = 24; // Points in font for embedded label
	private String embeddedIcon="";       // 32x32 icon to place in block in designer
	private String embeddedLabel="";      // Label place in block in designer
	private String iconPath="";           // Path to icon that is the entire block
	private boolean locked = false;
	private String name;
	private boolean nameDisplayed = false;
	private int nameOffsetX = 0;     // When displayed as an attribute
	private int nameOffsetY = 0;     // When displayed as an attribute
	private UUID originalId = null;       // Id of block from which this was cloned
	private int preferredHeight = 0;
	private int preferredWidth  = 0;
	private BlockProperty[] properties = null;
	private boolean receiveEnabled = false;
	private TruthValue state = TruthValue.UNSET;
	private String statusText;
	private BlockStyle style = BlockStyle.SQUARE;
	private UUID subworkspaceId = null;     // Non-null only for encapsulation blocks. Subworkspace is a diagram.
	private boolean transmitEnabled= false;
	private UUID uuid = null;
	private int x = 0;
	private int y = 0;
	public SerializableBlock() {
		this.anchors = new SerializableAnchor[0];
	}
	public SerializableAnchor[] getAnchors() { return anchors; }
	public int getBackground() {return background;}
	public String getClassName() {return className;}
	public String getEditorClass() {return editorClass;}
	public int getEmbeddedFontSize() {return embeddedFontSize;}
	public String getEmbeddedIcon() {return embeddedIcon;}
	public String getEmbeddedLabel() {return embeddedLabel;}
	public String getIconPath() {return iconPath;}
	public UUID getId() { return uuid; }
	public String getName() { return name; }
	public int getNameOffsetX() {return nameOffsetX;}
	public int getNameOffsetY() {return nameOffsetY;}
	public UUID getOriginalId() { return originalId; }
	public int getPreferredHeight() {return preferredHeight;}
	public int getPreferredWidth() {return preferredWidth;}
	public BlockProperty[] getProperties() { return properties; }
	public TruthValue getState() {return state;}
	public String getStatusText() { return statusText; }
	public UUID getSubworkspaceId() {return subworkspaceId;}
	public BlockStyle getStyle() { return style; }
	public int getX() { return x; }
	public int getY() { return y; }
	
	public boolean isDirty() {return dirty;}
	public boolean isLocked() {return locked;}
	public boolean isNameDisplayed() {return nameDisplayed;}
	public boolean isReceiveEnabled() {return receiveEnabled;}
	public boolean isTransmitEnabled() {return transmitEnabled;}
	public void setAnchors(SerializableAnchor[] array) {
		anchors = array;
		for(SerializableAnchor sa:anchors) {
			sa.setParentId(uuid);
		}
	}
	public void setBackground(int background) {this.background = background;}
	public void setClassName(String className) {this.className = className;}
	public void setDirty(boolean dirty) {this.dirty = dirty;}
	public void setEditorClass(String editorClass) {this.editorClass = editorClass;}
	public void setEmbeddedFontSize(int embeddedFontSize) {this.embeddedFontSize = embeddedFontSize;}
	public void setEmbeddedIcon(String embeddedIcon) {this.embeddedIcon = embeddedIcon;}
	public void setEmbeddedLabel(String embeddedLabel) {this.embeddedLabel = embeddedLabel;}
	public void setIconPath(String iconPath) {this.iconPath = iconPath;}
	public void setId(UUID id) { uuid = id; }
	public void setLocked(boolean flag) { this.locked = flag; }
	public void setName(String label) { this.name = label; }
	public void setNameDisplayed(boolean showName) {this.nameDisplayed = showName;}
	public void setNameOffsetX(int nameOffsetX) {this.nameOffsetX = nameOffsetX;}
	public void setNameOffsetY(int nameOffsetY) {this.nameOffsetY = nameOffsetY;}
	public void setOriginalId(UUID id) { originalId = id; }
	public void setPreferredHeight(int preferredHeight) {this.preferredHeight = preferredHeight;}
	public void setPreferredWidth(int preferredWidth) {this.preferredWidth = preferredWidth;}
	public void setProperties(BlockProperty[] array) { this.properties = array; }
	public void setReceiveEnabled(boolean receiveEnabled) {this.receiveEnabled = receiveEnabled;}
	public void setState(TruthValue state) {if(state!=null) this.state = state;}
	public void setStatusText(String statusText) { this.statusText = statusText; }
	public void setSubworkspaceId(UUID subworkspace) {this.subworkspaceId = subworkspace;}
	public void setStyle(BlockStyle style) { this.style = style; }
	public void setTransmitEnabled(boolean transmitEnabled) {this.transmitEnabled = transmitEnabled;}
	public void setX(int xx) { this.x=xx; }
	public void setY(int yy) { this.y=yy; }
}
