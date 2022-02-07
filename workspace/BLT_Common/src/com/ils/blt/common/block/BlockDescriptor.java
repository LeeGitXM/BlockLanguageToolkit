package com.ils.blt.common.block;

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * This view prototype contains all necessary information to create an
 * entry on the Block-and-Connector workspace representing the target block.
 * 
 * This class is designed to be serializable via JSON.
 */
public class BlockDescriptor {
	private static LoggerEx log = LogUtil.getLogger(BlockDescriptor.class.getPackage().getName());
	private static final String TAG = "BlockDescriptor";
	private List<AnchorPrototype> anchors;
	private int background = Color.WHITE.getRGB();  // Transmit color as an int for serialization.
	protected String blockClass = null;               // Class of the block in the Gateway (ils. ... implies Python)
	private int    embeddedFontSize = 24;
	private String embeddedIcon="";       // 32x32 icon to place in block in designer
	private String embeddedLabel="";      // Label place in block in designer
	private String editorClass = null;    // Class of custom property editor
	
	private boolean ctypeEditable=false;  // Can we globally change our connection types
	private boolean externallyAugmented  = false;     // True if this block uses auxiliary getters/setters
	private String iconPath = null;       // Icon to use for an icon-only block
	private int preferredHeight = 0;      // Size block to its "natural" size
	private int preferredWidth  = 0;
	private boolean receiveEnabled  = false;       // Whether or not this block can receive signals
	private BlockStyle style = BlockStyle.SQUARE;
	private String badgeChar = null;
//	private boolean transmitEnabled = false;       // Whether or not this block transmits signals
	
	public BlockDescriptor() {
		anchors = new ArrayList<AnchorPrototype>();
	}
	
	/**
	 * Deserialize from a Json 
	 * @param json
	 * @return the BlockDescriptor created from the string
	 */
	public static BlockDescriptor createPrototype(String json) {
		BlockDescriptor prototype = new BlockDescriptor();
		if( json!=null && json.length()>0 )  {
			ObjectMapper mapper = new ObjectMapper();

			try {
				prototype = mapper.readValue(json, BlockDescriptor.class);
			} 
			catch (JsonParseException jpe) {
				log.warnf("%s: createPrototype parse exception (%s)",TAG,jpe.getLocalizedMessage());
			}
			catch(JsonMappingException jme) {
				log.warnf("%s: createPrototype mapping exception (%s)",TAG,jme.getLocalizedMessage());
			}
			catch(IOException ioe) {
				log.warnf("%s: createPrototype IO exception (%s)",TAG,ioe.getLocalizedMessage());
			}
			 
		}
		return prototype;
	}

	public void addAnchor(AnchorPrototype anchor) { anchors.add(anchor); }
	public List<AnchorPrototype> getAnchors() { 
//		log.error("EREIAM jh - block descriptor get anchors");
		return anchors; 
		}
	public int getBackground() {return background;}
	public String getBlockClass() { return blockClass; }
	/**
	 * This class must be a JDialog, that takes a block view object in its constructor.
	 * @return a fully-qualified class name for a custom editor for the block's properties.
	 */
	public String getEditorClass() {return editorClass;}
	public int getEmbeddedFontSize() {return embeddedFontSize;}
	public String getEmbeddedIcon() {return embeddedIcon;}
	public String getEmbeddedLabel() {return embeddedLabel;}
	public String getIconPath() {return iconPath;}
	public int getPreferredHeight() {return preferredHeight;}
	public int getPreferredWidth() {return preferredWidth;}
	public BlockStyle getStyle() { return style; }
	public boolean isExternallyAugmented()      { return externallyAugmented; }
	public boolean isCtypeEditable() {return ctypeEditable;}
	public String getBadgeChar() {return badgeChar;}
	public boolean isReceiveEnabled() {return receiveEnabled;}
//	public boolean isTransmitEnabled() {return transmitEnabled;}
	
	public void setAnchors(List<AnchorPrototype> anchors) { this.anchors = anchors; }
	public void setBackground(int background) {this.background = background;}
	public void setBlockClass(String blockClass) { this.blockClass = blockClass; }
	public void setExternallyAugmented(boolean flag)      { this.externallyAugmented = flag; }
	public void setCtypeEditable(boolean ctypeEditable) {this.ctypeEditable = ctypeEditable;}
	public void setEditorClass(String editorClass) {this.editorClass = editorClass;}
	public void setEmbeddedFontSize(int embeddedFontSize) {this.embeddedFontSize = embeddedFontSize;}
	public void setEmbeddedIcon(String embeddedIcon) {this.embeddedIcon = embeddedIcon;}
	public void setEmbeddedLabel(String embeddedLabel) {this.embeddedLabel = embeddedLabel;}
	public void setIconPath(String iconPath) {this.iconPath = iconPath;}
	public void setPreferredHeight(int preferredHeight) {this.preferredHeight = preferredHeight;}
	public void setPreferredWidth(int preferredWidth) {this.preferredWidth = preferredWidth;}
	public void setReceiveEnabled(boolean receiveEnabled) {this.receiveEnabled = receiveEnabled;}
	public void setStyle(BlockStyle style) { this.style = style; }
	public void setBadgeCharacter(String ch) { this.badgeChar = ch; }
//	public void setTransmitEnabled(boolean transmitEnabled) {this.transmitEnabled = transmitEnabled;}

	/**
	 * Serialize into a JSON string
	 * @return the descriptor expressed as JSON
	 */
	public String toJson() {
		ObjectMapper mapper = new ObjectMapper();
		String json="";
		log.warnf("%s: toJson ...",TAG);
		try {
			json = mapper.writeValueAsString(this);
		}
		catch(Exception ge) {
			log.warnf("%s: toJson (%s)",TAG,ge.getMessage());
		}
		return json;
	}
}
