package com.ils.blt.common.block;

import java.io.IOException;
import java.io.Serializable;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * This palette prototype contains all necessary information to create an
 * entry on the Block-and-Connector palette representing the target block.
 * 
 * This class is designed to be Serializable.
 */
public class PalettePrototype implements Serializable {
	private static final long serialVersionUID = 6495150228923607776L;
	private static final String TAG = "PalettePrototype";
	private static LoggerEx log = LogUtil.getLogger(PalettePrototype.class.getPackage().getName());
	private String paletteIconPath;       // Path to icon for view in the palette
	private String paletteLabel;          // Label appearing under icon in palette
	private String tabName="";
	private String tooltipText="";        // Tooltip for in palette
	private BlockDescriptor blockDescriptor;  // Describe block in diagram view - use a view prototype
	
	
	public PalettePrototype() {
		this.blockDescriptor = new BlockDescriptor();
	}
	
	/**
	 * Deserialize from a Json string
	 * @param json
	 * @return the prototype created from the string
	 */
	public static PalettePrototype createPrototype(String json) {
		PalettePrototype prototype = new PalettePrototype();
		if( json!=null && json.length()>0 )  {
			ObjectMapper mapper = new ObjectMapper();

			try {
				prototype = mapper.readValue(json, PalettePrototype.class);
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

	public String getPaletteIconPath() { return paletteIconPath; }
	public void setPaletteIconPath(String paletteIconPath) { this.paletteIconPath = paletteIconPath; }
	public String getPaletteLabel() {return paletteLabel;}
	public void setPaletteLabel(String paletteLabel) {
		this.paletteLabel = paletteLabel;
	}
	public String getTooltipText() {return tooltipText;}
	public void setTooltipText(String tooltipText) {this.tooltipText = tooltipText;}
	public BlockDescriptor getBlockDescriptor() { return blockDescriptor; }
	public void setBlockDescriptor(BlockDescriptor desc) { this.blockDescriptor = desc; }
	public String getTabName() {return tabName;}
	public void setTabName(String tabName) {this.tabName = tabName;}
	
	
	/**
	 * Serialize into a JSON string
	 */
	public String toJson() {
		ObjectMapper mapper = new ObjectMapper();
		String json="";
		try {
			json = mapper.writeValueAsString(this);
		}
		catch(Exception ge) {
			log.warnf("%s: toJson (%s)",TAG,ge.getMessage());
		}
		log.tracef("%s: toJson = %s",TAG,json);
		return json;
	}
}
