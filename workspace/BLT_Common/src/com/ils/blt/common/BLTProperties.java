/**
 *   (c) 2014 ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.awt.Color;
import java.util.UUID;


/**
 *  Define an interface for accessing module properties .
 */
public interface BLTProperties   {   
	public final static String MODULE_ID = "block";     // See module-blt.xml
	public final static String MODULE_NAME = "BLT";     // See build-blt.xml
	public final static String SFC_MODULE_ID = "com.ils.sfc"; 
	
	/** This is the name of the jar file containing block class definitions */
	public final static String BLOCK_JAR_NAME = "block-definition";
	
	public final static String APPLICATION_RESOURCE_TYPE   = "blt.application";
	public final static String DIAGRAM_RESOURCE_TYPE       = "blt.diagram";
	public final static String FAMILY_RESOURCE_TYPE        = "blt.family";
//	public final static String FAMILY_FOLDER_TYPE          = "blt.familyFolder";
	public final static String FOLDER_RESOURCE_TYPE        = "__folder";
	
	public final static String DIAGRAM_SCRIPT_PACKAGE      = "system.ils.blt.diagram";
	
	/** This unique ID represents the root node in the project tree */
	public static final UUID ROOT_FOLDER_UUID = UUID
			.fromString("7bbbd6b9-3140-4328-a844-51817eb47574");
	public static final String ROOT_FOLDER_NAME = "ROOT";
	public static final long ROOT_PARENT_ID = 0;    // For the status manager
	public static final long ROOT_RESOURCE_ID = -1;
	
	// This is the common prefix under which bundle files are identified/registered
	public static final String BUNDLE_PREFIX = "blt";
	public static final String BLOCK_PREFIX = "block";
	public static final String CUSTOM_PREFIX = "custom";
	// This is where we find the string resources for blocks
	public static final String BLOCK_RESOURCE_PATH = "com.ils.blt.designer";
	
	// These are the property names in the message payload, gateway to client
	public static final String MSG_BLOCK_NAME      = "BlockName";
	public static final String MSG_BLOCK_STATE     = "BlockState";
	public static final String MSG_WORKSPACE_ID    = "WorkspaceID";    // UUID of the component's workspace
	
	// These are names of system properties  
	public static final String EXIM_PATH = "blt.exim.path";            // Initial default for file choose dialogs
	
	// These are the key names used a the Python dictionary that defines an anchor prototype.
	public static final String ANCHOR_ANNOTATION  = "annotation";   // port annotation
	public static final String ANCHOR_DIRECTION   = "direction";    // incoming or outgoing
	public static final String ANCHOR_HINT   = "hint";     			// Placement hint
	public static final String ANCHOR_NAME   = "name";     			// Name of the anchor
	public static final String ANCHOR_TYPE   = "type";              // Connection type
		
	// These are the key names allowed in the Python dictionary that defines a block attribute.
	public static final String BLOCK_ATTRIBUTE_BINDING    = "binding";
	public static final String BLOCK_ATTRIBUTE_BINDING_TYPE = "bindingType";
	public static final String BLOCK_ATTRIBUTE_CLASS      = "class";
	public static final String BLOCK_ATTRIBUTE_EDITABLE   = "editable";
	public static final String BLOCK_ATTRIBUTE_ID         = "uuid";
	public static final String BLOCK_ATTRIBUTE_ISSUE      = "issue";   // Reason for being invalid
	public static final String BLOCK_ATTRIBUTE_NAME       = "name";
	public static final String BLOCK_ATTRIBUTE_PARENT     = "parent";  // UUID of parent diagram
	public static final String BLOCK_ATTRIBUTE_PATH       = "path";
	public static final String BLOCK_ATTRIBUTE_QUALITY    = "quality";
	public static final String BLOCK_ATTRIBUTE_TARGET     = "target";
	public static final String BLOCK_ATTRIBUTE_DATA_TYPE  = "type";
	public static final String BLOCK_ATTRIBUTE_VALUE      = "value";
	
	// Class names for some specific blocks
	public static final String CLASS_NAME_INPUT      = "com.ils.block.Input";
	public static final String CLASS_NAME_OUTPUT     = "com.ils.block.Output";
	public static final String CLASS_NAME_SINK       = "com.ils.SinkConnection";
	public static final String CLASS_NAME_SOURCE     = "com.ils.SourceConnection";
	
	// Arguments for Color constructor are: R G B
	public static final Color DIAGRAM_ACTIVE_BACKGROUND          = new Color(0.9f,0.9f,0.9f);    // Light gray
	public static final Color DIAGRAM_ISOLATED_BACKGROUND     = new Color(0.46f,0.67f,0.9f);  // light blue
	public static final Color DIAGRAM_DIRTY_BACKGROUND           = new Color(0.94f,0.92f,0.5f);  // mustard
	public static final Color DIAGRAM_DISABLED_BACKGROUND        = new Color(0.6f,0.6f,0.6f);    // Dark gray
	
	// These are the key names used a the Python dictionary that defines a block prototype.
	public static final String PALETTE_ANCHOR_IN   = "inports";     // A list of dictionaries
	public static final String PALETTE_ANCHOR_OUT  = "outports";    // A list of dictionaries
	public static final String PALETTE_AUX_DATA    = "auxData";
	public static final String PALETTE_BLOCK_CLASS = "blockClass";
	public static final String PALETTE_BLOCK_STYLE = "blockStyle";
	public static final String PALETTE_EDITOR_CLASS = "editorClass";
	public static final String PALETTE_ICON_PATH   = "iconPath";
	public static final String PALETTE_LABEL       = "label";
	public static final String PALETTE_NAME_DISPLAYED    = "nameDisplayed";
	public static final String PALETTE_NAME_OFFSET_X     = "nameOffsetX";
	public static final String PALETTE_NAME_OFFSET_Y     = "nameOffsetY";
//	public static final String PALETTE_RECEIVE_ENABLED   = "receiveEnabled";
//	public static final String PALETTE_TRANSMIT_ENABLED   = "transmitEnabled";
	public static final String PALETTE_TOOLTIP     = "tooltip";
	public static final String PALETTE_TAB_NAME    = "tabName";
	public static final String PALETTE_VIEW_BACKGROUND = "viewBackgroundColor";   // RGB as an int
	public static final String PALETTE_VIEW_BLOCK_ICON  = "viewBlockIcon";
	public static final String PALETTE_VIEW_FONT_SIZE  = "viewFontSize";
	public static final String PALETTE_VIEW_HEIGHT  = "viewHeight";
	public static final String PALETTE_VIEW_ICON   = "viewIcon";
	public static final String PALETTE_VIEW_LABEL  = "viewLabel";
	public static final String PALETTE_VIEW_WIDTH  = "viewWidth";
	
	// Marker for a null qualified value
	public static final String UNDEFINED = "UNDEFINED";
	
	// Name to use for "Good" Quality
	public static final String QUALITY_GOOD        = "Good";
	// Preferences keys
	public static final String PREFERENCES_NAME     = "BLTPreferences";    // Preferences collection name
	public static final String PREF_EXIM_DIRECTORY  = "ExImDirectory";     // Export/import directory
	public static final String PREF_CONFIG_DIRECTORY= "ConfigDirectory";   // Project properties configuration
	public static final String PREF_RESPONSE_CLASS    = "ResponseClass";       // Class of block we're concerned about
	public static final String PREF_RESPONSE_INTERVAL = "ResponseInterval";    // Hours to declare block "unresponsive"
}
