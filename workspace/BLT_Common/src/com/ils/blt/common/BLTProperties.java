/**
 *   (c) 2014 ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.UUID;


/**
 *  Define an interface for accessing module properties .
 */
public interface BLTProperties   {   
	public final static String MODULE_ID = "block";     // See module-blt.xml
	public final static String MODULE_NAME = "BLT";     // See build-blt.xml
	
	/** This is the name of the jar file containing block class definitions */
	public final static String BLOCK_JAR_NAME = "block-definition";
	
	public final static String APPLICATION_RESOURCE_TYPE   = "blt.application";
	public final static String DIAGRAM_RESOURCE_TYPE       = "blt.diagram";
	public final static String FAMILY_RESOURCE_TYPE        = "blt.family";
	public final static String FOLDER_RESOURCE_TYPE        = "__folder";
	
	public final static String BLOCK_SCRIPT_PACKAGE            = "system.ils.blt.block";
	public final static String REGISTRATION_SCRIPT_PACKAGE     = "system.ils.blt.registrar";
	
	/** These are recognized values of external callbacks for registration */
	public final static String CREATE_BLOCK_INSTANCE_CALLBACK     = "CreateBlockInstance";
	public final static String EVALUATE_CALLBACK                  = "Evaluate";
	public final static String GET_BLOCK_CLASSES_CALLBACK         = "GetBlockClasses";
	public final static String GET_BLOCK_PROPERTIES_CALLBACK      = "GetBlockProperties";
	public final static String GET_BLOCK_PROTOTYPES_CALLBACK      = "GetBlockPrototypes";
	public final static String SET_BLOCK_PROPERTY_CALLBACK        = "SetBlockProperty";
	public final static String SET_VALUE_CALLBACK                 = "SetValue";

	
	/** This unique ID represents the root node in the project tree */
	public static final UUID ROOT_FOLDER_UUID = UUID
			.fromString("7bbbd6b9-3140-4328-a844-51817eb47574");
	public static final String ROOT_FOLDER_NAME = "ROOT";   
	
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
	public static final String EXIM_PATH = "blt.exim.path";            // Default for file choose dialogs
	// These are the key names allowed in the Python dictionary that defines a block attribute.
	public static final String BLOCK_ATTRIBUTE_BINDING    = "binding";
	public static final String BLOCK_ATTRIBUTE_BINDING_TYPE = "bindingType";
	public static final String BLOCK_ATTRIBUTE_EDITABLE   = "editable";
	public static final String BLOCK_ATTRIBUTE_MAX        = "maximum";
	public static final String BLOCK_ATTRIBUTE_MIN        = "minimum";	
	public static final String BLOCK_ATTRIBUTE_NAME       = "name";
	public static final String BLOCK_ATTRIBUTE_QUALITY    = "quality";
	public static final String BLOCK_ATTRIBUTE_DATA_TYPE  = "type";
	public static final String BLOCK_ATTRIBUTE_VALUE      = "value";
	
	// These are the key names used a the Python dictionary that defines a block prototype.
	public static final String PALETTE_ANCHOR_IN   = "inports";     // A list of dictionaries
	public static final String PALETTE_ANCHOR_OUT  = "outports";    // A list of dictionaries
	public static final String PALETTE_BLOCK_CLASS = "blockClass";
	public static final String PALETTE_BLOCK_STYLE = "blockStyle";
	public static final String PALETTE_ICON_PATH   = "iconPath";
	public static final String PALETTE_LABEL       = "label";
	public static final String PALETTE_RECEIVE_ENABLED   = "receiveEnabled";
	public static final String PALETTE_TRANSMIT_ENABLED   = "transmitEnabled";
	public static final String PALETTE_TOOLTIP     = "tooltip";
	public static final String PALETTE_TAB_NAME    = "tabName";
	public static final String PALETTE_VIEW_BLOCK_ICON  = "viewBlockIcon";
	public static final String PALETTE_VIEW_FONT_SIZE  = "viewFontSize";
	public static final String PALETTE_VIEW_HEIGHT  = "viewHeight";
	public static final String PALETTE_VIEW_ICON   = "viewIcon";
	public static final String PALETTE_VIEW_LABEL  = "viewLabel";
	public static final String PALETTE_VIEW_WIDTH  = "viewWidth";
}
