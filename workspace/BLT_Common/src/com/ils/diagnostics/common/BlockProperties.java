/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 *  
 *   Class contains static constants that have meaning to the ILS-Core module.
 */
package com.ils.diagnostics.common;


/**
 *  Global properties for SCT blocks.
 */
public interface BlockProperties   {
	
	// These are the standard node names for the Gateway module configuration menus
	public static String ROOT = "ILS";       // Root of Gateway menu tree
	// These properties support block language execution
	/** The message type used to report block state changes between gateway and designer. */
	public final static String SCT_BUNDLE_KEY = "sct";
	public final static String GATEWAY_BLOCK_STATE_MESSAGE = "block.state";
	
	// These are the recognized project resource types
	public final static String FOLDER_RESOURCE_TYPE    = "__folder";  // See ProjectResource
	public final static String MODEL_RESOURCE_TYPE     = "ils.model"; // gson description of the model
	public final static String PANEL_RESOURCE_TYPE     = "ils.panel"; // holds a WorkspacePanel
	public final static String WINDOW_RESOURCE_TYPE    = "window";    // fpmi window, encapsulates a WindowInfo
	
	// These are block property names that have meaning to the BlockController
	public static final String BLOCK_PROPERTY_ACTION_SCRIPT  = "ActionScript";            // Action Performed Script
	public static final String BLOCK_PROPERTY_CHANGE_SCRIPT  = "PropertyChangeScript";    // Property changed script
	public static final String BLOCK_PROPERTY_PARENT_WORKSPACE = "ParentWorkspace";       // UUID of workspace connected to,
	public static final String BLOCK_PROPERTY_DELAY  = "Delay";                           // sleep time ~msecs
	public static final String BLOCK_PROPERTY_NAME  = "Name";                             // Block name
	public static final String BLOCK_PROPERTY_NEXT  = "NextBlocks";                       // Comma-separated list
	public static final String BLOCK_PROPERTY_PARENT= "Parent";                           // Name of the superior block
	public static final String BLOCK_PROPERTY_SCRIPT= "Script";                           // Gateway execution script
	public static final String BLOCK_PROPERTY_STATE = "State";                            // Block state
	public static final String BLOCK_PROPERTY_SUBWORKSPACE = "Subworkspace";              // UUID of subworkspace, encapsulation
	public static final String BLOCK_PROPERTY_TYPE = "BlockType";                         // Block type
	
	// These are the property names in the message payload, gateway to designer
	public static final String MSG_BLOCK_NAME      = "BlockName";
	public static final String MSG_BLOCK_STATE     = "BlockState";
	public static final String MSG_WORKSPACE_ID    = "WorkspaceID";    // UUID of the component's workspace
}
