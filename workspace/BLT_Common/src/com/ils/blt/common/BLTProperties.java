/**
 *   (c) 2014 ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.awt.Color;
import java.util.UUID;


/**
 *  Define an interface for accessing module properties. We have constants for all the known "flavors" here.
 */
public interface BLTProperties   {   
	public final static String CLASSIC_MODULE_ID = "block";              // See module-blt-classic.xml
	public final static String CLASSIC_MODULE_NAME = "BLT-Classic";      // See build-blt-classic.xml
	public final static String SCHEMATIC_MODULE_ID = "schematic-block";  // See module-blt-schematic.xml
	public final static String SCHEMATIC_MODULE_NAME = "BLT-Schematic";  // See build-blt-schematic.xml
	public final static String TEST_MODULE_ID = "mock-block";            // See module-blt-test.xml
	public final static String TEST_MODULE_NAME = "BLT-Test";            // See build-blt-test.xml
	public final static String SFC_MODULE_ID = "com.ils.sfc"; 
	
	/** This are the names of the jar files containing block class definitions */
	public final static String BLOCK_JAR_NAME = "block-definition";
	public final static String SCHEMATIC_BLOCK_JAR_NAME = "schematic-block-definition";
	
	public final static String APPLICATION_RESOURCE_TYPE   = "blt.application";
	public final static String CLASSIC_DIAGRAM_RESOURCE_TYPE = "blt.diagram";
	public final static String FAMILY_RESOURCE_TYPE        = "blt.family";
	public final static String FOLDER_RESOURCE_TYPE        = "__folder";
	public final static String SCHEMATIC_DIAGRAM_RESOURCE_TYPE = "blt.schematic.diagram";
	
	public final static String CLASSIC_SCRIPT_PACKAGE      = "system.ils.blt.diagram";
	public final static String SCHEMATIC_SCRIPT_PACKAGE    = "system.ils.blt.schematic";
	public final static String TEST_SCRIPT_PACKAGE         = "system.ils.blt.mock";
	
	/** This unique ID represents the root node in the project tree 
	 *  Note: The test system does not have a visible node.
	 */
	public static final UUID CLASSIC_ROOT_FOLDER_UUID = UUID
			.fromString("7bbbd6b9-3140-4328-a844-51817eb47574");
	public static final UUID SCHEMATIC_ROOT_FOLDER_UUID = UUID
			.fromString("7cccd6b9-3140-4328-a844-51817eb47574");
	public static final UUID TEST_ROOT_FOLDER_UUID = UUID
			.fromString("7dddd6b9-3140-4328-a844-51817eb47574");
	public static final String CLASSIC_ROOT_FOLDER_NAME   = "CROOT";
	public static final String SCHEMATIC_ROOT_FOLDER_NAME = "SROOT";
	public static final String TEST_ROOT_FOLDER_NAME      = "TROOT";
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
	public static final String BLOCK_ATTRIBUTE_PATH       = "path";
	public static final String BLOCK_ATTRIBUTE_QUALITY    = "quality";
	public static final String BLOCK_ATTRIBUTE_TARGET     = "target";
	public static final String BLOCK_ATTRIBUTE_DATA_TYPE  = "type";
	public static final String BLOCK_ATTRIBUTE_VALUE      = "value";
	
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
	public static final String PALETTE_RECEIVE_ENABLED   = "receiveEnabled";
	public static final String PALETTE_TRANSMIT_ENABLED   = "transmitEnabled";
	public static final String PALETTE_TOOLTIP     = "tooltip";
	public static final String PALETTE_TAB_NAME    = "tabName";
	public static final String PALETTE_VIEW_BACKGROUND = "viewBackgroundColor";   // RGB as an int
	public static final String PALETTE_VIEW_BLOCK_ICON  = "viewBlockIcon";
	public static final String PALETTE_VIEW_FONT_SIZE  = "viewFontSize";
	public static final String PALETTE_VIEW_HEIGHT  = "viewHeight";
	public static final String PALETTE_VIEW_ICON   = "viewIcon";
	public static final String PALETTE_VIEW_LABEL  = "viewLabel";
	public static final String PALETTE_VIEW_WIDTH  = "viewWidth";
	
	// Dialog Titles
	public static final String INTERFACE_MENU_TITLE  = "External Interface Configuration";
	public static final String VALIDATION_MENU_TITLE = "Validate Diagrams";
	
	// Name to use for "Good" Quality
	public static final String QUALITY_GOOD        = "Good";
	public static final String NOT_FOUND           = "NotFound";
	// Preferences keys
	public static final String PREFERENCES_NAME     = "BLTPreferences";    // Preferences collection name
	public static final String PREF_EXIM_DIRECTORY  = "ExImDirectory";     // Export/import directory
	public static final String PREF_CONFIG_DIRECTORY= "ConfigDirectory";   // Project properties configuration
	// These are the names of diagram configurable properties (specific instances saved in HSQLdb)
	public static final String DIAGRAM_PROPERTY_ROOT_NAME           = "RootName";       // Name of root node in nav tree
	public static final String DIAGRAM_PROPERTY_CLASSIC_ROOT_NAME   = "ClassicRootName";       // Name of root node in nav tree
	public static final String DIAGRAM_PROPERTY_SCHEMATIC_ROOT_NAME = "SchematicRootName";       // Name of root node in nav tree
	
	// These are the names of toolkit properties that are to be stored in HSQLdb
	public static final String TOOLKIT_PROPERTY_ACTIVE_BLOCKS       = "EnableActive";       // Enable active dialogs
	public static final String TOOLKIT_PROPERTY_COMPILED_BLOCKS     = "EnableCompiled";     // Enable schematic dialogs
	public static final String TOOLKIT_PROPERTY_DATABASE            = "Database";           // Production database
	public static final String TOOLKIT_PROPERTY_ENHANCED_TREE       = "EnhancedTree";       // Show Applications/Families
	public static final String TOOLKIT_PROPERTY_ISOLATION_DATABASE  = "SecondaryDatabase";  // Database when in isolation
	public static final String TOOLKIT_PROPERTY_PROVIDER            = "Provider";           // Production tag provider
	public static final String TOOLKIT_PROPERTY_ISOLATION_PROVIDER  = "SecondaryProvider";  // Tag provider when in isolation
	public static final String TOOLKIT_PROPERTY_ISOLATION_TIME      = "SecondaryTimeFactor";// Time speedup when in isolation
	
	
}
