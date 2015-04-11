/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.common.block;

import java.awt.Color;


/**
 *  Global constants for blocks and connection properties. These are
 *  collected in the common jar to prevent mutual dependencies 
 *  in otherwise unrelated projects.
 */
public interface BlockConstants   {
	public static final String TIMESTAMP_FORMAT = "yyyy.MM.dd HH:mm:ss.SSS";
	public static final long UNKNOWN = -1;
	
	// Use these when there is only a single input and/or output
	public static final String IN_PORT_NAME    = "in";
	public static final String OUT_PORT_NAME = "out";
	public static final String CONTROL_PORT_NAME   = "recv";     // Receive
	public static final String BROADCAST_PORT_NAME = "send";     // Send
	public static final String SIGNAL_PORT_NAME    = "signal";   // Signal (every block has one)
		
	// These are block property names that used in multiple block definitions
	public static final String BLOCK_PROPERTY_CLEAR_ON_RESET = "ClearOnReset";
	public static final String BLOCK_PROPERTY_COMMAND       = "Command";
	public static final String BLOCK_PROPERTY_DEADBAND      = "Deadband";
	public static final String BLOCK_PROPERTY_DISTRIBUTION  = "Distribution";
	public static final String BLOCK_PROPERTY_FILL_REQUIRED = "FillRequired";
	public static final String BLOCK_PROPERTY_FORMAT        = "Format";
	public static final String BLOCK_PROPERTY_GET_AUX_DATA_HOOK  = "GetAuxiliaryDataHook";
	public static final String BLOCK_PROPERTY_HEIGHT        = "Height";
	public static final String BLOCK_PROPERTY_HYSTERESIS    = "Hysteresis";
	public static final String BLOCK_PROPERTY_INHIBIT_INTERVAL = "InhibitInterval";  // Inhibit period ~ sec
	public static final String BLOCK_PROPERTY_INSTANCE      = "Instance"; 
	public static final String BLOCK_PROPERTY_INTERVAL      = "Interval";
	public static final String BLOCK_PROPERTY_LABEL         = "Label";
	public static final String BLOCK_PROPERTY_LIMIT         = "Limit";
	public static final String BLOCK_PROPERTY_LIMIT_TYPE    = "LimitType";
	public static final String BLOCK_PROPERTY_MEAN          = "Mean";
	public static final String BLOCK_PROPERTY_OFFSET        = "Offset";
	public static final String BLOCK_PROPERTY_SAMPLE_SIZE   = "SampleSize";
	public static final String BLOCK_PROPERTY_SCOPE         = "Scope";	
	public static final String BLOCK_PROPERTY_SCAN_INTERVAL    = "ScanInterval";     // Compute interval ~ sec
	public static final String BLOCK_PROPERTY_SET_AUX_DATA_HOOK  = "SetAuxiliaryDataHook";
	public static final String BLOCK_PROPERTY_STANDARD_DEVIATION = "StandardDeviation";
	public static final String BLOCK_PROPERTY_SYNC_INTERVAL    = "SyncInterval";     // Time to coalesce inputs ~ sec
	public static final String BLOCK_PROPERTY_TAG_PATH = "TagPath";
	public static final String BLOCK_PROPERTY_TEXT             = "Text";
	public static final String BLOCK_PROPERTY_TIME_WINDOW      = "TimeWindow";
	public static final String BLOCK_PROPERTY_TRIGGER          = "Trigger";
	public static final String BLOCK_PROPERTY_TRIGGER_COUNT    = "TriggerCount";
	public static final String BLOCK_PROPERTY_VALUE = "Value";                       // Current value
	public static final String BLOCK_PROPERTY_WIDTH = "Width";
	
	
	// These are valid block data types
	public static final String BLOCK_TYPE_SCRIPT        = "script";  // Python module path
	public static final String BLOCK_TYPE_STRING        = "string";
	public static final String BLOCK_TYPE_TAG           = "tag";     // FUlly qualified tag path
	
	// These are valid/required properties for ports
	public static final String PORT_NAME                = "name";
	public static final String PORT_TYPE                = "type";   // datatype for a port
	
	// These are standard connection property names
	public static final String CONNECTION_PROPERTY_DOWNSTREAM_PORT     = "downstream"; 
	public static final String CONNECTION_PROPERTY_QUALITY             = "quality";
	public static final String CONNECTION_PROPERTY_UPSTREAM_PORT       = "upstream"; 
	public static final String CONNECTION_PROPERTY_VALUE               = "value"; 
	
	// These are standard palette tab names
	public static final String PALETTE_TAB_ARITHMETIC         = "Arithmetic";
	public static final String PALETTE_TAB_ANALYSIS           = "Analysis";
	public static final String PALETTE_TAB_CONNECTIVITY       = "Connectivity";
	public static final String PALETTE_TAB_CONTROL            = "Control";
	public static final String PALETTE_TAB_LOGIC              = "Logic";
	public static final String PALETTE_TAB_OBSERVATION        = "Observation";
	public static final String PALETTE_TAB_STATISTICS         = "Statistics";
	public static final String PALETTE_TAB_MISC         = "Misc";
	
	// Default attribute offsets
	public static final int DEFAULT_ATTRIBUTE_OFFSET_X     = 30;
	public static final int DEFAULT_ATTRIBUTE_OFFSET_Y     = 75;
	// Block filler colors
	public static final int BLOCK_BACKGROUND_GRAYISH        = (new Color(208,215,220)).getRGB();
	public static final int BLOCK_BACKGROUND_LIGHT_ROSE     = (new Color(220,208,200)).getRGB();
	public static final int BLOCK_BACKGROUND_LIGHT_GRAY     = (new Color(240,240,240)).getRGB();
	
	// These are well-known commands
	public static final String COMMAND_CLEAR_LOW               	= "CLEAR_LOWER_SQC";
	public static final String COMMAND_CLEAR_HIGH              	= "CLEAR_UPPER_SQC";
	public static final String COMMAND_EVALUATE                 = "EVALUATE";
	public static final String COMMAND_INHIBIT                 	= "INHIBIT";
	public static final String COMMAND_LOCK                 	= "LOCK";
	public static final String COMMAND_RESET                   	= "RESET";
	public static final String COMMAND_START                   	= "START";
	public static final String COMMAND_UNLOCK                 	= "UNLOCK";
	
	
}
