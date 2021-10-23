/**
 *   (c) 2013-2021  ILS Automation. All rights reserved.
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
	// Note that send and receive ports are not affected by type change actions
	public static final String DEFAULT_BLOCK_NAME = ".";         // Name set when block is created. Should be overridden
	public static final String IN_PORT_NAME    = "in";
	public static final String OUT_PORT_NAME = "out";
	public static final String BROADCAST_PORT_NAME = "send";     // Send
	public static final String CONTROL_PORT_NAME   = "control";  // Control line
	public static final String RECEIVER_PORT_NAME   = "recv";     // Receive
	public static final String SIGNAL_PORT_NAME    = "signal";   // Signal (every block has one)
		
	// These are block property names used specially for attribute displays
	public static final String ATTRIBUTE_PROPERTY_BACKGROUND_COLOR  = "BackgroundColor";
	public static final String ATTRIBUTE_PROPERTY_BLOCK_ID    		= "BlockId";
	public static final String ATTRIBUTE_PROPERTY_FONT_SIZE        	= "FontSize";
	public static final String ATTRIBUTE_PROPERTY_FOREGROUND_COLOR  = "ForegroundColor";
	public static final String ATTRIBUTE_PROPERTY_HEIGHT        	= "Height";
	public static final String ATTRIBUTE_PROPERTY_FORMAT        	= "Format";
	public static final String ATTRIBUTE_PROPERTY_OFFSET_X        	= "OffsetX";
	public static final String ATTRIBUTE_PROPERTY_OFFSET_Y        	= "OffsetY";
	public static final String ATTRIBUTE_PROPERTY_PROPERTY    		= "Property";

	public static final String ATTRIBUTE_PROPERTY_VALUE    			= "Value";
	public static final String ATTRIBUTE_PROPERTY_WIDTH        		= "Width";
	// These are block property names that used in multiple block definitions
	public static final String BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE 	= "ActivityBufferSize";
	public static final String BLOCK_PROPERTY_BACKGROUND_COLOR     	= "BackgroundColor";
	public static final String BLOCK_PROPERTY_CLEAR_ON_RESET       	= "ClearOnReset";
	public static final String BLOCK_PROPERTY_COMMAND              	= "Command";
	public static final String BLOCK_PROPERTY_DEADBAND             	= "Deadband";
	public static final String BLOCK_PROPERTY_DISTRIBUTION  		= "Distribution";
	public static final String BLOCK_PROPERTY_EXPIRATION_TIME 		= "ExpirationTime";  // System time ~ msecs
	public static final String BLOCK_PROPERTY_FILL_REQUIRED 		= "FillRequired";
	public static final String BLOCK_PROPERTY_FORMAT        		= "Format";
	public static final String BLOCK_PROPERTY_HEIGHT        		= "Height";
	public static final String BLOCK_PROPERTY_PREFIX        		= "Prefix";
	public static final String BLOCK_PROPERTY_SUFFIX        		= "Suffix";
	public static final String BLOCK_PROPERTY_HYSTERESIS    		= "Hysteresis";
	public static final String BLOCK_PROPERTY_INHIBIT_INTERVAL 		= "InhibitInterval";  // Inhibit period ~ sec
	public static final String BLOCK_PROPERTY_INITIAL_VALUE 		= "InitialValue";
	public static final String BLOCK_PROPERTY_INSTANCE      		= "Instance"; 
	public static final String BLOCK_PROPERTY_INTERVAL      		= "Interval";
	public static final String BLOCK_PROPERTY_LABEL         		= "Label";
	public static final String BLOCK_PROPERTY_LIMIT         		= "Limit";
	public static final String BLOCK_PROPERTY_LIMIT_TYPE    		= "LimitType";
	public static final String BLOCK_PROPERTY_MEAN          		= "Mean";
	public static final String BLOCK_PROPERTY_NAME          		= "Name";
	public static final String BLOCK_PROPERTY_OFFSET        		= "Offset";
	public static final String BLOCK_PROPERTY_PROPERTY      		= "Property";            // Property name
	public static final String BLOCK_PROPERTY_SAMPLE_SIZE   		= "SampleSize";
	public static final String BLOCK_PROPERTY_SCOPE         		= "Scope";	
	public static final String BLOCK_PROPERTY_SCAN_INTERVAL    		= "ScanInterval";     // Compute interval ~ sec
	public static final String BLOCK_PROPERTY_SCALE_FACTOR     		= "ScaleFactor";	
	public static final String BLOCK_PROPERTY_STANDARD_DEVIATION 	= "StandardDeviation";
	public static final String BLOCK_PROPERTY_STATISTICS_FUNCTION 	= "Function";
	public static final String BLOCK_PROPERTY_NAME_VALUES     		= "Name:Values";     // List of name:value pairs
	public static final String BLOCK_PROPERTY_SYNC_INTERVAL    		= "SyncInterval";     // Time to coalesce inputs ~ sec
	public static final String BLOCK_PROPERTY_TAG_PATH         		= "TagPath";
	public static final String BLOCK_PROPERTY_TEXT             		= "Text";
	public final static String BLOCK_PROPERTY_THRESHOLD    	   		= "Threshold";
	public static final String BLOCK_PROPERTY_TIME_WINDOW      		= "TimeWindow";   // time window in minutes
	public static final String BLOCK_PROPERTY_TRIGGER          		= "Trigger";
	public static final String BLOCK_PROPERTY_TRIGGER_COUNT    		= "TriggerCount";
	public static final String BLOCK_PROPERTY_VALUE 				= "Value";                       // Current value
	public static final String BLOCK_PROPERTY_WIDTH 				= "Width";
	
	// Use this for notifications for all properties in the block
	public static final String BLOCK_PROPERTY_ALL 	= "*";
	
	// These are valid block data types
	public static final String BLOCK_TYPE_SCRIPT        = "script";  // Python module path
	public static final String BLOCK_TYPE_STRING        = "string";
	public static final String BLOCK_TYPE_TAG           = "tag";     // FUlly qualified tag path
	
	// These are valid/required properties for ports
	public static final String PORT_NAME                = "name";
	public static final String PORT_TYPE                = "type";   // datatype for a port
	
	// These are block classes with special Handling
	public static final String BLOCK_CLASS_ATTRIBUTE    = "com.ils.block.AttributeDisplay"; 
	public static final String BLOCK_CLASS_INPUT     	= "com.ils.block.Input"; 
	public static final String BLOCK_CLASS_OUTPUT     	= "com.ils.block.Output"; 
	public static final String BLOCK_CLASS_SINK     	= "com.ils.block.SinkConnection"; 
	public static final String BLOCK_CLASS_SOURCE     	= "com.ils.block.SourceConnection"; 
	
	// Special folder to contain binding tags for Source/Sink blocks
	public static final String SOURCE_SINK_TAG_FOLDER                  = "DiagnosticToolkit/Connections"; 
	
	// These are standard connection property names
	public static final String CONNECTION_PROPERTY_DOWNSTREAM_PORT     = "downstream"; 
	public static final String CONNECTION_PROPERTY_QUALITY             = "quality";
	public static final String CONNECTION_PROPERTY_UPSTREAM_PORT       = "upstream"; 
	public static final String CONNECTION_PROPERTY_VALUE               = "value"; 

	
	// These are standard palette tab names - do not use names with embedded spaces
	public static final String PALETTE_TAB_ARITHMETIC         = "Arithmetic";
	public static final String PALETTE_TAB_ANALYSIS           = "Analysis";
	public static final String PALETTE_TAB_CONNECTIVITY       = "Connectivity";
	public static final String PALETTE_TAB_CONTROL            = "Control";
	public static final String PALETTE_TAB_CONCLUSION         = "Conclusion";
	public static final String PALETTE_TAB_INFERENCE          = "Inference";
	public static final String PALETTE_TAB_LOGIC              = "Logic";
	public static final String PALETTE_TAB_NONE               = "";     // These blocks will not be displayed
	public static final String PALETTE_TAB_OBSERVATION        = "Observation";
	public static final String PALETTE_TAB_STATISTICS         = "Statistics";
	public static final String PALETTE_TAB_TIMERS_COUNTERS    = "Counters";
	public static final String PALETTE_TAB_MISC               = "Misc";
	
	// Attribute Display Dimensions and Positions
	public static final int ATTRIBUTE_DISPLAY_OFFSET_X     = 10;  // From the bottom of the block
	public static final int ATTRIBUTE_DISPLAY_OFFSET_Y     = 30;
	public static final int ATTRIBUTE_DISPLAY_HEIGHT     = 25;
	public static final int ATTRIBUTE_DISPLAY_WIDTH      = 180;
	// Block filler colors
	public static final int BLOCK_BACKGROUND_BLUE_GRAY      = (new Color(143,172,183)).getRGB();
	public static final int BLOCK_BACKGROUND_GRAYISH        = (new Color(208,215,220)).getRGB();
	public static final int BLOCK_BACKGROUND_LIGHT_ROSE     = (new Color(220,208,200)).getRGB();
	public static final int BLOCK_BACKGROUND_LIGHT_GRAY     = (new Color(240,240,240)).getRGB();
	public static final int BLOCK_BACKGROUND_MUSTARD     	= (new Color(248,229,34)).getRGB();
	
	// These are well-known commands
	public static final String COMMAND_CLEAR_LOW               	= "CLEAR_LOWER_SQC";
	public static final String COMMAND_CLEAR_HIGH              	= "CLEAR_UPPER_SQC";
	public static final String COMMAND_CONFIGURE                = "CONFIGURE";
	public static final String COMMAND_EVALUATE                 = "EVALUATE";
	public static final String COMMAND_INHIBIT                 	= "INHIBIT";
	public static final String COMMAND_LOCK                 	= "LOCK";
	public static final String COMMAND_RESET                   	= "RESET";
	public static final String COMMAND_START                   	= "START";
	public static final String COMMAND_UNLOCK                 	= "UNLOCK";	
}
