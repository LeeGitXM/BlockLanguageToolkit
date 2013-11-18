/**
 *   (c) 2013 ILS Automation. All rights reserved.
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
	
	public final static String FOLDER_RESOURCE_TYPE    = "__folder";
	public final static String MODEL_RESOURCE_TYPE    = "blt.model";
	public final static String PANEL_RESOURCE_TYPE    = "blt.panel";
	
	public final static String REPORTING_SCRIPT_PACKAGE  = "system.ils.blt.report";
	public final static String PROPERTIES_SCRIPT_PACKAGE = "system.ils.blt.properties";
	public final static String GRAPHICS_SCRIPT_PACKAGE = "system.ils.ui";
	
	/** This unique ID represents the root node in the project tree */
	public static final UUID ROOT_FOLDER_UUID = UUID
			.fromString("7aaad6b9-3140-4328-a844-51817eb47574");
}
