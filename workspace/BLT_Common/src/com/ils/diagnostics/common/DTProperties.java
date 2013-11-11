/**
 *   (c) 2012  ILS Automation. All rights reserved.
 *  
 *   The tag factory is designed to be called from the client
 *   via RPC.
 */
package com.ils.diagnostics.common;

import java.util.UUID;


/**
 *  Define an interface for accessing module properties .
 */
public interface DTProperties   {   
	public final static String MODULE_ID = "diagnostics";     // See module-diag.xml
	public final static String MODULE_NAME = "DT";            // See build-jgx.xml
	
	public final static String FOLDER_RESOURCE_TYPE    = "__folder";
	public final static String MODEL_RESOURCE_TYPE    = "diag.model";
	public final static String PANEL_RESOURCE_TYPE    = "diag.panel";
	
	public final static String REPORTING_SCRIPT_PACKAGE  = "system.ils.diag.report";
	public final static String PROPERTIES_SCRIPT_PACKAGE = "system.ils.diag.properties";
	public final static String GRAPHICS_SCRIPT_PACKAGE = "system.ils.ui";
	
	/** This unique ID represents the root node in the project tree */
	public static final UUID ROOT_FOLDER_UUID = UUID
			.fromString("7aaad6b9-3140-4328-a844-51817eb47574");
}
