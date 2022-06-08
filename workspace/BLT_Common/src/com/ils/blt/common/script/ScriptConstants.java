/**
 *   (c) 2022 ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;


/**
 * These are hard-coded constants for dealing with the scripting notification.
 *  
 */
public interface ScriptConstants   {


	public final static String NOTIFICATION_MODULE_PATH      = "ils.diagToolkit.notifications.notify";
	
	// These are tag-paths within the UDT for each method type
	public final static String DELETE_NOTIFICATION      = "delete";
	public final static String RENAME_NOTIFICATION      = "rename";
	public final static String SAVE_NOTIFICATION        = "save";
	
	// The arguments are fixed for each type of script
	public final static String DELETE_SCRIPT_ARGS      = "path";
	public final static String RENAME_SCRIPT_ARGS      = "oldPath,newPath";
	public final static String SAVE_SCRIPT_ARGS        = "path,json";
}
