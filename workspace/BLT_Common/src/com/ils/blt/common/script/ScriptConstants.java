/**
 *   (c) 2015 ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;


/**
 *  These are constants dealing with the scripting extensions
 */
public interface ScriptConstants   {
	// Types of scripts
	public final static String APP_ADD_TYPE     = "app-add-type";
	public final static String APP_CLONE_TYPE   = "app-clone-type"; 
	public final static String APP_DELETE_TYPE  = "app-delete-type"; 
	public final static String APP_GET_AUX_TYPE = "app-get-aux-type"; 
	public final static String APP_SET_AUX_TYPE = "app-set-aux-type"; 
	public final static String APP_UPDATE_TYPE  = "app-update-type"; 
	
	public final static String FAM_ADD_TYPE     = "fam-add-type";
	public final static String FAM_CLONE_TYPE   = "fam-clone-type"; 
	public final static String FAM_DELETE_TYPE  = "fam-delete-type"; 
	public final static String FAM_GET_AUX_TYPE = "fam-get-aux-type"; 
	public final static String FAM_SET_AUX_TYPE = "fam-set-aux-type"; 
	public final static String FAM_UPDATE_TYPE  = "fam-update-type"; 

	// These are the keys for individual sub-dictionaries
	public final static String ARGS_KEY   = "arglist";
	public final static String ENTRY_KEY  = "entry";
	public final static String SCRIPT_KEY = "script";
}
