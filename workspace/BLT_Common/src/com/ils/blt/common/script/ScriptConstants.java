/**
 *   (c) 2015,2021 ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;


/**
 * These are constants dealing with the scripting extensions.
 *  
 */
public interface ScriptConstants   {
	// The applications and families exist in the NavTree and are not really 
	// block classes. Here are the "className" keys that we use for extensions
	public final static String APPLICATION_CLASS_NAME  = "com.ils.application";
	public final static String FAMILY_CLASS_NAME       = "com.ils.family";
	public final static String DIAGRAM_CLASS_NAME      = "com.ils.diagram";
	
	// These are the UDT paths 
	public final static String APPLICATION_TAG_PATH  = "Configuration/DiagnosticToolkit/ApplicationExtensions";
	public final static String FAMILY_TAG_PATH       = "Configuration/DiagnosticToolkit/FamilyExtensions";
	public final static String DIAGRAM_TAG_PATH      = "Configuration/DiagnosticToolkit/DiagramExtensions";
	
	// These are tag-paths within the UDT for each method type
	public final static String DELETE_OPERATION      = "Delete";
	public final static String GET_AUX_OPERATION     = "GetAux";
	public final static String GET_LIST_OPERATION    = "GetList";
	public final static String RENAME_OPERATION      = "Rename";
	public final static String SAVE_OPERATION        = "Save";
	public final static String SET_AUX_OPERATION     = "SetAux";
	
	// The arguments are fixed for each type of script
	public final static String DELETE_SCRIPT_ARGS      = "uuid";
	public final static String GET_AUX_SCRIPT_ARGS     = "uuid,properties,db";
	public final static String GET_LIST_SCRIPT_ARGS    = "key,db";
	public final static String RENAME_SCRIPT_ARGS      = "uuid,oldName,newName";
	public final static String SAVE_SCRIPT_ARGS        = "uuid";
	public final static String SET_AUX_SCRIPT_ARGS     = "uuid,properties,db";

	// For the GetList extension function, these are the known keys
	public final static String LIST_KEY_GROUP_RAMP         = "GroupRamp";
	public final static String LIST_KEY_MESSAGE_QUEUE      = "MessageQueue";
	public final static String LIST_KEY_UNIT               = "Unit";
}
