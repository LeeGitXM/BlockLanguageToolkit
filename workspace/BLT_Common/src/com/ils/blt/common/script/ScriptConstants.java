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
	public final static String APP_ADD_SCRIPT     = "app-add-script";
	public final static String APP_CLONE_SCRIPT   = "app-clone-script"; 
	public final static String APP_DELETE_SCRIPT  = "app-delete-script"; 
	public final static String APP_GET_AUX_SCRIPT = "app-get-aux-script"; 
	public final static String APP_SET_AUX_SCRIPT = "app-set-aux-script"; 
	public final static String APP_UPDATE_SCRIPT  = "app-update-script"; 
	
	public final static String FAM_ADD_SCRIPT     = "fam-add-script";
	public final static String FAM_CLONE_SCRIPT   = "fam-clone-script"; 
	public final static String FAM_DELETE_SCRIPT  = "fam-delete-script"; 
	public final static String FAM_GET_AUX_SCRIPT = "fam-get-aux-script"; 
	public final static String FAM_SET_AUX_SCRIPT = "fam-set-aux-script"; 
	public final static String FAM_UPDATE_SCRIPT  = "fam-update-script";
	// These scripts are compiled with each use ...
	public final static String GENERIC_PROPERTY_GET_SCRIPT  = "property-get-script";
	public final static String GENERIC_PROPERTY_SET_SCRIPT  = "property-set-script";

	// These are the keys for individual sub-dictionaries
	public final static String ARGS_KEY   = "arglist";
	public final static String ENTRY_KEY  = "entry";
	public final static String SCRIPT_KEY = "script";
	
	// These are standard names of properties
	public static final String PROPERTY_NAME        = "Name";
	public final static String PROPERTY_PRIORITY    = "priority";
	
	// These are properties of a final diagnosis (it also has a priority)
	public final static String PROPERTY_CALCULATION_METHOD    = "calculationMethod";
	public final static String PROPERTY_EXPLANATION    = "explanation";
	public final static String PROPERTY_POST_TEXT_RECOMMENDATION    = "postTextRecommendation";
	public final static String PROPERTY_REFRESH_RATE    = "refreshRate";
	public final static String PROPERTY_TEXT_RECOMMENDATION    = "textRecommendation";
	public final static String PROPERTY_TEXT_RECOMMENDATION_CALLBACK    = "textRecommendationCallback";
	public final static String PROPERTY_TRAP_INSIGNITFICANT_RECOMMENDATIONS    = "trapInsignificantRecommendations";
}
