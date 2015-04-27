/**
 *   (c) 2015 ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;


/**
 *  These are constants dealing with the scripting extensions
 */
public interface ScriptConstants   {
	// The applications and families exist in the NavTree and are not really 
	// block classes. Here are the "className" keys that we use for extensions
	public final static String APPLICATION_CLASS_NAME  = "com.ils.application";
	public final static String FAMILY_CLASS_NAME       = "com.ils.family";
	// There are a pair of scripts for each class with extensions. 
	public final static String PROPERTY_GET_SCRIPT     = "property-get-script";
	public final static String PROPERTY_RENAME_SCRIPT  = "property-rename-script";
	public final static String PROPERTY_SET_SCRIPT     = "property-set-script";

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
