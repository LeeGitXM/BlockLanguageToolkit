/**
 *   (c) 2014 ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.client.component;



/**
 *  These are constants used for the RecommendationMap
 */
public interface RecommendationConstants   {   
	// These are the property names for the bean infos";
	public static final String DIAGNOSES_PROPERTY          = "diagnoses";
	public static final String OUTPUTS_PROPERTY            = "outputs";
	public static final String RECOMMENDATIONS_PROPERTY    = "recommendations";
	
	// Dataset column names
	public static final String DIAGNOSIS_ID_COLUMN     = "Diagnosis";
	public static final String OUTPUT_ID_COLUMN        = "Output";
	public static final String ID_COLUMN     = "Id";
	public static final String NAME_COLUMN    = "Name";
	public static final String VALUE_COLUMN   = "Value";
	
	// VisualItem column names
	public static final String KIND    = "Kind";       // diagnosis, recommendation, output
	public static final String ID       = "Id";
	public static final String NAME     = "Name";
	public static final String ROW      = "Row";
	public static final String SOURCEROW  = "SourceRef";
	public static final String TARGETROW  = "TargetRef";
	public static final String VALUE    = "Value";      // For recommendations, the factor
}
