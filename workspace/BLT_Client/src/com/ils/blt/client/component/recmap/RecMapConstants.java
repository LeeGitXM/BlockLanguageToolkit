/**
 *   (c) 2014-2016 ILS Automation. All rights reserved. 
 */
package com.ils.blt.client.component.recmap;

/**
 *  These are constants used for the RecommendationMap
 */
public interface RecMapConstants   {   
	// Default display size
	public static final int ZOOM_DURATION = 2000;   // ~ msecs
	// These are the property names for the bean infos";
	public static final String CONNECTIONS_PROPERTY        = "connections";
	public static final String DIAGNOSES_PROPERTY          = "diagnoses";
	public static final String OUTPUTS_PROPERTY            = "outputs";
	public static final String RECOMMENDATIONS_PROPERTY    = "recommendations";
	
	// Dataset column names
	public static final String DIAGNOSIS_ID_COLUMN     = "DiagnosisId";
	public static final String OUTPUT_ID_COLUMN        = "OutputId";
	public static final String ID_COLUMN     = "Id";
	public static final String NAME_COLUMN    = "Name";
	
	// VisualItem column names
	public static final String KIND    = "Kind";       // diagnosis, recommendation, output
	public static final String ID       = "Id";
	public static final String INDEX    = "Index";     // Row of the dataset
	public static final String NAME     = "Name";
	public static final String ROW      = "Row";
	public static final String SOURCEROW  = "SourceRef";
	public static final String TARGETROW  = "TargetRef";
	public static final String ACTIVE     = "Active";        // Connection
	public static final String AUTO       = "Auto";          // Recomendations
	public static final String CURRENT    = "CurrentSetpoint"; // Output
	public static final String FINAL      = "FinalSetpoint"; // Output
	public static final String MULTIPLIER = "Multiplier";    // Diagnosis
	public static final String PROBLEM    = "Problem";       // Diagnosis
	public static final String RECOMMENDATION = "Recommendation"; // Output
	public static final String TARGET     = "Target";        // Output
	
	// "kinds" of nodes
	public static final int SOURCE_KIND      = 0;
	public static final int INFO_KIND        = 1;
	public static final int TARGET_KIND      = 2;
}
