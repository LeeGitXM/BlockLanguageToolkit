/**
 *   (c) 2014-2016 ILS Automation. All rights reserved. 
 */
package com.ils.common.component.recmap;

import java.awt.Dimension;

import prefuse.util.ColorLib;

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
	public static final String NAME_COLUMN    = "Name";
	
	// VisualItem column names
	public static final String KIND    = "Kind";       // diagnosis, recommendation, output
	public static final String DSROW    = "DsRow";     // Row of the dataset
	public static final String NAME     = "Name";
	public static final String ROW      = "Row";       // Number of the node
	public static final String SOURCEROW  = "SourceRef";
	public static final String TARGETROW  = "TargetRef";
	public static final String ACTIVE     = "Active";        // Connection    - boolean
	public static final String AUTO       = "Auto";          // Recomendation - double
	public static final String CURRENT    = "CurrentSetpoint"; // Output    - double
	public static final String FINAL      = "FinalSetpoint"; // Output      - double
	public static final String HAS_SQC    = "HasSQC";        // Diagnosis/Output - boolean
	public static final String IS_AUTO    = "AutoOrManual";  // Recommendation   - string (auto/manual)
	public static final String MANUAL     = "Manual";        // Recomendation - double
	public static final String MULTIPLIER = "Multiplier";    // Diagnosis   - double
	public static final String PROBLEM    = "Problem";       // Diagnosis   - string
	public static final String RECOMMENDATION = "Recommendation"; // Output - double
	public static final String TARGET     = "Target";        // Output      - string
	
	// "kinds" of nodes
	public static final int SOURCE_KIND      = 0;
	public static final int INFO_KIND        = 1;
	public static final int TARGET_KIND      = 2;
	
	public static final Dimension BADGE_SIZE = new Dimension(20,40);
	
	// Header colors of nodes
	public static final int SOURCE_HEADER_COLOR = ColorLib.rgb(151,230,250);
	public static final int INFO_HEADER_COLOR   = ColorLib.rgb(153,235,118);
	public static final int TARGET_HEADER_COLOR = ColorLib.rgb(242,241,213);
	
}
