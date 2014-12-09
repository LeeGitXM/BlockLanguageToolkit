/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 *   Class contains static constants that regulate appearance of the 
 *   block and connections.
 */
package com.ils.blt.designer.workspace;

import java.awt.Color;


/**
 *  Constants used in drawing components of the workspace.
 */
public interface WorkspaceConstants   {
	
	// Connection widths. In each case the border is a single pixel.
	// The "informational" path has a black line down its center.
	// These seem to look better with odd pixel counts for the width
	public static final int CONNECTION_WIDTH_CENTERLINE     = 1;
	public static final int CONNECTION_WIDTH_SIGNAL         = 2;  
	public static final int CONNECTION_WIDTH_TRUTHVALUE     = 5;
	public static final int CONNECTION_WIDTH_DATA           = 7;
	public static final int CONNECTION_WIDTH_TEXT           = 9;
	
	public static final Color CONNECTION_BACKGROUND           = Color.BLACK;
	public static final Color CONNECTION_HOVER                = new Color(255,255,40);   // Yellow
	public static final Color CONNECTION_SELECTED             = Color.MAGENTA;
	public static final Color CONNECTION_FILL_BAD             = Color.YELLOW;
	public static final Color CONNECTION_FILL_DATA            = Color.LIGHT_GRAY;
	public static final Color CONNECTION_FILL_EMPTY           = new Color(230,230,230);   // Mercury
	public static final Color CONNECTION_FILL_FALSE           = new Color(175,0,0);       // Dark red
	public static final Color CONNECTION_FILL_SIGNAL          = Color.BLACK;
	public static final Color CONNECTION_FILL_TRUE            = new Color(0,128,0);       // Clover
	public static final Color CONNECTION_FILL_TEXT            = new Color(230,200,55);    // Mustard
	public static final Color CONNECTION_FILL_UNKNOWN         = new Color(76,76,76);      // Iron
	
	public static final int DEFAULT_EMBEDDED_FONT_SIZE    = 24;
}
