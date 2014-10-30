/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Dimension;



/**
 *  These constants refer to the order of individual panes
 *  in the block editor's sliding pane.
 */
public interface BlockEditConstants   {
	// These are the panels in the set of sliding screens
	public static int HOME_PANEL          = 0;
	public static int CONFIGURATION_PANEL = 1;
	public static int LIST_EDIT_PANEL     = 2;
	public static int NAME_EDIT_PANEL     = 3;
	public static int TAG_BROWSER_PANEL   = 4;
	
	// Standard sizes
	public static final Dimension BUTTON_SIZE = new Dimension(16,16);
	public static final Dimension COMBO_BOX_SIZE  = new Dimension(120,24);
	public static final Dimension ENTRY_BOX_SIZE  = new Dimension(160,24);
	public static final Dimension OFFSET_BOX_SIZE  = new Dimension(40,24);
	public static final Dimension TREE_SIZE        = new Dimension(300,120);
	public static final Dimension TABLE_SIZE       = new Dimension(300,120);
	
}
