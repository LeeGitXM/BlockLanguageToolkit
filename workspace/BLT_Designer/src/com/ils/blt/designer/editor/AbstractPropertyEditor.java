/**
 *   (c) 2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.designer.editor;

import com.inductiveautomation.ignition.client.util.gui.SlidingPane;

/**
 * This abstract class, adds a shutdown method to a SlidingPane (a JPanel) used as content for the property editor
 * frame in the designer.
 */
public abstract class AbstractPropertyEditor extends SlidingPane  {	
	/**
	 * Stop any current processing.
	 */
	abstract public void shutdown();
}