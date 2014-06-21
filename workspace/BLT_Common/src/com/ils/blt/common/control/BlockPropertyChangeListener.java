/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.control;

import java.util.EventListener;



/**
 * This is a somewhat stricter version of a PropertyChangeListtner interface
 * that forces QualifiedValue types.
 */
public interface BlockPropertyChangeListener extends EventListener  {

	//============================= PropertyChangeListener ===========================
	/**
	 * This is a stricter implementation that enforces QualifiedValue data
	 */
	public void propertyChange(BlockPropertyChangeEvent event);
}