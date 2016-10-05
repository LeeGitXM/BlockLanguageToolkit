/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;

import java.util.EventListener;



/**
 * Implementation of a PropertyChangeListener.
 */
public interface BlockPropertyChangeListener extends EventListener {

	//============================= PropertyChangeListener ===========================
	/**
	 * This is a stricter implementation that enforces QualifiedValue data
	 * @param event contains the new change infofrmation
	 */
	public void propertyChange(BlockPropertyChangeEvent event);
}