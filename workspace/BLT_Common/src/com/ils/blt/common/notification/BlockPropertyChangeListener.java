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
	 */
	public void propertyChange(BlockPropertyChangeEvent event);
}