/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.control;

import java.util.EventListener;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;



/**
 * The listener is expected to be in either of the Client or Designer scopes.
 * The event might trigger a UI change, but not directly. 
 */
public interface NotificationChangeListener extends EventListener  {

	//============================= NotificationChangeListener ===========================
	/**
	 * Just send the new value.
	 */
	public void valueChange(QualifiedValue value);
}