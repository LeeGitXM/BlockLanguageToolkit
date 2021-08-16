/**
  *   (c) 2014-2020  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;

import java.util.EventListener;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;


/**
 * A listener for push notifications. The listener is expected to be in either of the Client
 * or Designer scopes. The event might trigger a UI change, but not directly. 
 */
public interface NotificationChangeListener extends EventListener  {

	//============================= NotificationChangeListener ===========================
	/**
	 * A diagram alert state has changed
	 * @param resourceId the resource Id of the affected diagram
	 * @param state the state of the reporting block
	 */
	public void diagramStateChange(ProjectResourceId resourceId,String state);
	/**
	 * A property binding has changed
	 */
	public void bindingChange(String name,String binding);
	/**
	 * The name has changed
	 */
	public void nameChange(String name);
	/**
	 * Use this for a block listening on property value changes
	 */
	public void propertyChange(String name,Object value);
	/**
	 * Just send the new value.
	 */
	public void valueChange(QualifiedValue value);
	/**
	 * Watermark has changed. This is currently used only for diagrams,
	 * but could conceivably be used for block marquees.
	 */
	public void watermarkChange(String newWatermark);
}