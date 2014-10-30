/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;

import java.beans.PropertyChangeEvent;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is a thin extension of a PropertyChangeEvent that enforces
 * that the value type is a QualifiedValue.
 * 
 */
public class BlockPropertyChangeEvent extends PropertyChangeEvent {
	private static final long serialVersionUID = 6886769663284199568L;

	/**
	 * Constructor. Value is a simple object (not null,not a QualifiedValue)
	 * @param source the block Id of the change originator
	 * @param propertyName
	 * @param oldValue
	 * @param newValue
	 */
	public BlockPropertyChangeEvent(String source, String propertyName, Object oldValue, Object newValue)  {	
		super(source,propertyName,oldValue,newValue);
		if(newValue==null) throw new IllegalArgumentException("null property not allowed");
		if(newValue instanceof QualifiedValue) throw new IllegalArgumentException(String.format("Complex object %s not allowed",newValue.getClass().getName()));
	}
}
