/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.control;

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
	 * Constructor. Value is expressed as a QualifiedValue
	 * @param source the block Id of the change originator
	 * @param propertyName
	 * @param oldValue
	 * @param newValue
	 */
	public BlockPropertyChangeEvent(String source, String propertyName, QualifiedValue oldValue, QualifiedValue newValue)  {	
		super(source,propertyName,oldValue,newValue);
	}
	
	/**
	 * Constructor. This constructor is a shortcut for push notification listeners in the Designer/client
	 * @param newValue
	 */
	public BlockPropertyChangeEvent(QualifiedValue newValue)  {	
		super("engine","",null,newValue);
	}
	
	public QualifiedValue getOldValue() { return (QualifiedValue)super.getOldValue(); }
	public QualifiedValue getNewValue() { return (QualifiedValue)super.getNewValue(); }
	
}
