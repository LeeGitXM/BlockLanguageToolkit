/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;

import com.ils.blt.common.block.ProcessBlock;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * A SignalNotification is an in-bound message to a block that contains
 * a signal object as its data. The block must be typed as a signal receptor.
 * There is no relationship to connections.
 * 
 * This is a property container with no behavior.
 */
public class SignalNotification {
	private final ProcessBlock block;
	private final QualifiedValue value;
	
	/**
	 * Constructor. The signal is the only property.
	 * 
	 * @param blk the addressee of the notification
	 * @param qv a QualifiedValue containing the signal to be delivered to the block.
	 */
	public SignalNotification(ProcessBlock blk,QualifiedValue qv)  {
		this.block = blk;
		this.value = qv;
	}
	
	public ProcessBlock getBlock() {return block;}
	public Signal getSignal() {return (Signal)value.getValue();}
	public QualifiedValue getValue() {return value;}
}
