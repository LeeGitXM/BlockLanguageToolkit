/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;

import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.Activity;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is used to hold change information representing 
 * a new object placed on an output port of a block. Depending 
 * on the type of connector, the class of the object is one
 * of the following:
 *    - Signal
 *    - TruthValue
 *    - QualifiedValue
 *    - String
 * 
 * This is a property container with no behavior.
 */
public class OutgoingNotification {
	private final ProcessBlock block;
	private final String port;
	private final QualifiedValue value;
	
	/**
	 * Constructor. Value is expressed as a QualifiedValue. 
	 *              Log creation of the notification in the block's 
	 *              history.
	 * 
	 * @param blk the block that is the source of the value.
	 * @param prt the output port on which the value was placed.
	 * @param val the new value
	 */
	public OutgoingNotification(ProcessBlock blk,String prt, QualifiedValue val)  {	
		this.block = blk;
		this.port = prt;
		this.value = val;
		block.recordActivity(Activity.ACTIVITY_SEND,port,(value==null?"NULL":(value.getValue()==null?"NULL VALUE":val.getValue().toString())));
	}
	
	public ProcessBlock getBlock()      { return block; }
	public String getPort()             { return port; }
	public QualifiedValue getValue()    { return value; }
}
