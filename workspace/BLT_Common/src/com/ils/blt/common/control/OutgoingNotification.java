/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.control;

import com.ils.blt.common.block.ProcessBlock;

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
	private final Object value;
	
	/**
	 * Constructor. Value is expressed as a QualifiedValue
	 * 
	 * @param blk the block that is the source of the value.
	 * @param prt the output port on which the value was placed.
	 * @param val the new value
	 */
	public OutgoingNotification(ProcessBlock blk,String prt, Object val)  {	
		this.block = blk;
		this.port = prt;
		this.value = val;
	}
	
	public ProcessBlock getBlock()      { return block; }
	public String getPort()             { return port; }
	public Object getValue()            { return value; }
}
