/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

/**
 * Hold the results of a block evaluation. This is property container with no behavior.
 */
public class ExecutionCompletionNotification {
	private final String key;
	private final Object value;
	private final String port;
	/**
	 * Constructor.
	 * 
	 * @param keyString block identifier, includes the tree-path to the diagram and block-id within the diagram
	 * @param val the result of the block's computation
	 * @param portName the output port on which to insert the result
	 */
	public ExecutionCompletionNotification(String keyString,Object val,String portName)  {	
		this.key = keyString;
		this.value = val;
		this.port = portName;
	}
	
	public String getKey()   { return key; }
	public Object getValue() { return value; }
	public String getPort()  { return port; }
}
