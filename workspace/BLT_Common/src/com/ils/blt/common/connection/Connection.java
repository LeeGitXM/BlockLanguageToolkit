/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.connection;

import java.util.UUID;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;



/**
 * This is the interface that defines a connection (edge) in a diagram.
 * A connection is identified only by its source and target blocks and ports.
 */
public interface Connection {
	/**
	 * @return a the type of this connection, the data type.
	 */
	public ConnectionType getType();
	
	/**
	 * @return the name of output port on the upstream block to which we are connected.
	 */
	public String getUpstreamPortName();
	/**
	 * @param the name of output port on the upstream block to which we are connected.
	 */
	public void setUpstreamPortName(String port);
	/**
	 * @return the name of input port on the downstream block to which we are connected.
	 */
	public String getDownstreamPortName();
	/**
	 * @param the name of input port on the downstream block to which we are connected
	 */
	public void setDownstreamPortName(String port);
	/**
	 * @return the block id of the upstream block to which we are connected.
	 */
	public UUID getSource();
	/**
	 * @param id the block id of the upstream block to which we are connected
	 */
	public void setSource(UUID id);
	/**
	 * @return the block id of the downstream block to which we are connected.
	 */
	public UUID getTarget();
	/**
	 * @param id the block id of the downstream block to which we are connected
	 */
	public void setTarget(UUID id);
	
	/**
	 * @return the latest value placed on this connection
	 */
	public QualifiedValue getValue();
	/**
	 * @param val the current value on this connection
	 */
	public void setValue(QualifiedValue val);
}