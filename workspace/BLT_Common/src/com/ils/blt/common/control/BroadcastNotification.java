/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.control;

import java.util.UUID;

import com.ils.blt.common.block.TransmissionScope;

/**
 * This class is used to hold a signal to be broadcast to a collection of blocks.
 * The broadcast notification is sent to the execution controller. It figures out
 * potential destinations. A broadcast bypasses any concept of connections.
 * 
 * This is a property container with no behavior.
 */
public class BroadcastNotification {
	private final UUID diagramId;
	private final TransmissionScope scope;
	private final Signal signal;
	
	/**
	 * Constructor. 
	 * @param diagramId
	 * @param scope
	 * @param sig the signal to be transmitted.
	 */
	public BroadcastNotification(UUID diagId,TransmissionScope tscope,Signal sig)  {
		this.diagramId = diagId;
		this.scope = tscope;
		this.signal = sig;
	}
	
	public UUID getDiagramId() {return diagramId;}
	public Signal getSignal() {return signal;}
	public TransmissionScope getScope() {return scope;}
}
