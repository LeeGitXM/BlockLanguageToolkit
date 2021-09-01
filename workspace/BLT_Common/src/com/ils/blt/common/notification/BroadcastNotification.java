/**
 *   (c) 2014-2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;

import com.ils.blt.common.block.TransmissionScope;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
/**
 * This class is used to hold a signal to be broadcast to a collection of blocks.
 * The broadcast notification is sent to the execution controller. It figures out
 * potential destinations. A broadcast bypasses any concept of connections.
 * 
 * This is a property container with no behavior.
 */
public class BroadcastNotification {
	private final ProjectResourceId diagramId;
	private final String blockName;
	private final TransmissionScope scope;
	private final QualifiedValue value;
	
	/**
	 * Constructor. Use this constructor to define a broadcast to multiple blocks in a diagram.
	 *              Note that the qualified value is guaranteed to hold a signal.
	 * @param diagId id of the parent diagram
	 * @param tscope transmission scope
	 * @param qv qualified value containing the signal to be transmitted.
	 */
	public BroadcastNotification(ProjectResourceId diagId,TransmissionScope tscope,QualifiedValue qv)  {
		this.diagramId = diagId;
		this.scope = tscope;
		this.value = qv;
		this.blockName = null;
	}
	
	/**
	 * Constructor. Use this constructor to send a signal to a specific block.
	 *              Note that the qualified value is guaranteed to hold a signal.
	 * @param diagId id of the parent diagram
	 * @param block name of the target block
	 * @param qv qualified value containing the signal to be transmitted.
	 */
	public BroadcastNotification(ProjectResourceId diagId,String block,QualifiedValue qv)  {
		this.diagramId = diagId;
		this.scope = TransmissionScope.BLOCK;
		this.value = qv;
		this.blockName = block;
	}
	
	public String getBlockName() { return this.blockName; }
	public ProjectResourceId getDiagramId() {return diagramId;}
	public Signal getSignal() {return (Signal)value.getValue();}
	public QualifiedValue getValue() {return value;}
	public TransmissionScope getScope() {return scope;}
}
