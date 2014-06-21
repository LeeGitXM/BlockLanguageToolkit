/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.control;

import java.util.UUID;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is used to hold a value to be delivered to "source" connection posts
 * listening for transmissions from the named "sink" post.
 * The notification is sent to the execution controller. It figures out
 * potential destinations, bypassing any concept of connections.
 * 
 * This is a property container with no behavior.
 */
public class ConnectionPostNotification {
	private final UUID diagramId;
	private final String originName;
	private final QualifiedValue value;
	
	/**
	 * Constructor.
	 * @param did the UUID of the parent diagram. Notification is not
	 *            transmitted if the unless the diagram is ACTIVE.
	 * @param name of the Sink Connection that originated the value
	 * @param qv value to be transmitted 
	 */
	public ConnectionPostNotification(UUID did,String name,QualifiedValue qv)  {
		this.diagramId = did;
		this.originName = name;
		this.value = qv;
	}
	public UUID getDiagramId()    { return diagramId; }
	public String getOriginName() {return originName;}
	public QualifiedValue getValue() {return value;}
}
