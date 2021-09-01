/**
 *   (c) 2014-2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
/**
 * This class is used to hold a value to be delivered to "source" connection posts
 * listening for transmissions from the named "sink" post.
 * The notification is sent to the execution controller. It figures out
 * potential destinations, bypassing any concept of connections.
 * 
 * This is a property container with no behavior.
 */
public class ConnectionPostNotification {
	private final ProjectResourceId diagramId;
	private final String originName;
	private final QualifiedValue value;
	
	/**
	 * Constructor.
	 * @param did the UUID of the parent diagram. Notification is not
	 *            transmitted if the unless the diagram is ACTIVE.
	 * @param name of the Sink Connection that originated the value
	 * @param qv value to be transmitted 
	 */
	public ConnectionPostNotification(ProjectResourceId did,String name,QualifiedValue qv)  {
		this.diagramId = did;
		this.originName = name;
		this.value = qv;
	}
	public ProjectResourceId getDiagramId()    { return diagramId; }
	public String getOriginName() {return originName;}
	public QualifiedValue getValue() {return value;}
}
