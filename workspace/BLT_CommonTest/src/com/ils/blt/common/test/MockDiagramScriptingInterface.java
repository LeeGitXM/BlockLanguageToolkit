/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 *   Based on sample code in the IA-scripting-module
 *   by Travis Cox.
 */
package com.ils.blt.common.test;

import java.util.UUID;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 *  Define the methods available to a python test script in the designer.
 */
public interface MockDiagramScriptingInterface   {
	/**
	 * Create a new mock diagram and add it to the list of diagrams known to the BlockController.
	 * This diagram has no valid resourceId and so is never saved permanently. It never shows
	 * in the designer. This call does not start subscriptions to tag changes. Subscriptions are
	 * triggered in response to a "start" call. This should be made after all to mock inputs and
	 * outputs are defined.
	 * 
	 * The diagram holds exactly one block, the "Unit Under Test".
	 * 
	 * @param blockClass
	 * @param project the name of the owning project
	 * @return the new uniqueId of the test diagram
	 */
	public UUID createMockDiagram(String blockClass,String project);
	/**
	 * Define an input connected to the named port. This input is held as part of the 
	 * mock diagram. Once defined, the input cannot be deleted.A separate (duplicate) 
	 * input should be defined for every connection coming into the named port.
	 * @param diagram
	 * @param tagPath path to tag on which we listen for input
	 * @param propertyType
	 * @param port
	 */
	public void addMockInput(UUID diagram,String tagPath,String propertyType,String port );
	/**
	 * Define an output connection from the named port. This output is held as part of the 
	 * mock diagram. Once defined, the output cannot be deleted. Only one output may be
	 * defined as eminating from the same port.
	 * @param diagram
	 * @param tagPath path to tag on which we write the output.
	 * @param propertyType
	 * @param port
	 */
	public void addMockOutput(UUID diagram,String tagPath,String propertyType,String port );
	/**
	 * Clear any local data stored in the named output.
	 * @param diagram
	 * @param port
	 */
	public void clearOutput(UUID diagramId,String port);
	/**
	 * Remove the test diagram from the execution engine (block controller).
	 * The diagram is stopped before being deleted.
	 * 
	 * @param diagram
	 */
	public void deleteMockDiagram(UUID diagram);
	/**
	 * Force the block under test to present a specified value on the named output.
	 * @param diagram
	 * @param port
	 * @param value to be presented on the output connection.
	 */
	public void forcePost(UUID diagram,String port,String value);
	/**
	 * Get the current value of the named property in the block-under-test.
	 * 
	 * @param diagramId
	 * @param propertyName
	 */
	public Object getTestBlockPropertyValue(UUID diagramId,String propertyName);
	/**
	 * Return the execution state of the block under test.
	 * @param diagram
	 * @return the state of the block under test.
	 */
	public String getState(UUID diagramId);
	/**
	 * Return the locked state of the block under test.
	 * @param diagram
	 * @return true if the block under test is locked.
	 */
	public boolean isLocked(UUID diagram);
	/**
	 * Read the value of a tag
	 * 
	 * @param path a fully qualified tag path
	 * @retunr value the qualified value to be written to the tag.
	 */
	public QualifiedValue readTag(String path);
	/**
	 * Read the current value held by the mock output identified by the specified
	 * port name.
	 * @param diagram
	 * @param port
	 * @return the current value held by the specified port.
	 */
	public QualifiedValue readValue(UUID diagram,String port);
	/**
	 * Execute the block under test's reset method.
	 * @param diagram
	 */
	public void reset(UUID diagram);
	/**
	 * Set the locked state of the block nder test
	 * 
	 * @param diagramId
	 * @param flag the new locked state of the block
	 */
	public void setLocked(UUID diagramId,Boolean flag);
	/**
	 * Set the value of the named property in the block-under-test. This value ignores
	 * any type of binding. Normally, if the property is bound to a tag, then the value
	 * should be set by writing to that tag.
	 * 
	 * @param diagramId
	 * @param propertyName
	 * @param value
	 */
	public void setTestBlockProperty(UUID diagramId,String propertyName,String value);

	/**
	 * Set the binding type and binding string of the named property in the block-under-test. 
	 * 
	 * @param diagramId
	 * @param propertyName
	 * @param type binding type: NONE or TAG
	 * @param binding for a TAG type this is a fully qualified tag path
	 */
	public void setTestBlockPropertyBinding(UUID diagramId,String propertyName,String type,String binding);
	
	/**
	 * Start the test diagram by activating subscriptions for bound properties and
	 * mock inputs.
	 * @param diagram
	 */
	public void startMockDiagram(UUID diagram);
	/**
	 * Stop all property updates and input receipt by canceling all active
	 * subscriptions involving the diagram.
	 * @param diagram unique Id
	 */
	public void stopMockDiagram(UUID diagram);
	/**
	 * Update the connection type of a block anchor
	 * @param diagramId
	 * @param port name
	 * @param type of the connection to be attached to this port
	 */
	public void updateBlockAnchor(UUID diagramId,String port,String type);
	/**
	 * Write to a tag
	 * 
	 * @param projectId the project from which to obtain the default tag provider
	 *                  should one not be specified in the path.
	 * @param path a fully qualified tag path
	 * @param value the qualified value to be written to the tag.
	 */
	public void updateTag(Long projectId,String path,QualifiedValue value);
	/**
	 * Transmit a signal with the specified command to the block-under-test.
	 *   
	 * @param diagram
	 * @param command
	 * @return timestamp, the system time at which the value was created
	 */
	public long writeCommand(UUID diagram,String command,String arg,String msg);
	/**
	 * Simulate data arriving on the nth connection into the named input port of the 
	 * block under test. 
	 * @param diagram
	 * @param port
	 * @param index of the connection into the named port. The index is zero-based.
	 * @param value
	 * @return timestamp, the system time at which the value was created
	 */
	public long writeValue(UUID diagram,String port,Integer index,String value,String quality);
}
