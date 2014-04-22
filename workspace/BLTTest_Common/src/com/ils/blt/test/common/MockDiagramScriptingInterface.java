/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 *   Based on sample code in the IA-scripting-module
 *   by Travis Cox.
 */
package com.ils.blt.test.common;

import java.util.UUID;

import com.ils.block.common.PropertyType;
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
	 * @return the new uniqueId of the test harness
	 */
	public UUID createMockDiagram(String blockClass);
	/**
	 * Define an input connected to the named port. This input is held as part of the 
	 * mock diagram. Once defined, the input cannot be deleted.A separate (duplicate) 
	 * input should be defined for every connection comming into the named port.
	 * @param harness
	 * @param tagPath path to tag on which we listen for input
	 * @param dt
	 * @param port
	 */
	public void addMockInput(UUID harness,String tagPath,PropertyType dt,String port );
	/**
	 * Define an output connection from the named port. This output is held as part of the 
	 * mock diagram. Once defined, the output cannot be deleted. Only one output may be
	 * defined as eminating from the same port.
	 * @param harness
	 * @param tagPath path to tag on which we write the output.
	 * @param dt
	 * @param port
	 */
	public void addMockOutput(UUID harness,String tagPath,PropertyType dt,String port );
	/**
	 * Remove the test harness from the execution engine (block controller).
	 * The harness is stopped before being deleted.
	 * 
	 * @param harness
	 */
	public void deleteMockDiagram(UUID harness);
	/**
	 * Read the current value held by the mock output identified by the specified
	 * port name.
	 * @param harness
	 * @param port
	 * @return the current value held by the specified port.
	 */
	public QualifiedValue readValue(UUID harness,String port);
	/**
	 * Set the value of the named property. This value ignores any type of binding.
	 * If the property is bound to a tag, then the value should be set by writing
	 * to that tag.
	 * 
	 * @param harness
	 * @param propertyName
	 * @param value
	 */
	public void setProperty(UUID harness,String propertyName,Object value);
	/**
	 * Simulate data arriving on the nth connection into the named input port. 
	 * @param harness
	 * @param port
	 * @param index of the connection into the named port. The index is zero-based.
	 * @param value
	 */
	public void setValue(UUID harness,String port,Integer index,QualifiedValue value);
	/**
	 * Start the test harness by activating subscriptions for bound properties and
	 * mock inputs.
	 * @param harness
	 */
	public void startMockDiagram(UUID harness);
	/**
	 * Stop all property updates and input receipt by canceling all active
	 * subscriptions involving the harness.
	 * @param harness unique Id
	 */
	public void stopMockDiagram(UUID harness);
}
