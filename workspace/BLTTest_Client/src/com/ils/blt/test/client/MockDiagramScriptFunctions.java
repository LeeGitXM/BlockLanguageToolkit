/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *   Based on sample code in the IA-scripting-module
 *   by Travis Cox.
 */
package com.ils.blt.test.client;

import java.util.UUID;

import com.ils.block.common.PropertyType;
import com.ils.blt.test.common.BLTTestProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes the methods available to a designer/client for the
 *  purposes of testing BLT blocks. 
 *  
 *  These methods mimic MockDiagramScriptingInterface, but must be defined as static.
 */
public class MockDiagramScriptFunctions   {
	private static final String TAG = "MockDiagramScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(MockDiagramScriptFunctions.class.getPackage().getName());
	/**
	 * Create a new mock diagram and add it to the list of diagrams known to the BlockController.
	 * This diagram has no valid resourceId and so is never saved permanently. It never shows
	 * in the designer. This call does not start subscriptions to tag changes. Subscriptions are
	 * triggered in response to a "start" call. This should be made after all to mock inputs and
	 * outputs are defined.
	 * 
	 * @param blockClass
	 * @return the new uniqueId of the test harness
	 */
	public static UUID createMockDiagram(String blockClass) {
		log.infof("%s.createMockDiagram: for class %s ",TAG,blockClass);
		UUID result = null;
		try {
			result = (UUID)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "createMockDiagram", blockClass);
		}
		catch(Exception ge) {
			log.infof("%s.createMockDiagram: GatewayException ("+ge.getMessage()+")");
		}
		return result;
	}
	/**
	 * Define an input connected to the named port. This input is held as part of the 
	 * mock diagram. Once defined, the input cannot be deleted.
	 * @param harness
	 * @param tagPath
	 * @param dt
	 * @param port
	 */
	public static void addMockInput(UUID harness,String tagPath,PropertyType dt,String port ) {
		log.infof("%s.addMockInput: %s %s %s",TAG,tagPath, dt.toString(),port);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "addMockInput", tagPath,dt.toString(), port);
		}
		catch(Exception ge) {
			log.infof("%s.addMockInput: GatewayException ("+ge.getMessage()+")");
		}
	}
	/**
	 * Define an output connected to the named port. This output is held as part of the 
	 * mock diagram. Once defined, the output cannot be deleted.
	 * @param harness
	 * @param tagPath
	 * @param dt
	 * @param port
	 */
	public static void addMockOutput(UUID harness,String tagPath,PropertyType dt,String port ) {
		log.infof("%s.addMockOutput: %s %s %s %s",TAG,tagPath,dt.toString(),port);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "addMockOutput", tagPath,dt.toString(),port);
		}
		catch(Exception ge) {
			log.infof("%s.addMockOutput: GatewayException ("+ge.getMessage()+")");
		}
	}
	/**
	 * Remove the test harness from the execution engine (block controller).
	 * The harness is stopped before being deleted.
	 * 
	 * @param harness
	 */
	public static void deleteMockDiagram(UUID harness) {
		log.infof("%s.deleteMockDiagram: %s ",TAG,harness.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "deleteMockDiagram", harness);
		}
		catch(Exception ge) {
			log.infof("%s.deleteMockDiagram: GatewayException ("+ge.getMessage()+")");
		}
	}
	/**
	 * Read the current value held by the mock output identified by the specified
	 * port name.
	 * @param harness
	 * @param blockId
	 * @param port
	 * @return the current value held by the specified port.
	 */
	public static QualifiedValue readValue(UUID harness,String port){ 
		log.infof("%s.readValue: %s %s",TAG,harness.toString(),port);
		QualifiedValue val = null;
		try {
			val = (QualifiedValue) GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "readValue", harness,port);
		}
		catch(Exception ge) {
			log.infof("%s.readValue: GatewayException ("+ge.getMessage()+")");
		}
		return val;
	}
	/**
	 * Set the value of the named property. This value ignores any type of binding.
	 * If the property is bound to a tag, then the value should be set by writing
	 * to that tag.
	 * 
	 * @param harness
	 * @param propertyName
	 * @param value
	 */
	public static void setProperty(UUID harness,String propertyName,Object value){ 
		log.infof("%s.setProperty: %s %s=%s",TAG,harness.toString(), propertyName,value.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "setProperty", harness,propertyName,value.toString());
		}
		catch(Exception ge) {
			log.infof("%s.setProperty: GatewayException ("+ge.getMessage()+")");
		}
	}
	/**
	 * Simulate data arriving on the named input port. 
	 * @param harness
	 * @param port
	 * @param index of the connection into the named port. The index is zero-based.
	 * @param value
	 */
	public static void setValue(UUID harness,String port,int index,QualifiedValue value) {
		log.infof("%s.setValue: %s %s%%d=%s",TAG,harness.toString(),port,index,value);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "setValue", harness,port,new Integer(index),value);
		}
		catch(Exception ge) {
			log.infof("%s.setValue: GatewayException ("+ge.getMessage()+")");
		}
	}
	/**
	 * Start the test harness by activating subscriptions for bound properties and
	 * mock inputs.
	 * @param harness
	 */
	public static void startMockDiagram(UUID harness){
		log.infof("%s.startMockDiagram: %s ",TAG,harness.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "startMockDiagram", harness);
		}
		catch(Exception ge) {
			log.infof("%s.startMockDiagram: GatewayException ("+ge.getMessage()+")");
		}
	}
	/**
	 * Stop all property updates and input receipt by canceling all active
	 * subscriptions involving the harness.
	 * @param harness unique Id
	 */
	public static void stopMockDiagram(UUID harness) {
		log.infof("%s.stopMockDiagram: %s ",TAG,harness.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "stopMockDiagram", harness);
		}
		catch(Exception ge) {
			log.infof("%s.stopMockDiagram: GatewayException ("+ge.getMessage()+")");
		}
	}
	
}