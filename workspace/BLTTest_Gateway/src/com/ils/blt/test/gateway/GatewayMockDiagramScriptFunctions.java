/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.test.gateway;

import java.util.UUID;

import com.ils.block.common.PropertyType;
import com.ils.blt.test.common.BLTTestProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes python-callable functions used to report test blocks.
 *  These functions are designed for access from python scripts executing in the Gateway..
 *  
 *  Since we are in Gateway, we can make local calls.
 */
public class GatewayMockDiagramScriptFunctions  {
	private static final String TAG = "GatewayMockDiagramScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(GatewayMockDiagramScriptFunctions.class.getPackage().getName());
	public static BLTTGatewayRpcDispatcher dispatcher = null;
	
	/**
	 * Create a new mock diagram and add it to the list of diagrams known to the BlockController.
	 * This diagram has no valid resourceId and so is never saved permanently. It never shows
	 * in the designer. This call does not start subscriptions to tag changes. Subscriptions are
	 * triggered in response to a "start" call. This should be made after all to mock inputs and
	 * outputs are defined.
	 * 
	 * @param projectName name of the caller's project
	 * @param blockClass
	 * @return the new uniqueId of the test harness
	 */
	public static UUID createTestHarness(String projectName,String blockClass) {
		log.infof("%s.createTestHarness: %s %s ",TAG,projectName,blockClass);
		UUID result = null;
		if( dispatcher!=null ) {
			result = dispatcher.createTestHarness(projectName,blockClass);
		}
		return result;
	}
	/**
	 * Define an input connected to the named port. This input is held as part of the 
	 * mock diagram. Once defined, the input cannot be deleted.
	 * @param harness
	 * @param dt
	 * @param port
	 */
	public static void addMockInput(UUID harness,PropertyType dt,String port ) {
		log.infof("%s.addMockInput: %s %s",TAG,dt.toString(),port);
		if( dispatcher!=null ) {
			dispatcher.addMockInput(harness,dt,port);
		}
	}
	/**
	 * Define an output connected to the named port. This output is held as part of the 
	 * mock diagram. Once defined, the output cannot be deleted.
	 * @param harness
	 * @param dt
	 * @param port
	 */
	public static void addMockOutput(UUID harness,PropertyType dt,String port ) {
		log.infof("%s.addMockOutput: %s ",TAG,dt.toString(),port);
		if( dispatcher!=null ) {
			dispatcher.addMockOutput(harness,dt,port);
		}
	}
	/**
	 * Remove the test harness from the execution engine (block controller).
	 * The harness is stopped before being deleted.
	 * 
	 * @param harness
	 */
	public static void deleteTestHarness(UUID harness) {
		log.infof("%s.deleteTestHarness: %s ",TAG,harness.toString());
		if( dispatcher!=null ) {
			dispatcher.deleteTestHarness(harness);
		}
	}
	/**
	 * Read the current value held by the mock output identified by the specified
	 * port name.
	 * @param harness
	 * @param port
	 * @return the current value held by the specified port.
	 */
	public static QualifiedValue readValue(UUID harness,String port){ 
		log.infof("%s.readValue: %s %s",TAG,harness.toString(),port);
		QualifiedValue val = null;
		if( dispatcher!=null ) {
			val = dispatcher.readValue(harness,port);
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
		log.infof("%s.setProperty: %s %s=%s",TAG,harness.toString(),propertyName,value.toString());
		if( dispatcher!=null ) {
			dispatcher.setProperty(harness,propertyName,value);
		}
	}
	/**
	 * Simulate data arriving on the named input port. 
	 * @param harness
	 * @param value
	 * @param port
	 */
	public static void setValue(UUID harness,UUID blockId,String port,QualifiedValue value) {
		log.infof("%s.setValue: %s %s=%s",TAG,harness.toString(),port,value);
		if( dispatcher!=null ) {
			dispatcher.setValue(harness,port,value);
		}
	}
	/**
	 * Start the test harness by activating subscriptions for bound properties and
	 * mock inputs.
	 * @param harness
	 */
	public static void startTestHarness(UUID harness){
		log.infof("%s.startTestHarness: %s ",TAG,harness.toString());
		if( dispatcher!=null ) {
			dispatcher.startTestHarness(harness);
		}
	}
	/**
	 * Stop all property updates and input receipt by canceling all active
	 * subscriptions involving the harness.
	 * @param harness unique Id
	 */
	public static void stopTestHarness(UUID harness) {
		log.infof("%s.stopTestHarness: %s ",TAG,harness.toString());
		if( dispatcher!=null ) {
			dispatcher.stopTestHarness(harness);
		}
	}
}