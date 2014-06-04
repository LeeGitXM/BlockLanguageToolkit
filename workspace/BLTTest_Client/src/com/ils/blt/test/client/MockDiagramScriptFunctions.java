/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.test.client;

import java.util.UUID;

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
	private static final String TAG = "MockDiagramScriptFunctions";
	private static LoggerEx log = LogUtil.getLogger(MockDiagramScriptFunctions.class.getPackage().getName());
	/**
	 * Create a new mock diagram and add it to the list of diagrams known to the BlockController.
	 * This diagram has no valid resourceId and so is never saved permanently. It never shows
	 * in the designer. This call does not start subscriptions to tag changes. Subscriptions are
	 * triggered in response to a "start" call. This should be made after all to mock inputs and
	 * outputs are defined.
	 * 
	 * @param blockClass
	 * @return the new uniqueId of the test diagram
	 */
	public static UUID createMockDiagram(String blockClass) {
		log.debugf("%s.createMockDiagram: for class %s",TAG,blockClass);
		UUID result = null;
		try {
			result = (UUID)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "createMockDiagram", blockClass);
		}
		catch(Exception ge) {
			log.infof("%s.createMockDiagram: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * Define an input connected to the named port. This input is held as part of the 
	 * mock diagram. Once defined, the input cannot be deleted.
	 * @param diagramId
	 * @param tagPath
	 * @param propertyType
	 * @param port
	 */
	public static void addMockInput(UUID diagramId,String tagPath,String propertyType,String port ) {
		log.debugf("%s.addMockInput: %s %s %s",TAG,tagPath, propertyType.toString(),port);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "addMockInput", diagramId,tagPath,propertyType, port);
		}
		catch(Exception ge) {
			log.infof("%s.addMockInput: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Define an output connected to the named port. This output is held as part of the 
	 * mock diagram. Once defined, the output cannot be deleted.
	 * @param diagramId
	 * @param tagPath
	 * @param propertyType
	 * @param port
	 */
	public static void addMockOutput(UUID diagramId,String tagPath,String propertyType,String port ) {
		log.debugf("%s.addMockOutput: %s %s %s",TAG,tagPath,propertyType.toString(),port);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "addMockOutput", diagramId, tagPath, propertyType.toString(),port);
		}
		catch(Exception ge) {
			log.infof("%s.addMockOutput: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Remove the test diagram from the execution engine (block controller).
	 * The diagram is stopped before being deleted.
	 * 
	 * @param diagram
	 */
	public static void deleteMockDiagram(UUID diagram) {
		log.debugf("%s.deleteMockDiagram: %s ",TAG,diagram.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "deleteMockDiagram", diagram);
		}
		catch(Exception ge) {
			log.infof("%s.deleteMockDiagram: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Force the block under test to present a specified value on the named output.
	 * @param diagram
	 * @param port
	 * @param value to be presented on the output connection. It will be coerced into the
	 *              correct data type for the connection.
	 */
	public static void forcePost(UUID diagramId,String port,String value) {
		log.infof("%s.forcePost: %s %s->%s",TAG,diagramId.toString(),port,value.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "forcePost", diagramId,port,value.toString());
		}
		catch(Exception ge) {
			log.info(String.format("%s.forcePost: GatewayException (%s)",TAG,ge.getMessage()),ge);
		}
	}
	/**
	 * Return the execution state of the block under test.
	 * @param diagram
	 * @return the state of the block under test.
	 */
	public static String getState(UUID diagramId) {
		log.debugf("%s.getState: %s",TAG,diagramId.toString());
		String state = "";
		try {
			String result = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "getState", diagramId);
			state = result;
		}
		catch(Exception ge) {
			log.infof("%s.getState: GatewayException (%s)",TAG,ge.getMessage());
		}
		return state;
	}
	/**
	 * Return the locked state of the block under test.
	 * @param diagram
	 * @return true if the block under test is locked.
	 */
	public static boolean isLocked(UUID diagramId) {
		log.debugf("%s.isLocked: %s",TAG,diagramId.toString());
		boolean locked = false;
		try {
			Boolean result = (Boolean)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "isLocked", diagramId);
			locked = result.booleanValue();
		}
		catch(Exception ge) {
			log.infof("%s.isLocked: GatewayException (%s)",TAG,ge.getMessage());
		}
		return locked;
	}
	/**
	 * Read the current value held by the mock output identified by the specified
	 * port name.  NOTE: A legitimate null value is returned as a Qualified value,
	 * that has null for its value.
	 * @param diagram
	 * @param blockId
	 * @param port
	 * @return the current value held by the specified port.
	 */
	public static QualifiedValue readValue(UUID diagram,String port){ 
		QualifiedValue val = null;
		try {
			val = (QualifiedValue) GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "readValue", diagram,port);
			log.debugf("%s.readValue: %s %s=%s",TAG,diagram.toString(),port,val.toString());
		}
		catch(Exception ge) {
			log.infof("%s.readValue: GatewayException (%s)",TAG,ge.getMessage());
		}
		return val;
	}
	/**
	 * Execute the block under test's reset method.
	 * @param diagram
	 */
	public static void reset(UUID diagramId) {
		log.debugf("%s.reset: %s",TAG,diagramId.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "reset", diagramId);
		}
		catch(Exception ge) {
			log.infof("%s.reset: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Set the locked state of the block under test
	 * 
	 * @param diagramId
	 * @param flag the new locked state of the block
	 */
	public static void setLocked(UUID diagramId,boolean flag) {
		log.debugf("%s.setLocked: %s %s",TAG,diagramId.toString(), (flag?"true":"false"));
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "setLocked", diagramId,new Boolean(flag));
		}
		catch(Exception ge) {
			log.infof("%s.setLocked: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Set the value of the named property in the block-under-test. This value ignores
	 * any type of binding. Normally, if the property is bound to a tag, then the value
	 * should be set by writing to that tag.
	 * 
	 * @param diagramId
	 * @param propertyName
	 * @param value
	 */
	public static void setTestBlockProperty(UUID diagramId,String propertyName,Object value){ 
		log.debugf("%s.setTestBlockProperty: %s %s=%s",TAG,diagramId.toString(), propertyName,value.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "setTestBlockProperty", diagramId,propertyName,value.toString());
		}
		catch(Exception ge) {
			log.infof("%s.setTestBlockProperty: GatewayException (%s)",TAG,ge.getMessage());
		}
	}

	/**
	 * Start the test diagram by activating subscriptions for bound properties and
	 * mock inputs.
	 * @param diagram
	 */
	public static void startMockDiagram(UUID diagram){
		log.debugf("%s.startMockDiagram: %s ",TAG,diagram.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "startMockDiagram", diagram);
		}
		catch(Exception ge) {
			log.infof("%s.startMockDiagram: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Stop all property updates and input receipt by canceling all active
	 * subscriptions involving the diagram.
	 * @param diagram unique Id
	 */
	public static void stopMockDiagram(UUID diagram) {
		log.debugf("%s.stopMockDiagram: %s ",TAG,diagram.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "stopMockDiagram", diagram);
		}
		catch(Exception ge) {
			log.infof("%s.stopMockDiagram: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	
	/**
	 * Direct a MockInput block to transmit a value to the block-under-test.
	 *   
	 * @param diagram
	 * @param port
	 * @param index of the connection into the named port. The index is zero-based.
	 * @param value
	 * @param quality
	 */
	public static long writeValue(UUID diagram,String port,int index,String value, String quality) {
		log.debugf("%s.writeValue: %s %s.%d=%s",TAG,diagram.toString(),port,index,value);
		long timestamp = 0;
		try {
			Long result = (Long)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "writeValue", diagram,port,new Integer(index),value,quality);
			if( result!=null ) timestamp = result.longValue();
		}
		catch(Exception ge) {
			log.infof("%s.writeValue: GatewayException (%s)",TAG,ge.getMessage());
		}
		return timestamp;
	}
	
}