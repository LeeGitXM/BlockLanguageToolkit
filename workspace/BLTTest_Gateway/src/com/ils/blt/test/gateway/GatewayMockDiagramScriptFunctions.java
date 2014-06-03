/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.test.gateway;

import java.util.UUID;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes python-callable functions used to report test blocks.
 *  These functions are designed for access from python scripts executing in the Gateway..
 *  
 *  Delegate most requests through the MockDiagramRequestHandler. This allows a single
 *  handler for both Gateway scripting and RPC requests.
 */
public class GatewayMockDiagramScriptFunctions  {
	private static final String TAG = "GatewayMockDiagramScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(GatewayMockDiagramScriptFunctions.class.getPackage().getName());
	public static MockDiagramRequestHandler requestHandler = null;   // Set by the hook
	
	/**
	 * Create a new mock diagram and add it to the list of diagrams known to the BlockController.
	 * This diagram has no valid resourceId and so is never saved permanently. It never shows
	 * in the designer. This call does not start subscriptions to tag changes. Subscriptions are
	 * triggered in response to a "start" call. This should be made after all to mock inputs and
	 * outputs are defined.
	 * 
	 * @param projectName name of the caller's project
	 * @param blockClass class of block-under-test
	 * @return the new uniqueId of the test diagramId
	 */
	public static UUID createMockDiagram(String blockClass) {
		log.infof("%s.createMockDiagram: for class %s ",TAG,blockClass);
		return requestHandler.createMockDiagram(blockClass);
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
		log.infof("%s.addMockInput: %s %s %s",TAG,tagPath,propertyType.toString(),port);
		requestHandler.addMockInput(diagramId,tagPath,propertyType,port);
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
		log.infof("%s.addMockOutput: %s %s %s",TAG,tagPath,propertyType.toString(),port);
		requestHandler.addMockOutput(diagramId,tagPath,propertyType,port);
	}
	/**
	 * Remove the test diagramId from the execution engine (block controller).
	 * The diagramId is stopped before being deleted.
	 * 
	 * @param diagramId
	 */
	public static void deleteMockDiagram(UUID diagramId) {
		log.infof("%s.deleteMockDiagram: %s ",TAG,diagramId.toString());
		if( requestHandler!=null ) {
			requestHandler.deleteMockDiagram(diagramId);
		}
	}
	/**
	 * Force the block under test to present a specified value on the named output.
	 * @param diagram
	 * @param port
	 * @param value to be presented on the output connection.
	 */
	public void forcePost(UUID diagramId,String port,Object value) {
		log.infof("%s.forcePost: %s %s = %s",TAG,diagramId.toString(),port,value.toString());
		if( requestHandler!=null ) {
			requestHandler.deleteMockDiagram(diagramId);
		}
	}
	/**
	 * Read the current value held by the mock output identified by the specified
	 * port name.
	 * @param diagramId
	 * @param port
	 * @return the current value held by the specified port.
	 */
	public static QualifiedValue readValue(UUID diagramId,String port){ 
		log.infof("%s.readValue: %s %s",TAG,diagramId.toString(),port);
		QualifiedValue val = null;
		if( requestHandler!=null ) {
			val = requestHandler.readValue(diagramId,port);
		}
		return val;
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
	public static void setTestBlockProperty(UUID diagramId,String propertyName,String value){ 
		log.infof("%s.setTestBlockProperty: %s %s=%s",TAG,diagramId.toString(),propertyName,value);
		if( requestHandler!=null ) {
			requestHandler.setTestBlockProperty(diagramId,propertyName,value);
		}
	}
	/**
	 * Start the test diagramId by activating subscriptions for bound properties and
	 * mock inputs. This also starts the controller if it wasn't running.
	 * @param diagramId
	 */
	public static void startMockDiagram(UUID diagramId){
		log.infof("%s.startMockDiagram: %s ",TAG,diagramId.toString());
		if( requestHandler!=null ) {
			requestHandler.startMockDiagram(diagramId);
		}
	}
	
	/**
	 * Stop all property updates and input receipt by canceling all active
	 * subscriptions involving the diagramId. Stop the controller.
	 * @param diagramId unique Id
	 */
	public static void stopMockDiagram(UUID diagramId) {
		log.infof("%s.stopMockDiagram: %s ",TAG,diagramId.toString());
		if( requestHandler!=null ) {
			requestHandler.stopMockDiagram(diagramId);
		}
	}
	
	/**
	 * Simulate data arriving on the named input port. 
	 * @param diagramId
	 * @param index of the connection into the named port. The index is zero-based.
	 * @param port
	 * @param value
	 * @param quality
	 * @return timestamp, the time at which the input was created
	 */
	public static long writeValue(UUID diagramId,UUID blockId,String port,int index,String value,String quality) {
		log.infof("%s.writeValue: %s %s.%d=%s",TAG,diagramId.toString(),port,index,value);
		long timestamp = 0;
		if( requestHandler!=null ) {
			timestamp = requestHandler.writeValue(diagramId,port,index,value,quality);
		}
		return timestamp;
	}
}