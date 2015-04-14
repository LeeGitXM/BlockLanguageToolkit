/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.List;

import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;


/**
 * This class exposes python-callable functions that deal with properties
 * of applications, families, diagrams, blocks and connections. It also handles
 * functions of the engine itself. All requests are delegated to the 
 * ApplicationRequestManager.
 * 
 * These calls are available from any Ignition scope.
 */
public class ApplicationScriptFunctions   {
	private static ApplicationRequestHandler handler = new ApplicationRequestHandler();

	/**
	 * Remove all running diagrams from the controller. 
	 * Cancel all tag subscriptions. 
	 */
	public static void clearController() {
		handler.clearController();
	}
	public static String getApplicationName(String uuid) {
		return handler.getApplicationName(uuid);
	}
	/**
	 * Query the gateway for a list of prototypes for the defined blocks. 
	 */
	@SuppressWarnings("rawtypes")
	public static List getBlockPrototypes() {
		List<PalettePrototype> result = handler.getBlockPrototypes();
		return result;
	}
	/**
	 * @return the current state of the controller.
	 */
	public static String getControllerState() {
		return handler.getControllerState();
	}
	
	public static List<String> getDatasourceNames() {
		return handler.getDatasourceNames();
	}
	/**
	 * @param diagramId identifier of the diagram to be queried, a String
	 * @param className fully qualified class name of blocks to be listed
	 * @return a list of ids for blocks owned by a specified diagram that are of a
	 *         specified class.
	 */
	public static List getDiagramBlocksOfClass(String diagramId,String className) {
		return handler.getDiagramBlocksOfClass(diagramId,className);
	}
	
	/**
	 * Query the gateway for list of diagrams 
	 * 
	 * @param projectName
	 * @return a list of tree-paths to the diagrams saved (ie. known to the Gateway).
	 */
	@SuppressWarnings("rawtypes")
	public static List getDiagramDescriptors(String projectName) {
		return handler.getDiagramDescriptors(projectName);
	}
	public static String getFamilyName(String uuid) {
		return handler.getFamilyName(uuid);
	}
	
	/**
	 * @return the the internal state of a block.
	 */
	public static SerializableBlockStateDescriptor getInternalState(String diagramId,String blockId) {
		return handler.getInternalState(diagramId,blockId);
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of a specified block property.
	 */
	public static Object getPropertyValue(String diagramId,String blockId,String propertyName) {
		return handler.getPropertyValue(diagramId,blockId,propertyName);
	}
	/**
	 * Query the gateway for list of resources that it knows about. This is
	 * a debugging aid. 
	 * 
	 * @return a list of resources known to the BlockController.
	 */
	@SuppressWarnings("rawtypes")
	public static List queryControllerResources() {
		List<SerializableResourceDescriptor> result = handler.queryControllerResources();
		return result;
	}
	/**
	 * Query a diagram in the gateway for list of blocks that it knows about. This is
	 * a debugging aid. 
	 * 
	 * @return a list of blocks known to the diagram.
	 */
	@SuppressWarnings("rawtypes")
	public static List queryDiagram(String diagId) {
		List<SerializableResourceDescriptor> result = handler.queryDiagram(diagId);
		return result;
	}
	/**
	 * Post a (simulated) block result on its output.
	 * @param diagramId the parent diagram
	 * @param blockId
	 * @param port
	 * @param value
	 */
	public static void postResult(String diagramId,String blockId,String port,String value) {
		handler.postResult(diagramId,blockId, port, value);
	}
	/**
	 * Execute reset() on the specified block
	 */
	public static void resetBlock(String diagramId,String blockId) {
		handler.resetBlock(diagramId,blockId);
	}
	/**
	 * Execute reset() on every block inside the controller
	 */
	public static void resetDiagram(String diagramId) {
		handler.resetDiagram(diagramId);
	}
	/**
	 * Change the state of every diagram in the named application
	 * to the specified state.
	 * @param appname name of the application
	 * @param state new diagram state
	 */
	public static void setApplicationState(String appname, String state) {
		handler.setApplicationState(appname, state);
	}
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission.
	 * 
	 * @param diagramId diagram identifier
	 * @param className filter of the receiver blocks to be targeted.
	 * @param command string of the signal.
	 * @return true on success
	 */
	public static boolean sendLocalSignal(String diagramId,String command,String message,String arg) {
		return handler.sendLocalSignal(diagramId,command,message,arg);
	}
	/**
	 * Specify the new state of a diagram
	 * @param diagramId
	 * @param state
	 */
	public static void setDiagramState(String diagramId, String state)  {
		handler.setDiagramState(diagramId,state);
	}
	/**
	 * Set a clock rate factor. This will change timing for isolation mode only.
	 * This method is provided as a hook for test frameworks.
	 * @param factor the amount to speed up or slow down the clock.
	 */
	public static void setTimeFactor(double factor) {
		handler.setTimeFactor(factor);
	}
	/**
	 * Start the block execution engine in the gateway.
	 */
	public static void startController() {
		handler.startController();
	}

	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	public static void stopController() {
		handler.stopController();
	}

	/**
	 * Direct the blocks in a specified diagram to report their
	 * status values. This is in order to update the UI. 
	 */
	public static void triggerStatusNotifications() {
		handler.triggerStatusNotifications();
	}
}