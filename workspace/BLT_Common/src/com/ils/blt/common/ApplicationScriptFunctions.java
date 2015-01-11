/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.List;

import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;


/**
 * This class exposes python-callable functions that deal with properties
 * of applications, families, diagrams, blocks and connections. It also handles
 * functions of the engine itself. All requests are delegated to the 
 * ApplicationRequestManager.
 */
public class ApplicationScriptFunctions   {
	private static ApplicationRequestHandler manager = new ApplicationRequestHandler();

	/**
	 * Remove all running diagrams from the controller. Cancel all tag subscriptions. 
	 */
	public static void clearController() {
		manager.clearController();
	}
	
	/**
	 * @return the default database for the project defined by the supplied Id  
	 */
	public static String databaseForProject(long projectId) {
		return manager.databaseForProject(projectId);
	}
	
	/**
	 * Query the gateway for a list of prototypes for the defined blocks. 
	 */
	@SuppressWarnings("rawtypes")
	public static List getBlockPrototypes() {
		List<PalettePrototype> result = manager.getBlockPrototypes();
		return result;
	}
	
	/**
	 * @return the current state of the controller.
	 */
	public static String getControllerState() {
		return manager.getControllerState();
	}
	
	/**
	 * Query the gateway for list of diagrams 
	 * 
	 * @param projectName
	 * @return a list of tree-paths to the diagrams saved (ie. known to the Gateway).
	 */
	@SuppressWarnings("rawtypes")
	public static List getDiagramDescriptors(String projectName) {
		return manager.getDiagramDescriptors(projectName);
	}
	/**
	 * @param diagramId identifier of the diagram to be queried, a String
	 * @param className fully qualified class name of blocks to be listed
	 * @return a list of ids for blocks owned by a specified diagram that are of a
	 *         specified class.
	 */
	public static List getDiagramBlocksOfClass(String diagramId,String className) {
		return manager.getDiagramBlocksOfClass(diagramId,className);
	}
	
	/**
	 * @return the the internal state of a block.
	 */
	public static SerializableBlockStateDescriptor getInternalState(String diagramId,String blockId) {
		return manager.getInternalState(diagramId,blockId);
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of a specified block property.
	 */
	public static Object getPropertyValue(String diagramId,String blockId,String propertyName) {
		return manager.getPropertyValue(diagramId,blockId,propertyName);
	}
	/**
	 * Query the gateway for list of resources that it knows about. This is
	 * a debugging aid. 
	 * 
	 * @return a list of resources known to the BlockController.
	 */
	@SuppressWarnings("rawtypes")
	public static List queryControllerResources() {
		List<SerializableResourceDescriptor> result = manager.queryControllerResources();
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
		List<SerializableResourceDescriptor> result = manager.queryDiagram(diagId);
		return result;
	}
	/**
	 * Execute reset() on the specified block
	 */
	public static void resetBlock(String diagramId,String blockId) {
		manager.resetBlock(diagramId,blockId);
	}
	/**
	 * Execute reset() on every block inside the controller
	 */
	public static void resetDiagram(String diagramId) {
		manager.resetDiagram(diagramId);
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
	public static boolean sendLocalSignal(String diagramId,String className, String command) {
		return manager.sendLocalSignal(diagramId,className,command);
	}
	
	/**
	 * Start the block execution engine in the gateway.
	 */
	public static void startController() {
		manager.startController();
	}

	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	public static void stopController() {
		manager.stopController();
	}

	/**
	 * Direct the blocks in a specified diagram to report their
	 * status values. This is in order to update the UI. 
	 */
	public static void triggerStatusNotifications() {
		manager.triggerStatusNotifications();
	}
}