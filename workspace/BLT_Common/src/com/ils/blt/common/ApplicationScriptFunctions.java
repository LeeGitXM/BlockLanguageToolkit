/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.List;

import com.ils.blt.common.block.PalettePrototype;
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
	 * Query the gateway for a list of prototypes for the defined blocks. 
	 */
	@SuppressWarnings("rawtypes")
	public static List getBlockPrototypes() {
		List<PalettePrototype> result = manager.getBlockPrototypes();
		return result;
	}
	
	
	/**
	 * Query the gateway for list of diagrams 
	 * 
	 * @param projectName
	 * @return a list of tree-paths to the diagrams saved (ie. known to the Gateway).
	 */
	@SuppressWarnings("rawtypes")
	public static List getDiagramTreePaths(String projectName) {
		return manager.getDiagramTreePaths(projectName);
	}
	
	/**
	 * @return the current state of the controller.
	 */
	public static String getControllerState() {
		return manager.getControllerState();
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
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission.
	 * 
	 * @param projectName
	 * @param diagramPath
	 * @param className filter of the receiver blocks to be targeted.
	 * @param command string of the signal.
	 * @return true on success
	 */
	public static boolean sendLocalSignal(String projectName, String diagramPath,String className, String command) {
		return manager.sendLocalSignal(projectName,diagramPath,className,command);
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

	
}