/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.List;

import com.ils.block.common.PalettePrototype;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;


/**
 * This class exposes python-callable functions that deal with properties
 * of applications, families, diagrams, blocks and connections. It also handles
 * functions of the engine itself. 
 * 
 * @see com.ils.blt.gateway.GatewayBlockScriptFunctions for the same routines available from Gateway scope.
 * 
 * Where applicable, we make use of the BlockRequestHandler to perform the requests.
 */
public class BlockScriptFunctions   {
	private static BlockRequestManager handler = new BlockRequestManager();

	/**
	 * Query the gateway for a list of prototypes for the defined blocks. 
	 */
	@SuppressWarnings("rawtypes")
	public static List getBlockPrototypes() {
		List<PalettePrototype> result = handler.getBlockPrototypes();
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
		return handler.getDiagramTreePaths(projectName);
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
		return handler.sendLocalSignal(projectName,diagramPath,className,command);
	}
	
	/**
	 * Start the block execution engine in the gateway.
	 */
	public void startController() {
		handler.startController();
	}

	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	public void stopController() {
		handler.stopController();
	}

	
}