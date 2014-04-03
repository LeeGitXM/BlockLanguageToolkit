/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.List;
import java.util.Set;

import com.ils.block.common.PalettePrototype;


/**
 * This class exposes python-callable functions that deal with properties
 * of applications, families, diagrams, blocks and connections.
 * 
 * Where applicable, we make use of the BlockRequestHandler to perform the requests.
 */
public class BlockScriptFunctions   {
	private static BlockRequestHandler handler = new BlockRequestHandler();

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
}