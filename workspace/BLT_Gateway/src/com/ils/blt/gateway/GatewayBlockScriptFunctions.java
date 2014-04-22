/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 * This class exposes python-callable functions that deal with properties
 * of applications, families, diagrams, blocks and connections. It also handles
 * functions of the engine itself. 
 * 
 * @see com.ils.blt.common.BlockScriptFunctions for the same routines available in Designer/Client scope.
 * These functions are available through the  BlockExecutionEngine.
 */
public class GatewayBlockScriptFunctions   {
	private static BlockExecutionController controller = BlockExecutionController.getInstance();
	public static GatewayContext context = null;   // Set in the hook class
	
	/**
	 * Start the block execution engine in the gateway.
	 */
	public static void startController() {
		controller.start(context);
	}

	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	public static void stopController() {
		controller.stop();
	}
}