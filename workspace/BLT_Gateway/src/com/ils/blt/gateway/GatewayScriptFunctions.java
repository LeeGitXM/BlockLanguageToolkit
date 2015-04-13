/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.List;


/**
 * This class exposes python-callable functions in the Gateway scope. Currently
 * the main user of these is the test framework. These are a subset of functions
 * available in Client/Designer scope.  All requests are delegated to the 
 * same request handler as the GatewayRpcDispatcher.
 * 
 * These calls are available from any Ignition scope.
 */
public class GatewayScriptFunctions   {
	private static ControllerRequestHandler handler = ControllerRequestHandler.getInstance();
	private static PythonRequestHandler pyhandler = new PythonRequestHandler();
	
	/**
	 * Query the gateway for list of diagrams. 
	 * 
	 * @param projectName
	 * @return a list of tree-paths to the diagrams saved 
	 *         (ie. known to the Gateway).
	 */
	@SuppressWarnings("rawtypes")
	public static List getDiagramDescriptors() {
		return handler.getDiagramDescriptors();
	}
	
	/**
	 * The Python request handler is made available to
	 * every block that is implemented in Python. The
	 * handler provides facilities to acquire database and
	 * tag provider references, and to submit block outputs.
	 * 
	 * The handler also provides access to diagram and block objects.
	 * 
	 * @return the python request handler.
	 */
	public static PythonRequestHandler getHandler() {
		return pyhandler;
	}
	/**
	 * Execute reset() on every block inside the controller
	 */
	public static void resetDiagram(String diagramId) {
		handler.resetDiagram(diagramId);
	}
	
	/**
	 * Set the state for every diagram under the named application.
	 * @param app name of the application
	 * @param state new state of the diagrams
	 */
	public static void setApplicationState(String app,String state) {
		handler.setApplicationState(app,state);
	}

	/**
	 * Set a clock rate factor. This must NOT be exercised in a production environment.
	 * This is a hook for testing only.
	 * @param factor the amount to speed up or slow down the clock.
	 */
	public static void setTimeFactor(Double factor) {
		handler.setTimeFactor(factor);
	}
}