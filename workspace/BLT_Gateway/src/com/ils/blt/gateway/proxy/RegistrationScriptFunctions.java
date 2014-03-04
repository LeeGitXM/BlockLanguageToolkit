/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

import org.python.core.PyObject;

/**
 *  This class exposes python-callable functions used to register a project with the Gateway.
 *  The registrations identify the project as a handler of a variety of features dealing
 *  with blocks implemented in Python. 
 *  
 *  Since we are in Gateway, we can make local calls.
 */
public class RegistrationScriptFunctions   {

	/**
	 * Register a python callback function. These allow the gateway module code to interact
	 * with block definitions stored in a Python scripting module. Presumably this call is
	 * embedded in a Gateway startup script. 
	 * 
	 * If multiple projects register to handle the same module name, then one of the projects
	 * will be selected indeterminately. The module must start with app.block.
	 * 
	 * @param type a string denoting the callback kind. Valid values are found in BLTProperties.
	 * @param project name of the project that is the block code repository
	 * @param module the python code module. Must be in package app.block.
	 * @param variable the name of the global variable that will hold both function arguments and results..
	 */
	public static void register(String type,String project,String module,String variable) {
		ProxyHandler.getInstance().register(type,project,module,variable);
	}

	/**
	 * Save a python block object into a persistent object. 
	 * NOTE: This method is ONLY available in Gateway scope because the 
	 * objects stored in the repository were created in Gateway scripts.
	 * 
	 * @param pyobj the Python object that is the block to save
	 * @param projectId
	 * @param diagramId
	 * @param blockId
	 */
	public static void saveObject(PyObject pyobg,long projectId,long diagramId,String blockId) {
	}
	/**
	 * Obtain the common dictionary used for storing python block instances. 
	 * NOTE: This method is ONLY available in Gateway scope because the 
	 * objects stored in the repository were created in Gateway scripts.
	 * 
	 * @param projectId
	 * @param diagramId
	 * @param blockId
	 * @return the Python object that is the block that is restored
	 */
	public static PyObject restoreObject(long projectId,long diagramId,String blockId) {
		return null;
	}
}