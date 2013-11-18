/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.common;

import org.python.core.PyDictionary;
import org.python.core.PyList;



/**
 *  This interface describes status reporting functions available in the designer and client.
 *  The expectation is that these will be called by the Python classes that represent 
 *  blocks in the diagram, or controllers of those classes.
 */
public interface StatusReportingInterface  {

	/**
	 * Enable or disable a diagram.
	 * 
	 * @param path tree-path to the diagram
	 * @param flag true to enable the diagram
	 */
	public void enableDiagram(String path,boolean flag);
	
	/**
	 * Obtain the common dictionary used for storing python block instances. 
	 * NOTE: This method is ONLY available in Designer scope.
	 * 
	 * @return repository a PyDictionary containing object instances keyed by project:treepath:blockId
	 */
	public PyDictionary getRepository();
	
	

	/**
	 * Define or update a list of values to be placed on the output connectors.
	 * 
	 * @param key name of the collection of models that are inter-dependent
	 * @param pyAttributes a PyList of PyDictionaries containing the output parameters
	 */
	public void reportStatus(String key,PyList pyAttributes) throws Exception;
	}