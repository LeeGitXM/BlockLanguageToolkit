/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.diagnostics.designer;

import java.util.List;

import org.python.core.PyDictionary;
import org.python.core.PyList;

import com.google.gson.Gson;
import com.ils.common.PythonToJava;
import com.ils.diagnostics.common.DTProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes functions that report status after block execution.  
 *  Remote procedure calls are made to the Gateway scope to report changes
 *  after a block evaluation.
 *  @see com.ils.diagnostics.common.StatusReportingInterface
 */
public class StatusReportingScriptFunctions  {
	private static final String TAG = "StatusReportingScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(StatusReportingScriptFunctions.class.getPackage().getName());

	/**
	 * Enable or disable a diagram.
	 * 
	 * @param path tree-path to the diagram
	 * @param flag true to enable the diagram
	 */
	public static void enableDiagram(String path,boolean flag)  {
		log.debug(TAG+String.format("enableDiagram - %s = %s",path,(flag?"true":"false")));
		try {;
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(DTProperties.MODULE_ID, "enableDiagram",new Boolean(flag));
		}
		catch(Exception ge) {
			log.info(TAG+"enableDiagram: GatewayException ("+ge.getMessage()+")");
		}
	}
	
	/**
	 * Obtain the common dictionary used for storing python block instances. 
	 * NOTE: This method is ONLY available in Designer scope.
	 * 
	 * @return repository a PyDictionary containing object instances keyed by project:treepath:blockId
	 */
	public static PyDictionary getRepository() {
		PyDictionary dict = null;
		try {
			log.debug(TAG+"getRepository ...");
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(DTProperties.MODULE_ID, "getRepository");
		}
		catch(Exception ge) {
			log.info(TAG+"getRepository: GatewayException ("+ge.getMessage()+")");
		}
		return dict;
	}

	/**
	 * Define or update a list of values to be placed on the output connectors.
	 * 
	 * @param key name of the collection of models that are inter-dependent
	 * @param pyAttributes a PyList of PyDictionaries containing the output parameters
	 */
	public static void reportStatus(String key,PyList pyAttributes) throws Exception {
		List<?> attributes = new PythonToJava().pyListToArrayList(pyAttributes);
		Gson gson = new Gson();
		
		try {
			log.debug(TAG+"defineModels: PYTHON=\n"+pyAttributes);
			String json = gson.toJson(attributes);
			log.debug(TAG+"defineModels: JSON=\n"+json);
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(DTProperties.MODULE_ID, "reportStatus", key, json);
		}
		catch(Exception ge) {
			log.info(TAG+"reportStatus: GatewayException ("+ge.getMessage()+")");
		}
	}

	

}