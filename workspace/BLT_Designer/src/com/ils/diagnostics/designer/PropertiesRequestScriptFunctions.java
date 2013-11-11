/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.diagnostics.designer;

import com.ils.diagnostics.common.DTProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes scripting functions that obtain attribute information
 *  about blocks from the Gateway.
 *  @see com.ils.diagnostics.common.PropertiesRequestInterface
 */
public class PropertiesRequestScriptFunctions  {
	private static final String TAG = "PropertiesRequestScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(PropertiesRequestScriptFunctions.class.getPackage().getName());
	
	/**
	 * Obtain a list of attribute-value pairs for the class represented by this block.
	 * On the Python side, a class instance is created if it does not already exist.
	 * In the case where a class instance is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param key a string uniquely representing the cell within the project.
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the block object.
	 */
	public static String getBlockAttributes(String key,String json) throws Exception {
		log.info(TAG+"getBlockAttributes:"+key+"="+json);
		String result = "";
		try {
			result = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					DTProperties.MODULE_ID, "getBlockAttributes",key,json );
		}
		catch(Exception ge) {
			log.warn(TAG+"getAttributes: GatewayException ("+ge.getMessage()+")");
		}
		log.info(TAG+"getBlockAttributes: returned .. \n"+result);
		return result;
	}
	
	/**
	 * Obtain a list of attribute-value pairs for the class represented by this connection.
	 * On the Python side, a class instance is created if it does not already exist.
	 * In the case where a class instance is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param key a string uniquely representing the cell within the project.
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the connection.
	 */
	public static String getConnectionAttributes(String key,String json) throws Exception {
		log.debug(TAG+"getConnectionAttributes:"+key+"="+json);
		String result = "";
		try {
			result = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					DTProperties.MODULE_ID, "getConnectionAttributes",key,json );
		}
		catch(Exception ge) {
			log.info(TAG+"getConnectionAttributes: GatewayException ("+ge.getMessage()+")");
		}
		log.debug(TAG+"getConnectionAttributes: returned .. \n"+result);
		return result;
	}
	
	
	/**
	 * Obtain a list of attribute-value pairs for the class represented by this connection.
	 * On the Python side, a class instance is created if it does not already exist.
	 * In the case where a class instance is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param key a string uniquely representing the cell within the project.
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the block object.
	 */
	public static String getConnections(String key,String json) throws Exception {
		log.debug(TAG+"getConnections:"+key+"="+json);
		String result = "";
		try {
			result = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					DTProperties.MODULE_ID, "getConnections",key,json );
		}
		catch(Exception ge) {
			log.info(TAG+"getConnections: GatewayException ("+ge.getMessage()+")");
		}
		log.debug(TAG+"getConnections: returned .. \n"+result);
		return result;
	}
}