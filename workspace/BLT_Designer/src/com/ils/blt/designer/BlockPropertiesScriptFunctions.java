/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import org.apache.log4j.Level;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes scripting functions that obtain attribute information
 *  about blocks from the Gateway.
 *  @see com.ils.BlockPropertiesInterface.common.PropertiesRequestInterface
 */
public class BlockPropertiesScriptFunctions  {
	private static final String TAG = "PropertiesRequestScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(BlockPropertiesScriptFunctions.class.getPackage().getName());
	
	/**
	 * Enable or disable a diagram.
	 * 
	 * @param path tree-path to the diagram
	 * @param flag true to enable the diagram
	 */
	public static void enableDiagram(String path,boolean flag)  {
		log.debug(TAG+String.format("enableDiagram - %s = %s",path,(flag?"true":"false")));
		try {;
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(BLTProperties.MODULE_ID, "enableDiagram",new Boolean(flag));
		}
		catch(Exception ge) {
			log.info(TAG+"enableDiagram: GatewayException ("+ge.getMessage()+")");
		}
	}
	/**
	 * Obtain a keyed-list of attribute-value pairs for the block identified by the specified.
	 * key. On the Gateway side, a block instance is created if it does not already exist.
	 * In the case where a block instance is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param key a string representing the id of the cell within the project.
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the block object.
	 */
	public static String getBlockAttributes(String key,String json) throws Exception {
		log.info(TAG+"getBlockAttributes:"+key+"="+json);
		String result = "";
		try {
			result = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockAttributes",key,json );
		}
		catch(Exception ge) {
			log.warn(TAG+"getAttributes: GatewayException ("+ge.getMessage()+")");
		}
		log.info(TAG+"getBlockAttributes: returned .. \n"+result);
		return result;
	}
	
	/**
	 * Obtain a keyed list of attribute-value pairs for the connection represented by the supplied.
	 * key. On the Gateway side, a connection instance is created if it does not already exist.
	 * In the case where a connection is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param key a string representing the id of the cell within the project.
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the connection.
	 */
	public static String getConnectionAttributes(String key,String json) throws Exception {
		log.debug(TAG+"getConnectionAttributes:"+key+"="+json);
		String result = "";
		try {
			result = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getConnectionAttributes",key,json );
		}
		catch(Exception ge) {
			log.info(TAG+"getConnectionAttributes: GatewayException ("+ge.getMessage()+")");
		}
		log.debug(TAG+"getConnectionAttributes: returned .. \n"+result);
		return result;
	}

	/**
	 * Obtain a list of keyed attribute-value pairs (that is a list of dictionaries) containing
	 * default values of attributes for every executable block class. These attributes may include
	 * static elements as well.
	 * 
	 * @return a string representing a JSON document containing a list of keyed attribute dictionaries
	 * 								 corresponding to all executable block classes.
	 */
	public static String getPaletteBlockAttributes() {
		// TODO Auto-generated method stub
		return null;
	}
	/**
	 * Obtain a list of keyed attribute-value pairs (that is a list of dictionaries) containing
	 * default values of attributes for every connection type.
	 * 
	 * @return a string representing a JSON document containing a list of keyed attribute dictionaries
	 * 								 corresponding to all connection classes.
	 */
	public static String getPaletteConnectionAttributes() {
		// TODO Auto-generated method stub
		return null;
	}
}