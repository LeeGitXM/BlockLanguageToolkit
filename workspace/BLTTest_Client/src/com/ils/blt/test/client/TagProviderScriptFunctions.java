/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *   Based on sample code in the IA-scripting-module
 *   by Travis Cox.
 */
package com.ils.blt.test.client;

import java.util.Date;

import com.ils.blt.test.common.BLTTestProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes functions to create/remove SQLTag providers and to add or 
 *  delete their tags. We basically implement the TagProviderScriptingInterface,
 *  except all methods here are static.
 *  
 *  Remote procedure calls are made to the Gateway scope to produce the changes.
 */
public class TagProviderScriptFunctions  {
	private static final String TAG = "TagProviderScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(TagProviderScriptFunctions.class.getPackage().getName());

	
	/**
	 * Create a simple tag provider for SQLTags only. These tags are not associated with an OPC server.
	 * Once the provider exists, tags may be created against it.
	 * 
	 * @param name provider name. This is the name that appears in the [..] portion of a complete tag path.
	 */
	public static void createProvider(String name) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "createProvider",name );
		}
		catch(Exception ge) {
			log.info(TAG+"createProvider: GatewayException ("+ge.getLocalizedMessage()+")");
		}
	}
	
	/**
	 * Create an expression tag. Specify its provider, path and data type. We expect the provider to be one
	 * of the providers created through this scripting interface.
	 * 
	 * WARNING: This is not implemented.
	 * 
	 * @param provider name of the tag provider
	 * @param tagPath path relative to the provider (source)
	 * @param dataType valid values are "Int1", "Int2", "Int4", "Int8", "Float4", "Float8", "Boolean", "String", "DateTime", "DataSet"
	 * @param expr the expression
	 */
	public static void createExpression(String provider, String tagPath, String dataType,String expr) {
		log.debug(TAG+"createExpression: "+tagPath);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "createExpression",provider, tagPath, dataType, expr );
		}
		catch(Exception ge) {
			log.info(TAG+"createExpression: GatewayException ("+ge.getLocalizedMessage()+")");
		}
	}
	
	/**
	 * Create a tag. Specify its provider, path and data type. We expect the provider to be one
	 * of the providers created through this scripting interface.
	 * 
	 * @param provider name of the tag provider
	 * @param tagPath path relative to the provider (source)
	 * @param dataType valid values are "Int1", "Int2", "Int4", "Int8", "Float4", "Float8", "Boolean", "String", "DateTime", "DataSet"
	 */
	public static void createTag(String provider, String tagPath, String dataType) {
		log.debug(TAG+"createTag: "+tagPath);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "createTag",provider, tagPath, dataType );
		}
		catch(Exception ge) {
			log.info(TAG+"createTag: GatewayException ("+ge.getLocalizedMessage()+")");
		}
	}

	/**
	 * Remove a SQLTag given its path. This will succeed only if the tag is "owned"
	 * by the provider that is specified. This works for expression tags also.
	 * 
	 * @param provider name of the tag provider
	 * @param tagPath path relative to the provider
	 */
	public static void deleteTag(String provider,String tagPath) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "deleteTag",provider,tagPath );
		}
		catch(Exception ge) {
			log.info(TAG+"deleteTag: GatewayException ("+ge.getLocalizedMessage()+")");
		}
	}
	
	/**
	 * Shutdown the named provider. All tags associated with that provider
	 * will (should) disappear.
	 * 
	 * @param name provider name. This is the name that appears in the [..] portion of a complete tag path.
	 */
	public static void removeProvider(String name) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "removeProvider",name );
		}
		catch(Exception ge) {
			log.info(TAG+"removeProvider: GatewayException ("+ge.getLocalizedMessage()+")");
		}
	}
	
	/**
	 * Update the expression of a given expression tag. The provider is expected to be one
	 * of the providers configured through this interface.
	 * 
	 * WARNING: This is not implemented.
	 * 
	 * @param provider
	 * @param tagPath
	 * @param expr the new expression
	 */
	public static void updateExpression(String provider, String tagPath, String expr) {
		log.debug(TAG+"updateExpression: "+tagPath);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "updateExpression",provider,tagPath, expr );
		}
		catch(Exception ge) {
			log.info(TAG+"updateExpression: GatewayException ("+ge.getLocalizedMessage()+")");
		}
	}
	/**
	 * Update the value of a given tag. The value will be converted appropriately
	 * according to the datatype of the tag. The provider is expected to be one
	 * of the providers configured through this interface.
	 * 
	 * This method differs from the standard tag-setting capabilities in that
	 * the date can be specified.
	 * 
	 * @param provider
	 * @param tagPath
	 * @param value
	 */
	public static void updateTag(String provider, String tagPath, String value) {
		log.debug(TAG+"updateTag: "+tagPath);
		Date timestamp = new Date();
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTTestProperties.MODULE_ID, "updateTag",provider,tagPath, value, timestamp );
		}
		catch(Exception ge) {
			log.info(TAG+"updateTag: GatewayException ("+ge.getLocalizedMessage()+")");
		}
	}
}
