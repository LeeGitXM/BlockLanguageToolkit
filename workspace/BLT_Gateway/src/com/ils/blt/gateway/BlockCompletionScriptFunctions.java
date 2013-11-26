/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes python-callable functions used to report block status
 *  changes to the Gateway.
 *  
 *  Since we are in Gateway, we can make local calls.
 */
public class BlockCompletionScriptFunctions  {
	private static final String TAG = "GatewayStatusReportingScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(BlockCompletionScriptFunctions.class.getPackage().getName());

	
	/**
	 * Report the results of a block evaluation.
	 * 
	 * @param key block identifier, includes the tree-path to the diagram and block-id within the diagram
	 * @param value the result of the block's computation
	 * @param port the output port on which to insert the result
	 */
	public static void reportBlockCompletion(String key,Object value,String port)  {
		log.debug(TAG+String.format("reportBlockCompletion - %s = %s on %s",key,
												value.toString(),port));	
	}
}