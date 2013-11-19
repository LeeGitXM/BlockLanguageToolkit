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
public class StatusReportingScriptFunctions  {
	private static final String TAG = "GatewayStatusReportingScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(StatusReportingScriptFunctions.class.getPackage().getName());

	/**
	 * Enable or disable a diagram.
	 * 
	 * @param path tree-path to the diagram
	 * @param flag true to enable the diagram
	 */
	public static void enableDiagram(String path,boolean flag)  {
		log.debug(TAG+String.format("enableDiagram - %s = %s",path,(flag?"true":"false")));
	}

	
	/**
	 * Report the results of a block evaluation.
	 * 
	 * @param path tree-path to the diagram
	 * @param index within the diagram of the block reporting results
	 * @param value the result of the block's computation
	 * @param port the output port on which to insert the result
	 */
	public static void reportBlockCompletion(String path,int index,Object value,String port)  {
		log.debug(TAG+String.format("reportBlockCompletion - %s:%d = %s on %s",path,index,
												value.toString(),port));	
	}
}