/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 *  This class exposes python-callable functions used to report block status
 *  changes to the Gateway.
 *  
 *  Since we are in Gateway, we can make local calls.
 */
public class BlockCompletionScriptFunctions  {
	private static final String TAG = "BlockCompletionScriptFunctions";
	private static LoggerEx log = LogUtil.getLogger(BlockCompletionScriptFunctions.class.getPackage().getName());

	
	
	/**
	 * Report the results of a block evaluation.
	 * 
	 * @param path tree-path to the diagram
	 * @param index within the diagram of the block reporting results
	 * @param value the result of the block's computation
	 * @param port the output port on which to insert the result
	 */
	public static void reportBlockCompletion(String path,int index,Object value,String port)  {
		log.debugf("%s: reportBlockCompletion - %s:%d = %s on %s",TAG,path,index,
												value.toString(),port);	
	}
}