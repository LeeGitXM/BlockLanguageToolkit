/**
e *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.UUID;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 * This class exposes python-callable functions that deal with properties
 * of applications, families, diagrams, blocks and connections. It also handles
 * functions of the engine itself. 
 * 
 * @see com.ils.blt.common.ApplicationScriptFunctions for the same routines available in Designer/Client scope.
 * These functions use the BlockRequestHandler which serves as a common facility for handling
 * similar requests that arrive from the the scripting interface (this) or RPC calls.
 */
public class GatewayBlockScriptFunctions   {
	private static final String TAG = "GatewayBlockScriptFunctions: ";
	public static GatewayContext context = null;   // Set in the hook class
	private static LoggerEx log = LogUtil.getLogger(GatewayBlockScriptFunctions.class.getPackage().getName());
	
	/**
	 * Handle the block placing a new value on its output.
	 * 
	 * @param parent identifier for the parent, a string version of a UUID
	 * @param id block identifier a string version of the UUID
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 */
	public static void postValue(String parent,String id,String port,String value,String quality)  {
		log.infof("%s.postValue - %s = %s on %s",TAG,id,value.toString(),port);
		
		try {
			UUID uuid = UUID.fromString(id);
			UUID parentuuid = UUID.fromString(parent);
			BlockRequestHandler.getInstance().postValue(parentuuid,uuid,port,value,quality);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",TAG,parent,id,iae.getMessage());
		}
	}
	
}