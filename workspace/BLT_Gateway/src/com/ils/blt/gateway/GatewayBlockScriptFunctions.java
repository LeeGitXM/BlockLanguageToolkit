/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.control.OutgoingNotification;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 * This class exposes python-callable functions that deal with properties
 * of applications, families, diagrams, blocks and connections. It also handles
 * functions of the engine itself. 
 * 
 * @see com.ils.blt.common.BlockScriptFunctions for the same routines available in Designer/Client scope.
 * These functions are available through the  BlockExecutionEngine.
 */
public class GatewayBlockScriptFunctions   {
	private static final String TAG = "GatewayBlockScriptFunctions: ";
	private static BlockExecutionController controller = BlockExecutionController.getInstance();
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
	public static void send(String parent,String id,String port,String value,String quality)  {
		log.infof("%s.send - %s = %s on %s",TAG,id,value.toString(),port);
		
		try {
			UUID uuid = UUID.fromString(id);
			UUID parentuuid = UUID.fromString(parent);
			BlockRequestHandler.getInstance().send(parentuuid,uuid,port,value,quality);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.send: one of %s or %s illegal UUID (%s)",TAG,parent,id,iae.getMessage());
		}
	}
	
	/**
	 * Start the block execution engine in the gateway.
	 */
	public static void startController() {
		controller.start(context);
	}

	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	public static void stopController() {
		controller.stop();
	}
}