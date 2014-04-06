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

/**
 *  This class exposes python-callable functions used to report block status
 *  changes to the Gateway. The functions are designed for access from 
 *  python implementation of blocks..
 *  
 *  Since we are in Gateway, we can make local calls.
 */
public class BlockReportingScriptFunctions  {
	private static final String TAG = "BlockReportingScriptFunctions: ";
	private static LoggerEx log = LogUtil.getLogger(BlockReportingScriptFunctions.class.getPackage().getName());
	
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
		BlockExecutionController controller = BlockExecutionController.getInstance();
		try {
			UUID uuid = UUID.fromString(id);
			UUID parentuuid = UUID.fromString(parent);
			ProcessDiagram diagram = controller.getDiagram(parentuuid);
			if( diagram!=null) {
				ProcessBlock block = diagram.getBlock(uuid);
				QualifiedValue qv = new BasicQualifiedValue(value,new BasicQuality(quality,
						(quality.equalsIgnoreCase("good")?Quality.Level.Good:Quality.Level.Bad)));
				OutgoingNotification note = new OutgoingNotification(block,port,qv);
				controller.acceptCompletionNotification(note);
			}
			else {
				log.warnf("%s.send: no diagram found for %s",TAG,parent);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.send: one of %s or %s illegal UUID (%s)",TAG,parent,id,iae.getMessage());
		}
	}
}