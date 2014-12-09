/**
e *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.UUID;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * This class exposes python-callable requests directed at the execution engine. 
 * The class is accessed in Python via direct import 
 */
public class PythonRequestHandler   {
	private static final String TAG = "PythonRequestHandler";
	private static LoggerEx log = LogUtil.getLogger(PythonRequestHandler.class.getPackage().getName());
	
	/**
	 * Handle the block placing a new value on its output. The input may be PyObjects.
	 * 
	 * @param parent identifier for the parent, a string version of a UUID
	 * @param id block identifier a string version of the UUID
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 */
	public void postValue(String parent,String id,String port,String value,String quality)  {
		log.infof("%s.postValue - %s = %s (%s) on %s",TAG,id,value.toString(),quality.toString(),port);
		
		try {
			UUID uuid = UUID.fromString(id);
			UUID parentuuid = UUID.fromString(parent);
			ControllerRequestHandler.getInstance().postValue(parentuuid,uuid,port,value,quality);
			BlockExecutionController controller = BlockExecutionController.getInstance();
			controller.sendConnectionNotification(id, port, new BasicQualifiedValue(value));
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",TAG,parent,id,iae.getMessage());
		}
	}
}