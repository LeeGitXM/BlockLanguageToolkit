/**
e *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.UUID;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * This class exposes python-callable methods that interface with the execution engine. 
 * The class is accessed via direct import imto Python
 */
public class PythonRequestHandler   {
	private static final String TAG = "PythonRequestHandler";
	private static LoggerEx log = LogUtil.getLogger(PythonRequestHandler.class.getPackage().getName());
	
	/**
	 * Handle the block placing a new value on its output.
	 * 
	 * @param parent identifier for the parent, a string version of a UUID
	 * @param id block identifier a string version of the UUID
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 */
	public void postValue(String parent,String id,String port,String value,String quality)  {
		log.infof("%s.postValue - %s = %s on %s",TAG,id,value.toString(),port);
		
		try {
			UUID uuid = UUID.fromString(id);
			UUID parentuuid = UUID.fromString(parent);
			ControllerRequestHandler.getInstance().postValue(parentuuid,uuid,port,value,quality);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",TAG,parent,id,iae.getMessage());
		}
	}
}