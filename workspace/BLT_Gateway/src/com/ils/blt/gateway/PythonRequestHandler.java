/**
e *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.UUID;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessApplication;
import com.ils.blt.gateway.engine.ProcessFamily;
import com.ils.blt.gateway.engine.ProcessNode;
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
	 * Traverse the parent nodes until we find an Application. If there is none in
	 * our ancestry, return null.
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @return the ancestrial application
	 */
	public ProcessApplication getApplication(String parent)  {
		log.infof("%s.getApplication, diagram = %s ",TAG,parent);
		ProcessApplication app = null;
		try {
			UUID parentuuid = UUID.fromString(parent);
			BlockExecutionController controller = BlockExecutionController.getInstance();
			ProcessNode node = controller.getProcessNode(parentuuid);
			while( node!=null ) {
				if( node instanceof ProcessApplication ) {
					app = (ProcessApplication)node;
					break;
				}
				node = controller.getProcessNode(node.getParent());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getApplication: %s is an illegal UUID (%s)",TAG,parent,iae.getMessage());
		}
		return app;
	}
	
	/**
	 * Traverse the parent nodes until we find an Application. If there is none in
	 * our ancestry, return null.
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @return the ancestrial family
	 */
	public ProcessFamily getFamily(String parent)  {
		log.infof("%s.getFamily, diagram = %s ",TAG,parent);
		ProcessFamily fam = null;
		try {
			UUID parentuuid = UUID.fromString(parent);
			BlockExecutionController controller = BlockExecutionController.getInstance();
			ProcessNode node = controller.getProcessNode(parentuuid);
			while( node!=null ) {
				if( node instanceof ProcessFamily ) {
					fam = (ProcessFamily)node;
					break;
				}
				node = controller.getProcessNode(node.getParent());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getFamily: %s is an illegal UUID (%s)",TAG,parent,iae.getMessage());
		}
		return fam;
	}
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