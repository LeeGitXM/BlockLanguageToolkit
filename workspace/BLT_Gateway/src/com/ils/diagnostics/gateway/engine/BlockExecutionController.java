/**
 *   (c) 2012  ILS Automation. All rights reserved.
 *  
 *   The block controller is designed to be called from the client
 *   via RPC. All methods must be thread safe,
 */
package com.ils.diagnostics.gateway.engine;

import java.util.Hashtable;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;



/**
 *  The block execution controller receives status updates from the RPC controller
 *  and from the resource manager regarding model changes. The changes are analyzed to
 *  determine if one or more downstream blocks are to be informed of the change.
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class BlockExecutionController  {
	private final static String TAG = "BlockExecutionController: ";
	private final LoggerEx log;
	private GatewayContext context = null;    // Must be initialized before anything works
	private static BlockExecutionController instance = null;
	private final Hashtable<Long,Hashtable<String,Object>> resourceMap;


	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private BlockExecutionController() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		resourceMap = new Hashtable<Long,Hashtable<String,Object>>();
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static BlockExecutionController getInstance() {
		if( instance==null) {
			synchronized(BlockExecutionController.class) {
				instance = new BlockExecutionController();
			}
		}
		return instance;
	}
	/**
	 * Set the gateway context. The context is required for this block tansmit
	 * notifications.
	 * @param ctxt the context
	 */
	public void setContext(GatewayContext ctxt) { this.context=ctxt; }

	/**
	 * Remove all block resources associated with a project.
	 * Presumably the project has been deleted.
	 * @param projectId the identity of a project.
	 */
	public void deleteResources(Long projectId) {
		resourceMap.remove(projectId);
	}

	/**
	 * Remove a particular block resource associated with a project.
	 * Presumably the resource has been deleted.
	 * @param projectId the identity of a project.
	 * @param resid the resource
	 */
	public void deleteResource(long projectId,long resid) {
		//resourceMap.remove(projectId);
	}


}
