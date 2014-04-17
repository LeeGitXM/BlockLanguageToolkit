/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.test.gateway;

import java.util.Hashtable;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.connection.Connection;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 *  This handler provides for a specific collection of calls to  block
 *  layer from the Gateway. In general, the calls are made to update properties 
 *  in the block objects and to trigger their evaluation.
 *  
 *  This class also posts update notifications to client and designer scopes regarding
 *  those same attribute changes, expecting that they will be picked up by listeners 
 *  associated with the UI.
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class MockBlockPropertiesHandler   {
	private final static String TAG = "BlockPropertiesHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private static MockBlockPropertiesHandler instance = null;
	protected long projectId = 0;
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private MockBlockPropertiesHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static MockBlockPropertiesHandler getInstance() {
		if( instance==null) {
			synchronized(MockBlockPropertiesHandler.class) {
				instance = new MockBlockPropertiesHandler();
			}
		}
		return instance;
	}
	/**
	 * The gateway context must be specified before the instance is useful.
	 * @param cntx the GatewayContext
	 */
	public void setContext(GatewayContext cntx) {
		this.context = cntx;
	}
	
	



	
	/**
	 * Query DiagramModel for classes connected at the beginning and end of the connection to obtain a list
	 * of permissible port names. If the connection instance already exists in the Gateway model,
	 * then return the actual port connections.
	 * 
	 * @param projectId
	 * @param resourceId
	 * @param attributes
	 * @return
	 */
	public Hashtable<String,Hashtable<String,String>> getConnectionAttributes(long projectId,long resourceId,String connectionId,Hashtable<String,Hashtable<String,String>> attributes) {
		// Find the connection object
		BlockExecutionController controller = BlockExecutionController.getInstance();
		Connection cxn  = controller.getConnection(projectId, resourceId, connectionId);
		return attributes;
	}
}
