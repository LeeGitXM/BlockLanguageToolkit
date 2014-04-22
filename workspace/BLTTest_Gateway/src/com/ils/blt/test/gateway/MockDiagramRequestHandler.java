/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.test.gateway;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Hashtable;
import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.control.ExecutionController;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.connection.Connection;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 *  This handler provides is a common class for handling requests dealing with mock diagrams.
 *  The requests can be expected arrive both through the scripting interface
 *  and the RPC diispatcher.  Handle those requests which are more than simple passthrus 
 *  to the BlockExecutionController
 *  
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class MockDiagramRequestHandler   {
	private final static String TAG = "MockDiagramRequestHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private static MockDiagramRequestHandler instance = null;
	protected long projectId = 0;
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private MockDiagramRequestHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static MockDiagramRequestHandler getInstance() {
		if( instance==null) {
			synchronized(MockDiagramRequestHandler.class) {
				instance = new MockDiagramRequestHandler();
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
}
