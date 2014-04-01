/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

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
public class BlockPropertiesHandler   {
	private final static String TAG = "BlockPropertiesHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private static BlockPropertiesHandler instance = null;
	protected long projectId = 0;
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private BlockPropertiesHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static BlockPropertiesHandler getInstance() {
		if( instance==null) {
			synchronized(BlockPropertiesHandler.class) {
				instance = new BlockPropertiesHandler();
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
	 * Create an instance of a named class. 
	 * @param key
	 * @param className
	 * @return the instance created, else null
	 */
	private ProcessBlock createInstance(String className,UUID parentId,UUID blockId) {
		log.debugf("%s: createInstance of %s (%s:%s)",TAG,className,parentId.toString(),blockId.toString());   // Should be updated
		ProcessBlock block = null;
		try {
			Class<?> clss = Class.forName(className);
			Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {ExecutionController.class,UUID.class,UUID.class});
			block = (ProcessBlock)ctor.newInstance(BlockExecutionController.getInstance(),parentId,blockId);
		}
		catch(InvocationTargetException ite ) {
			log.warnf("%s: createInstance %s: Invocation failed (%s)",TAG,className,ite.getMessage()); 
		}
		catch(NoSuchMethodException nsme ) {
			log.warnf("%s: createInstance %s: Three argument constructor not found (%s)",TAG,className,nsme.getMessage()); 
		}
		catch( ClassNotFoundException cnf ) {
			log.warnf("%s: createInstance: Error creating %s (%s)",TAG,className,cnf.getMessage()); 
		}
		catch( InstantiationException ie ) {
			log.warnf("%s: createInstance: Error instantiating %s (%s)",TAG,className,ie.getLocalizedMessage()); 
		}
		catch( IllegalAccessException iae ) {
			log.warnf("%s: createInstance: Security exception creating %s (%s)",TAG,className,iae.getLocalizedMessage()); 
		}
		return block;
	}

	/**
	 * Query the block controller for a block specified by the block id. If the block
	 * does not exist, create it.
	 * 
	 * @param className
	 * @param projectId
	 * @param resourceId
	 * @param blockId
	 * @return the properties of an existing or new block.
	 */
	public BlockProperty[] getBlockProperties(String className,long projectId,long resourceId, UUID blockId) {
		// If the instance doesn't exist, create one
		BlockExecutionController controller = BlockExecutionController.getInstance();
		ProcessDiagram diagram = controller.getDiagram(projectId, resourceId);
		ProcessBlock block = null;
		if( diagram!=null ) block = diagram.getBlock(blockId);
		BlockProperty[] results = null;
		if(block!=null) {
			results = block.getProperties();  // Existing block
			log.tracef("%s: getProperties existing %s = %s",TAG,block.getClass().getName(),results.toString());
		}
		else if(className.startsWith("app")) {
			ProxyHandler ph = ProxyHandler.getInstance();
			block = ph.createBlockInstance(className,diagram.getSelf(),blockId);
			if(block!=null) {
				results = block.getProperties();
				log.tracef("%s: getProperties new from python %s = %s",TAG,block.getClass().getName(),results.toString());
			}
		}
		else {		
			block = createInstance(className,diagram.getSelf(),blockId);
			if(block!=null) {
				results = block.getProperties();
				log.tracef("%s: getProperties new %s = %s",TAG,block.getClass().getName(),results.toString());
			}
		}
		return results;
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
