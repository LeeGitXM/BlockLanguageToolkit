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
import com.ils.block.common.BlockConstants;
import com.ils.block.common.BlockProperty;
import com.ils.block.control.ExecutionController;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.proxy.ProxyBlock;
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
public class PropertiesUpdateHandler   {
	private final static String TAG = "PropertiesUpdateHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private static PropertiesUpdateHandler instance = null;
	protected long projectId = 0;
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private PropertiesUpdateHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static PropertiesUpdateHandler getInstance() {
		if( instance==null) {
			synchronized(PropertiesUpdateHandler.class) {
				instance = new PropertiesUpdateHandler();
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
	public ProcessBlock createInstance(long projectId,long resourceId,String blockId,String className) {
		log.debugf("%s: createInstance of %s (%d,%d,%d)",TAG,className,projectId,resourceId,blockId);   // Should be updated
		ProcessBlock block = null;
		try {
			Class<?> clss = Class.forName(className);
			Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {ExecutionController.class,long.class,long.class,String.class});
			block = (ProcessBlock)ctor.newInstance(BlockExecutionController.getInstance(),projectId,resourceId,blockId);
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
	 * Extract 
	 * @param key
	 * @param attributes
	 * @return
	 */
	public Hashtable<String,BlockProperty> getAttributes(ProcessBlock block,Hashtable<String,BlockProperty> attributes) {
		if( block==null) return attributes;

		Hashtable<String,BlockProperty> properties = block.getProperties();
		for( String key:attributes.keySet()) {
			if( !properties.containsKey(key) ) {
				properties.put(key, attributes.get(key));
			}
		}
		return properties;
	}

	/**
	 * Query the model resource manager for a block specified by the project, resource and block id. If the block
	 * does not exist, return null;
	 * 
	 * @param attributes already known
	 * @return the attribute table appropriately enhanced.
	 */
	public Hashtable<String,BlockProperty> getBlockProperties(Long projectId,Long resourceId,UUID blockId) {
		// If the instance doesn't exist, create one
		BlockExecutionController controller = BlockExecutionController.getInstance();
		ProcessBlock block = controller.getDelegate().getBlock(projectId, resourceId, blockId);
		Hashtable<String,BlockProperty> results = null;
		if(block!=null) {
			results = block.getProperties();
		}
		
		return results;
	}
	
	/**
	 * Query DiagramModel for classes connected at the beginning and end of the connection to obtain a list
	 * of permissible port names. If the connection instance already exists in the Gateway model,
	 * then return the actual port connections.
	 * 
	 * @param attributes
	 * @return
	 */
	public Hashtable<String,Hashtable<String,String>> getConnectionAttributes(long projectId,long resourceId,String connectionId,Hashtable<String,Hashtable<String,String>> attributes) {
		// Find the connection object
		BlockExecutionController controller = BlockExecutionController.getInstance();
		Connection cxn  = controller.getDelegate().getConnection(projectId, resourceId, connectionId);
		return attributes;
	}
}
