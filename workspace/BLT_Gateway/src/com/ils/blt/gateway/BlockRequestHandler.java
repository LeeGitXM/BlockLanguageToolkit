/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Hashtable;
import java.util.List;
import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.control.ExecutionController;
import com.ils.block.control.OutgoingNotification;
import com.ils.blt.common.serializable.DiagramState;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.connection.Connection;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 *  This handler provides is a common class for handling requests for block properties and control
 *  of the execution engine. The requests can be expected arrive both through the scripting interface
 *  and the RPC diispatcher.In general, the calls are made to update properties 
 *  in the block objects and to trigger their evaluation.
 *  
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class BlockRequestHandler   {
	private final static String TAG = "BlockRequestHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private static BlockRequestHandler instance = null;
	protected long projectId = 0;
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private BlockRequestHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static BlockRequestHandler getInstance() {
		if( instance==null) {
			synchronized(BlockRequestHandler.class) {
				instance = new BlockRequestHandler();
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
	
	public String getExecutionState() {
		return BlockExecutionController.getExecutionState();
	}
	/**
	 * @param projectId
	 * @param resourceId
	 * @return the current state of the specified diagram as a String.
	 */
	public String getDiagramState(Long projectId,Long resourceId) {
		DiagramState state = DiagramState.ACTIVE;
		BlockExecutionController controller = BlockExecutionController.getInstance();
		ProcessDiagram diagram = controller.getDiagram(projectId, resourceId);
		if( diagram!=null ) {
			state = diagram.getState();
		}
		return state.name();
	}
	
	/**
	 * Enable or disable the specified diagram
	 * @param projectId
	 * @param resourceId
	 * @param state
	 */
	public void setDiagramState(Long projectId,Long resourceId,String state) {
		BlockExecutionController controller = BlockExecutionController.getInstance();
		ProcessDiagram diagram = controller.getDiagram(projectId, resourceId);
		if( diagram!=null && state!=null ) {
			try {
				DiagramState ds = DiagramState.valueOf(state.toUpperCase());
				diagram.setState(ds);
			}
			catch( IllegalArgumentException iae) {
				log.warnf("%s.setDiagramState: Unrecognized state(%s) sent to %s (%s)",TAG,state,diagram.getName());
			}
		}
	}
	public void startController() {
		BlockExecutionController.getInstance().start(context);
	}
	
	public void stopController() {
		BlockExecutionController.getInstance().stop();
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
			log.tracef("%s.getProperties existing %s = %s",TAG,block.getClass().getName(),results.toString());
		}
		else if(!className.startsWith("com")) {
			ProxyHandler ph = ProxyHandler.getInstance();
			block = ph.createBlockInstance(className,diagram.getSelf(),blockId);
			if(block!=null) {
				results = block.getProperties();
				log.tracef("%s.getProperties new from python %s = %s",TAG,block.getClass().getName(),results.toString());
			}
		}
		else {
			block = createInstance(className,null,blockId);  // Block is not attached to a diagram
			if(block!=null) {
				results = block.getProperties();
				log.tracef("%s.getProperties new %s = %s",TAG,block.getClass().getName(),results.toString());
			}
		}
		return results;
	}
	
	/**
	 * Query the execution controller for a specified block property. 
	 * 
	 * @param parentId UUID of the containing ProcessDiagram
	 * @param blockId UUID of the block
	 * @param propertyName name of the property
	 * @return the properties of an existing or new block.
	 */
	public BlockProperty getBlockProperty(UUID parentId,UUID blockId,String propertyName) {
		BlockExecutionController controller = BlockExecutionController.getInstance();
		ProcessDiagram diagram = controller.getDiagram(parentId);
		ProcessBlock block = null;
		if( diagram!=null ) block = diagram.getBlock(blockId);
		BlockProperty property = null;
		if(block!=null) {
			property = block.getProperty(propertyName);  // Existing block
			log.tracef("%s.getProperty %s.%s %s",TAG,diagram.getName(),block.getName(),property.toString());
		}
		else {
			log.warnf("%s.getProperty Block not found for %s.%s",TAG,parentId.toString(),blockId.toString());
		}
		return property;
	}
	
	/**
	 * Set the value of a named property in a block. This method ignores any binding that the
	 * property may have and sets the value directly. Theoretically the value should be of the right
	 * type for the property, but if not, it can be expected to be coerced into the proper data type 
 	 * upon receipt by the block. The quality is assumed to be Good.
	 * 
	 * @param parentId
	 * @param blockId
	 * @param propertyName
	 * @param value
	 */
	public void setBlockProperty(UUID parentId, UUID blockId, UUID blockUUID, String propertyName,Object value) {
		BlockExecutionController controller = BlockExecutionController.getInstance();
		ProcessDiagram diagram = controller.getDiagram(parentId);
		ProcessBlock block = null;
		if( diagram!=null ) block = diagram.getBlock(blockId);
		if(block!=null) {
			block.setProperty(propertyName, new BasicQualifiedValue(value));
			log.tracef("%s.setProperty %s.%s %s=%s",TAG,diagram.getName(),block.getName(),propertyName,value.toString());
		}
		else {
			log.warnf("%s.setProperty Block not found for %s.%s",TAG,parentId.toString(),blockId.toString());
		}
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
	
	/**
	 * Query the ModelManager for a list of the project resources that it is currently
	 * managing. This is a debugging service.
	 * @return
	 */
	public List<SerializableResourceDescriptor> queryControllerResources() {
		return BlockExecutionController.getInstance().queryControllerResources();
	}
	/**
	 * Handle the block placing a new value on its output.
	 * 
	 * @param parentuuid identifier for the parent
	 * @param blockId identifier for the block
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 */
	public void postValue(UUID parentuuid,UUID blockId,String port,String value,String quality)  {
		log.infof("%s.postValue - %s = %s on %s",TAG,blockId,value.toString(),port);
		BlockExecutionController controller = BlockExecutionController.getInstance();
		try {
			ProcessDiagram diagram = controller.getDiagram(parentuuid);
			if( diagram!=null) {
				ProcessBlock block = diagram.getBlock(blockId);
				QualifiedValue qv = new BasicQualifiedValue(value,new BasicQuality(quality,
						(quality.equalsIgnoreCase("good")?Quality.Level.Good:Quality.Level.Bad)));
				OutgoingNotification note = new OutgoingNotification(block,port,qv);
				controller.acceptCompletionNotification(note);
			}
			else {
				log.warnf("%s.postValue: no diagram found for %s",TAG,parentuuid);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",TAG,parentuuid,blockId,iae.getMessage());
		}
	}
	
	/**
	 * Create an instance of a named class. This method considers only the blocks defined in the JVM. 
	 * @param key
	 * @param className
	 * @return the instance created, else null
	 */
	public ProcessBlock createInstance(String className,UUID parentId,UUID blockId) {
		
		log.debugf("%s.createInstance of %s (%s:%s)",TAG,className,(parentId==null?"null":parentId.toString()),blockId.toString());
		ProcessBlock block = null;
		try {
			Class<?> clss = Class.forName(className);
			Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {ExecutionController.class,UUID.class,UUID.class});
			block = (ProcessBlock)ctor.newInstance(BlockExecutionController.getInstance(),parentId,blockId);
		}
		catch(InvocationTargetException ite ) {
			log.warnf("%s.createInstance %s: Invocation failed (%s)",TAG,className,ite.getMessage()); 
		}
		catch(NoSuchMethodException nsme ) {
			log.warnf("%s.createInstance %s: Three argument constructor not found (%s)",TAG,className,nsme.getMessage()); 
		}
		catch( ClassNotFoundException cnf ) {
			log.warnf("%s.createInstance: Error creating %s (%s)",TAG,className,cnf.getMessage()); 
		}
		catch( InstantiationException ie ) {
			log.warnf("%s.createInstance: Error instantiating %s (%s)",TAG,className,ie.getLocalizedMessage()); 
		}
		catch( IllegalAccessException iae ) {
			log.warnf("%s.createInstance: Security exception creating %s (%s)",TAG,className,iae.getLocalizedMessage()); 
		}
		return block;
	}

}

