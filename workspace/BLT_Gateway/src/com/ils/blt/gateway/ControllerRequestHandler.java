/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.TransmissionScope;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessApplication;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.ProcessFamily;
import com.ils.blt.gateway.persistence.ToolkitRecord;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.common.ClassList;
import com.ils.common.watchdog.AcceleratedWatchdogTimer;
import com.inductiveautomation.ignition.common.datasource.DatasourceStatus;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.datasource.Datasource;
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
public class ControllerRequestHandler implements ToolkitRequestHandler  {
	private final static String TAG = "ControllerRequestHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private static ControllerRequestHandler instance = null;
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	private final PythonRequestHandler pyHandler;
	private final ScriptExtensionManager sem = ScriptExtensionManager.getInstance();
    
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private ControllerRequestHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		pyHandler = new PythonRequestHandler();
	}
	

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static ControllerRequestHandler getInstance() {
		if( instance==null) {
			synchronized(ControllerRequestHandler.class) {
				instance = new ControllerRequestHandler();
			}
		}
		return instance;
	}
	/**
	 * Remove all diagrams from the controller.
	 * Cancel all tag subscriptions.
	 */
	public void clearController() {
		controller.removeAllDiagrams();
	}
	
	/**
	 * Create an instance of a named class. If the class is not found in the JVM, try Python 
	 * @param key
	 * @param className
	 * @return the instance created, else null
	 */
	public ProcessBlock createInstance(String className,UUID parentId,UUID blockId) {
		
		log.infof("%s.createInstance of %s (%s:%s)",TAG,className,(parentId==null?"null":parentId.toString()),blockId.toString());
		ProcessBlock block = null;
		try {
			Class<?> clss = Class.forName(className);
			Constructor<?> ctor = clss.getDeclaredConstructor(new Class[] {ExecutionController.class,UUID.class,UUID.class});
			block = (ProcessBlock)ctor.newInstance(BlockExecutionController.getInstance(),parentId,blockId);
		}
		catch(InvocationTargetException ite ) {
			Throwable cause = ite.getCause();
			log.warn(String.format("%s.createInstance %s: Invocation of constructor failed (%s)",TAG,
					className,(cause==null?"No cause available":cause.getLocalizedMessage())),cause); 
		}
		catch(NoSuchMethodException nsme ) {
			log.warnf("%s.createInstance %s: Three argument constructor not found (%s)",TAG,className,nsme.getMessage()); 
		}
		catch( ClassNotFoundException cnf ) {
			log.infof("%s.createInstance: Java class %s not found - trying Python",TAG,className);
			ProxyHandler ph = ProxyHandler.getInstance();
			ProcessDiagram diagram = controller.getDiagram(parentId);
			if( diagram!=null ) {
				long projectId = diagram.getProjectId();
				block = ph.createBlockInstance(className,parentId,blockId,projectId);
			}
		}
		catch( InstantiationException ie ) {
			log.warnf("%s.createInstance: Error instantiating %s (%s)",TAG,className,ie.getLocalizedMessage()); 
		}
		catch( IllegalAccessException iae ) {
			log.warnf("%s.createInstance: Security exception creating %s (%s)",TAG,className,iae.getLocalizedMessage()); 
		}
		return block;
	}

	@Override
	public boolean diagramExists(String uuidString) {
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(uuidString);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.diagramExists: Diagram UUID string is illegal (%s), creating new",TAG,uuidString);
			diagramUUID = UUID.nameUUIDFromBytes(uuidString.getBytes());
		}
		ProcessDiagram diagram = controller.getDiagram(diagramUUID);
		return diagram!=null;
	}
	@Override
	public void evaluateBlock(String diagramId, String blockId) {
		UUID diagramUUID = null;
		UUID blockUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
			blockUUID = UUID.fromString(blockId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.evaluateBlock: Diagram or block UUID string is illegal (%s, %s), creating new",TAG,diagramId,blockId);
			diagramUUID = UUID.nameUUIDFromBytes(diagramId.getBytes());
			blockUUID = UUID.nameUUIDFromBytes(blockId.getBytes());
		}
		controller.evaluateBlock(diagramUUID, blockUUID);
	}
	@Override
	public String getApplicationName(String uuid) {
		ProcessApplication app = pyHandler.getApplication(uuid);
		return app.getName();
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
		log.debugf("%s.getBlockProperties of %s (%s)",TAG,className,blockId.toString());
		BlockProperty[] results = null;
		ProcessDiagram diagram = controller.getDiagram(projectId, resourceId);
		ProcessBlock block = null;
		if( diagram!=null ) block = diagram.getBlock(blockId);
		if(block!=null) {
			results = block.getProperties();  // Existing block
			log.tracef("%s.getProperties existing %s = %s",TAG,block.getClass().getName(),results.toString());
		}
		else {
			block = createInstance(className,(diagram!=null?diagram.getSelf():null),blockId);  // Block is not (yet) attached to a diagram
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
		ProcessDiagram diagram = controller.getDiagram(parentId);
		ProcessBlock block = null;
		if( diagram!=null ) block = diagram.getBlock(blockId);
		BlockProperty property = null;
		if(block!=null) {
			property = block.getProperty(propertyName);  // Existing block
		}
		else {
			log.warnf("%s.getProperty Block not found for %s.%s",TAG,parentId.toString(),blockId.toString());
		}
		return property;
	}
	@Override
	public List<PalettePrototype> getBlockPrototypes() {
		List<PalettePrototype> results = new ArrayList<>();
		ClassList cl = new ClassList();
		List<Class<?>> classes = cl.getAnnotatedClasses(BLTProperties.BLOCK_JAR_NAME, ExecutableBlock.class,"com/ils/block/");
		for( Class<?> cls:classes) {
			log.debugf("   found block class: %s",cls.getName());
			try {
				Object obj = cls.newInstance();
				if( obj instanceof ProcessBlock ) {
					PalettePrototype bp = ((ProcessBlock)obj).getBlockPrototype();
					results.add(bp);
				}
				else {
					log.warnf("%s: Class %s not a ProcessBlock",TAG,cls.getName());
				}
			} 
			catch (InstantiationException ie) {
				log.warnf("%s.getBlockPrototypes: Exception instantiating block (%s)",TAG,ie.getLocalizedMessage());
			} 
			catch (IllegalAccessException iae) {
				log.warnf("%s.getBlockPrototypes: Access exception (%s)",TAG,iae.getMessage());
			}
			catch (Exception ex) {
				log.warnf("%s.getBlockPrototypes: Runtime exception (%s)",TAG,ex.getMessage(),ex);
			}
		}
		// Now add prototypes from Python-defined blocks
		// NOTE: We use the gateway script manager because these blocks do
		//       not yet exist in a diagram (or project).
		ProxyHandler phandler = ProxyHandler.getInstance();
		try {
			ScriptManager mgr = context.getScriptManager();
			List<PalettePrototype> prototypes = phandler.getPalettePrototypes(mgr);
			for( PalettePrototype pp:prototypes) {
				results.add(pp);
			}
		}
		catch (Exception ex) {
			log.warnf("%s.getBlockPrototypes: Runtime exception (%s)",TAG,ex.getMessage(),ex);
		}
		log.infof("%s.getBlockPrototypes: returning %d palette prototypes",TAG,results.size());
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
		Connection cxn  = controller.getConnection(projectId, resourceId, connectionId);
		return attributes;
	}
	@Override
	public String getControllerState() {
		return getExecutionState();
	}
	@Override
	public List<String> getDatasourceNames() {
		List<Datasource> sources = context.getDatasourceManager().getDatasources();
		List<String> result = new ArrayList<>();
		for( Datasource source:sources) {
			if(source.getStatus().equals(DatasourceStatus.VALID)) {
				result.add(source.getName());
			}
		}
		return result;
	}
	
	@Override
	public List getDiagramBlocksOfClass(String diagramId, String className) {
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.diagramExists: Diagram UUID string is illegal (%s), creating new",TAG,diagramId);
			diagramUUID = UUID.nameUUIDFromBytes(diagramId.getBytes());
		}
		return getDiagramBlocksOfClass(diagramUUID,className);
	}
	public List getDiagramBlocksOfClass(UUID diagramId,String className) {
		ProcessDiagram diagram = controller.getDiagram(diagramId);
		List<String> result = new ArrayList<>();
		for(ProcessBlock block:diagram.getProcessBlocks()) {
			if( block.getClassName().equalsIgnoreCase(className)) {
				result.add(block.getBlockId().toString());
			}
		}
		return result;
	}
	
	@Override
	public List<SerializableResourceDescriptor> getDiagramDescriptors(String projectName) {
		List<SerializableResourceDescriptor> descriptors = controller.getDiagramDescriptors(projectName);
		return descriptors;
	}
	
	/**
	 * When called from the gateway, we have no project. Get them all.
	 */
	public List<SerializableResourceDescriptor> getDiagramDescriptors() {
		List<SerializableResourceDescriptor> descriptors = controller.getDiagramDescriptors();
		return descriptors;
	}
	
	/**
	 * @param projectId
	 * @param resourceId
	 * @return the current state of the specified diagram as a String.
	 */
	public DiagramState getDiagramState(Long projectId,Long resourceId) {
		DiagramState state = DiagramState.ACTIVE;
		ProcessDiagram diagram = controller.getDiagram(projectId, resourceId);
		if( diagram!=null ) {
			state = diagram.getState();
		}
		return state;
	}

	
	
	public String getExecutionState() {
		return BlockExecutionController.getExecutionState();
	}
	@Override
	public String getFamilyName(String uuid) {
		ProcessFamily fam = pyHandler.getFamily(uuid);
		return fam.getName();
	}
	/**
	 * Query a block for its internal state. This allows a read-only display in the
	 * designer to be useful for block debugging.
	 * 
	 * @param diagramId
	 * @param blockId
	 * @return a SerializableBlockStateDescriptor
	 */
	public SerializableBlockStateDescriptor getInternalState(String diagramId,String blockId) {
		SerializableBlockStateDescriptor descriptor = null;
		ProcessDiagram diagram = controller.getDiagram(UUID.fromString(diagramId));
		if(diagram!=null) {
			ProcessBlock block = controller.getBlock(diagram, UUID.fromString(blockId));
			if( block!=null ) descriptor = block.getInternalStatus();
		}
		return descriptor;
	}
	
	@Override
	public Object getPropertyValue(String diagramId, String blockId,String propertyName) {
		BlockProperty property = null;
		UUID diagramUUID;
		UUID blockUUID;
		try {
			diagramUUID = UUID.fromString(diagramId);
			blockUUID = UUID.fromString(blockId);
			property = getBlockProperty(diagramUUID,blockUUID,propertyName);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getPropertyValue: Diagram or block UUID string is illegal (%s,%s),",TAG,diagramId,blockId);
		}
		return property.getValue();
	}
	public Object getPropertyValue(UUID diagramId,UUID blockId,String propertyName) {
		Object val = null;
		ProcessDiagram diagram = controller.getDiagram(diagramId);
		if(diagram!=null) {
			ProcessBlock block = controller.getBlock(diagram, blockId);
			if( block!=null ) {
				BlockProperty prop = block.getProperty(propertyName);
				if( prop!=null) val = prop.getValue();
			}
		}
		return val;

	}
	/**
	 * On a failure to find the property, an empty string is returned.
	 */
	@Override
	public String getToolkitProperty(String propertyName) {
		String value = "";
		try {
			ToolkitRecord record = context.getPersistenceInterface().find(ToolkitRecord.META, propertyName);
			if( record!=null) value =  record.getValue();
		}
		catch(Exception ex) {
			log.warnf("%s.getToolkitProperty: Exception retrieving %s (%s),",TAG,propertyName,ex.getMessage());
		}
		return value;
	}
	
	@Override
	public boolean isControllerRunning() {
		return getExecutionState().equalsIgnoreCase("running");
	}
	/**
	 * Handle the block placing a new value on its output. This minimalist version
	 * is likely called from an external source through an RPC.
	 * 
	 * @param parentId identifier for the parent
	 * @param blockId identifier for the block
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 */
	public void postValue(String parentId,String blockId,String port,String value)  {
		log.infof("%s.postValue - %s = %s on %s",TAG,blockId,value,port);
		try {
			UUID diagramuuid = UUID.fromString(parentId);
			UUID blockuuid   = UUID.fromString(blockId);
			postValue(diagramuuid,blockuuid,port,value,BLTProperties.QUALITY_GOOD) ;
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",TAG,parentId,blockId,iae.getMessage());
		}
	}
	
	/**
	 * Handle the block placing a new value on its output. This version is called from
	 * a Python implementation of a block.
	 * 
	 * @param parentuuid identifier for the parent
	 * @param blockId identifier for the block
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 */
	public void postValue(UUID parentuuid,UUID blockId,String port,String value,String quality)  {
		log.infof("%s.postValue - %s = %s (%s) on %s",TAG,blockId,value,quality,port);
		try {
			ProcessDiagram diagram = controller.getDiagram(parentuuid);
			if( diagram!=null) {
				ProcessBlock block = diagram.getBlock(blockId);
				QualifiedValue qv = new BasicQualifiedValue(value,new BasicQuality(quality,
						(quality.equalsIgnoreCase(BLTProperties.QUALITY_GOOD)?Quality.Level.Good:Quality.Level.Bad)));
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
	 * Query the ModelManager for a list of the project resources that it is currently
	 * managing. This is a debugging service.
	 * @return
	 */
	public List<SerializableResourceDescriptor> queryControllerResources() {
		return controller.queryControllerResources();
	}
	@Override
	public List<SerializableResourceDescriptor> queryDiagram(String diagramId) {
		return queryDiagramForBlocks(diagramId);
	}
	/**
	 * Query the ModelManager for a list of the project resources that it is currently
	 * managing. This is a debugging service.
	 * @return
	 */
	public List<SerializableResourceDescriptor> queryDiagramForBlocks(String diagIdString) {
		List<SerializableResourceDescriptor> descriptors = new ArrayList<>();
		UUID diagId = UUID.fromString(diagIdString);

		ProcessDiagram diagram = controller.getDiagram(diagId);
		if( diagram!=null) {
			Collection<ProcessBlock> blocks = diagram.getProcessBlocks();
			for(ProcessBlock block:blocks) {
				SerializableResourceDescriptor desc = new SerializableResourceDescriptor();
				desc.setName(block.getName());
				desc.setClassName(block.getClass().getName());
				desc.setId(block.getBlockId().toString());
				descriptors.add(desc);
			}
		}
		else {
			log.warnf("%s.queryDiagramForBlocks: no diagram found for %s",TAG,diagIdString);
		}
		return descriptors;
	}
	@Override
	public void resetBlock(String diagramId, String blockId) {
		UUID diagramUUID = null;
		UUID blockUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
			blockUUID = UUID.fromString(blockId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.resetBlock: Diagram or block UUID string is illegal (%s, %s), creating new",TAG,diagramId,blockId);
			diagramUUID = UUID.nameUUIDFromBytes(diagramId.getBytes());
			blockUUID = UUID.nameUUIDFromBytes(blockId.getBytes());
		}
		controller.resetBlock(diagramUUID, blockUUID);
	}
	@Override
	public void resetDiagram(String diagramId) {
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.resetDiagram: Diagram UUID string is illegal (%s), creating new",TAG,diagramId);
			diagramUUID = UUID.nameUUIDFromBytes(diagramId.getBytes());
		}
		BlockExecutionController.getInstance().resetDiagram(diagramUUID);
		
	}

	@Override
	public boolean resourceExists(long projectId, long resid) {
		ProcessDiagram diagram = controller.getDiagram(projectId, resid);
		log.infof("%s.resourceExists diagram %d:%d ...%s",TAG,projectId,resid,(diagram!=null?"true":"false"));
		return diagram!=null;
	}
	@Override
	public boolean sendLocalSignal(String diagramId, String command, String message, String argument) {
		Boolean success = new Boolean(true);
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.sendLocalSignal: Diagram UUID string is illegal (%s), creating new",TAG,diagramId);
			diagramUUID = UUID.nameUUIDFromBytes(diagramId.getBytes());
		}
		ProcessDiagram diagram = BlockExecutionController.getInstance().getDiagram(diagramUUID);
		if( diagram!=null ) {
			// Create a broadcast notification
			Signal sig = new Signal(command,message,argument);
			BroadcastNotification broadcast = new BroadcastNotification(diagram.getSelf(),TransmissionScope.LOCAL,sig);
			BlockExecutionController.getInstance().acceptBroadcastNotification(broadcast);
		}
		else {
			log.warnf("%s.sendLocalSignal: Unable to find diagram %s for %s command",TAG,diagramId,command);
			success = new Boolean(false);
		}
		return success;
	}
	/**
	 * Set the state of every diagram in an application to the specified value.
	 * @param appname
	 * @param state
	 */
	@Override
	public void setApplicationState(String appname, String state) {
		try {
			DiagramState ds = DiagramState.valueOf(state.toUpperCase());
			for(SerializableResourceDescriptor srd:getDiagramDescriptors()) {
				ProcessApplication app = pyHandler.getApplication(srd.getId());
				if( app==null) continue;
				if( app.getName().equals(appname)) {
					UUID diagramuuid = UUID.fromString(srd.getId());
					ProcessDiagram pd = controller.getDiagram(diagramuuid);
					if( pd!=null) {
						pd.setState(ds);    // Must notify designer
					}
				}
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.setApplicationState: Illegal state (%s) supplied (%s)",TAG,state,iae.getMessage());
		}
	}
	/**
	 * Set the values of named properties in a block. This method ignores any binding that the
	 * property may have and sets the value directly. Theoretically the value should be of the right
	 * type for the property, but if not, it can be expected to be coerced into the proper data type 
 	 * upon receipt by the block. The quality is assumed to be Good.
	 * 
	 * @param parentId
	 * @param blockId
	 * @param properties a collection of properties that may have changed
	 */
	public void setBlockProperties(UUID parentId, UUID blockId, Collection<BlockProperty> properties) {
		ProcessDiagram diagram = controller.getDiagram(parentId);
		ProcessBlock block = null;
		if( diagram!=null ) block = diagram.getBlock(blockId);
		if(block!=null) {
			for( BlockProperty property:properties ) {
				BlockProperty existingProperty = block.getProperty(property.getName());
				if( existingProperty!=null ) {
					// Update the property
					updateProperty(block,existingProperty,property);
				}
				else {
					// Need to add a new one.
					BlockProperty[] props = new BlockProperty[block.getProperties().length+1];
					int index = 0;
					for(BlockProperty bp:block.getProperties()) {
						props[index] = bp;
						index++;
					}
					props[index] = property;
				}
			}
		}
	}
	/**
	 * Set the value of a named property in a block. This method ignores any binding that the
	 * property may have and sets the value directly. Theoretically the value should be of the right
	 * type for the property, but if not, it can be expected to be coerced into the proper data type 
 	 * upon receipt by the block. The quality is assumed to be Good.
	 * 
	 * @param parentId
	 * @param blockId
	 * @param property the newly changed or added block property
	 */
	public void setBlockProperty(UUID parentId, UUID blockId, BlockProperty property) {

		ProcessDiagram diagram = controller.getDiagram(parentId);
		ProcessBlock block = null;
		if( diagram!=null ) block = diagram.getBlock(blockId);
		if(block!=null) {
			BlockProperty existingProperty = block.getProperty(property.getName());
			if( existingProperty!=null ) {
				// Update the property
				updateProperty(block,existingProperty,property);
			}
			else {
				// Need to add a new one.
				BlockProperty[] props = new BlockProperty[block.getProperties().length+1];
				int index = 0;
				for(BlockProperty bp:block.getProperties()) {
					props[index] = bp;
					index++;
				}
				props[index] = property;
			}
		}
	}
	/**
	 * The gateway context must be specified before the instance is useful.
	 * @param cntx the GatewayContext
	 */
	public void setContext(GatewayContext cntx) {
		this.context = cntx;
	}
	/**
	 * Set the state of the specified diagram. 
	 * @param projectId
	 * @param resourceId
	 * @param state
	 */
	public void setDiagramState(Long projectId,Long resourceId,String state) {
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
	/**
	 * Set the state of the specified diagram. 
	 * @param diagramId UUID of the diagram
	 * @param state
	 */
	public void setDiagramState(String diagramId,String state) {
		UUID diagramUUID = UUID.fromString(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diagramUUID);
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
	
	public void setTimeFactor(Double factor) {
		log.infof("%s.setTimeFactor: %s", TAG, String.valueOf(factor));
		AcceleratedWatchdogTimer timer = controller.getSecondaryTimer();
		timer.setFactor(factor.doubleValue());
	}
	/**
	 * We have two types of properties of interest here. The first set is found in ScriptConstants
	 * and represents scripts used for external Python interfaces to Application/Family.
	 * The second category represents database and tag interfaces for production and isolation
	 * modes.
	 */
	@Override
	public void setToolkitProperty(String propertyName, String value) {
		try {
			ToolkitRecord record = context.getPersistenceInterface().find(ToolkitRecord.META, propertyName);
			if( record==null) record = context.getPersistenceInterface().createNew(ToolkitRecord.META);
			if( record!=null) {
				record.setName(propertyName);
				record.setValue(value);
				context.getPersistenceInterface().save(record);
			}
			else {
				log.warnf("%s.setToolkitProperty: %s=%s - failed to create persistence record (%s)",TAG,propertyName,value,ToolkitRecord.META.quoteName);
			}
			sem.setModulePath(propertyName, value);  // Does nothing for properties not part of Python external interface 
			controller.clearCache();                 // Force retrieval production/isolation constants from HSQLdb on next access.
		}
		catch(Exception ex) {
			log.warnf("%s.setToolkitProperty: Exception setting %s=%s (%s),",TAG,propertyName,value,ex.getMessage());
		}
	}
	public void startController() {
		BlockExecutionController.getInstance().start(context);
	}
	public void stopController() {
		BlockExecutionController.getInstance().stop();
	}
	/**
	 * Direct blocks in all diagrams to report their status for a UI update.
	 */
	public void triggerStatusNotifications() {
		BlockExecutionController.getInstance().triggerStatusNotifications();
		log.infof("%s.triggerStatusNotifications: Complete.",TAG);
	}
			
	/** Change the properties of anchors for a block. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param anchorUpdates the complete anchor list for the block.
	 */
	public void updateBlockAnchors(String diagramId,String blockId, Collection<SerializableAnchor> anchorUpdates) {
		UUID diagramUUID = UUID.fromString(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diagramUUID);
		ProcessBlock block = null;
		UUID blockUUID = UUID.fromString(blockId);
		if( diagram!=null ) block = diagram.getBlock(blockUUID);
		if(block!=null) {
			List<AnchorPrototype> anchors = block.getAnchors();
			for( SerializableAnchor anchorUpdate:anchorUpdates ) {
				// These are undoubtedly very short lists ... do linear searches
				boolean found = false;
				for( AnchorPrototype anchor:anchors ) {
					if( anchor.getName().equalsIgnoreCase(anchorUpdate.getDisplay())) {
						anchor.setConnectionType(anchorUpdate.getConnectionType());
						found = true;
						break;
					}
				}
				if( !found ) {
					// Add previously unknown anchor
					AnchorPrototype proto = new AnchorPrototype(anchorUpdate.getDisplay(),anchorUpdate.getDirection(),anchorUpdate.getConnectionType());
					proto.setAnnotation(anchorUpdate.getAnnotation());
					proto.setHint(anchorUpdate.getHint());
					anchors.add(proto);
				}
			}
		}
	}


	/** Change the properties of anchors for a block. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param anchorUpdates the complete anchor list for the block.
	 */
	@Override
	public void updateBlockAnchors(UUID diagramUUID, UUID blockUUID,Collection<SerializableAnchor> anchorUpdates) {

		ProcessDiagram diagram = controller.getDiagram(diagramUUID);
		ProcessBlock block = null;

		if( diagram!=null ) block = diagram.getBlock(blockUUID);
		if(block!=null) {
			List<AnchorPrototype> anchors = block.getAnchors();
			for( SerializableAnchor anchorUpdate:anchorUpdates ) {
				// These are undoubtedly very short lists ... do linear searches
				boolean found = false;
				for( AnchorPrototype anchor:anchors ) {
					if( anchor.getName().equalsIgnoreCase(anchorUpdate.getDisplay())) {
						anchor.setConnectionType(anchorUpdate.getConnectionType());
						found = true;
						break;
					}
				}
				if( !found ) {
					// Add previously unknown anchor
					AnchorPrototype proto = new AnchorPrototype(anchorUpdate.getDisplay(),anchorUpdate.getDirection(),anchorUpdate.getConnectionType());
					proto.setAnnotation(anchorUpdate.getAnnotation());
					proto.setHint(anchorUpdate.getHint());
					anchors.add(proto);
				}
			}
		}
	}


	// Handle all the intricasies of a property change
	private void updateProperty(ProcessBlock block,BlockProperty existingProperty,BlockProperty newProperty) {
		if( !existingProperty.isEditable() )  return;
		
		log.debugf("%s.updateProperty old: %s, new:%s",TAG,existingProperty.toString(),newProperty.toString());
		if( !existingProperty.getBindingType().equals(newProperty.getBindingType()) ) {
			// If the binding has changed - fix subscriptions.
			controller.removeSubscription(block, existingProperty);
			existingProperty.setBindingType(newProperty.getBindingType());
			existingProperty.setBinding(newProperty.getBinding());
			controller.startSubscription(block,newProperty);
			// If the new binding is a tag write - do the write.
			if( !block.isLocked() && 
				(newProperty.getBindingType().equals(BindingType.TAG_READWRITE) ||
				 newProperty.getBindingType().equals(BindingType.TAG_WRITE))	   ) {
					controller.updateTag(block.getParentId(),newProperty.getBinding(), new BasicQualifiedValue(newProperty.getValue()));
			}	
		}
		else if( !existingProperty.getBinding().equals(newProperty.getBinding()) ) {
			// Same type, new binding target.
			controller.removeSubscription(block, existingProperty);
			existingProperty.setBinding(newProperty.getBinding());
			controller.startSubscription(block,newProperty);
			// If the new binding is a tag write - do the write.
			if( !block.isLocked() && 
					(newProperty.getBindingType().equals(BindingType.TAG_READWRITE) ||
				     newProperty.getBindingType().equals(BindingType.TAG_WRITE))	   ) {
				controller.updateTag(block.getParentId(),newProperty.getBinding(), new BasicQualifiedValue(newProperty.getValue()));
			}	
		}
		else {
			// The event came explicitly from the designer/client. Send event whether it changed or not.
			if( existingProperty.getBindingType().equals(BindingType.NONE) && newProperty.getValue()!=null   )   {
				log.debugf("%s.setProperty sending event ...",TAG);
				BlockPropertyChangeEvent event = new BlockPropertyChangeEvent(block.getBlockId().toString(),newProperty.getName(),
						existingProperty.getValue(),newProperty.getValue());
				block.propertyChange(event);
			}
		}
	}
}

