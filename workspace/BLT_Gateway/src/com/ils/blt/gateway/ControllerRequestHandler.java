/**
 *   (c) 2014-2016  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TransmissionScope;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessApplication;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.ProcessFamily;
import com.ils.blt.gateway.engine.ProcessNode;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.common.ClassList;
import com.ils.common.persistence.ToolkitProperties;
import com.ils.common.persistence.ToolkitRecordHandler;
import com.ils.common.watchdog.AcceleratedWatchdogTimer;
import com.inductiveautomation.ignition.common.datasource.DatasourceStatus;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
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
	private final GatewayScriptExtensionManager sem = GatewayScriptExtensionManager.getInstance();
	private ToolkitRecordHandler toolkitRecordHandler;
	private final UtilityFunctions fcns;
    
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private ControllerRequestHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		pyHandler = new PythonRequestHandler();
		fcns = new UtilityFunctions();
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
	@Override
	public List<SerializableResourceDescriptor> childNodes(String nodeId) {
		UUID uuid = makeUUID(nodeId);
		ProcessNode node = controller.getProcessNode(uuid);
		List<SerializableResourceDescriptor> result = new ArrayList<>();
		if( node!=null ) {
			Collection<ProcessNode> children =  node.getChildren();
			for( ProcessNode child:children ) {
				result.add(child.toResourceDescriptor());
			}
		}
		return result;
	}
	
	/**
	 * Remove all diagrams from the controller.
	 * Cancel all tag subscriptions.
	 */
	@Override
	public void clearController() {
		controller.removeAllDiagrams();
	}
	
	/**
	 * Clear any watermark on a diagram. 
	 */
	public void clearWatermark(String diagramId) {
		controller.sendWatermarkNotification(diagramId,"");
	}

	/**
	 * Create an instance of a named class. If the class is not found in the JVM, try Python 
	 * @param className class to create
	 * @param parentId diagram identifier
	 * @param blockId identifier to assign to the newly created block
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
			Throwable cause = ite.getCause();
			log.warn(String.format("%s.createInstance %s: Invocation of constructor failed (%s)",TAG,
					className,(cause==null?"No cause available":cause.getLocalizedMessage())),cause); 
		}
		catch(NoSuchMethodException nsme ) {
			log.warnf("%s.createInstance %s: Three argument constructor not found (%s)",TAG,className,nsme.getMessage()); 
		}
		catch( ClassNotFoundException cnf ) {
			log.debugf("%s.createInstance: Java class %s not found - trying Python",TAG,className);
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
	public String getApplicationName(String uuid) {
		ProcessApplication app = pyHandler.getApplication(uuid);
		return app.getName();
	}
	@Override
	public String getBlockId(String diagramId, String blockName) {
		String id = "UNKNOWN";
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
			ProcessDiagram diagram = controller.getDiagram(diagramUUID);
			for(ProcessBlock block:diagram.getProcessBlocks()) {
				if( block.getName().equalsIgnoreCase(blockName)) {
					id = block.getBlockId().toString();
					break;
				}
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getBlockId: Diagram UUID string is illegal (%s)",TAG,diagramId);
		}
		return id;
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
	@Override
	public List<BlockProperty> getBlockProperties(String className,long projectId,long resourceId, UUID blockId) {
		// If the instance doesn't exist, create one
		log.debugf("%s.getBlockProperties of %s (%s)",TAG,className,blockId.toString());
		List<BlockProperty> results = new ArrayList<>();
		ProcessDiagram diagram = controller.getDiagram(projectId, resourceId);
		ProcessBlock block = null;
		if( diagram!=null ) block = diagram.getBlock(blockId);
		BlockProperty[] props = null;
		if(block!=null) {
			props = block.getProperties();  // Existing block
			log.tracef("%s.getProperties existing %s = %s",TAG,block.getClass().getName(),props.toString());
		}
		else {
			block = createInstance(className,(diagram!=null?diagram.getSelf():null),blockId);  // Block is not (yet) attached to a diagram
			if(block!=null) {
				props = block.getProperties();
				log.tracef("%s.getProperties new %s = %s",TAG,block.getClass().getName(),props.toString());
			}
		}
		for(BlockProperty prop:props) {
			results.add(prop);
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
			List<PalettePrototype> prototypes = phandler.getPalettePrototypes();
			for( PalettePrototype pp:prototypes) {
				results.add(pp);
			}
		}
		catch (Exception ex) {
			log.warnf("%s.getBlockPrototypes: Runtime exception (%s)",TAG,ex.getMessage(),ex);
		}
		return results;
	}
	
	@Override
	public String getBlockState(String diagramId, String blockName) {
		String state = "UNKNOWN";
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
			ProcessDiagram diagram = controller.getDiagram(diagramUUID);
			for(ProcessBlock block:diagram.getProcessBlocks()) {
				if( block.getName().equalsIgnoreCase(blockName)) {
					state = block.getState().name();
					break;
				}
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getBlockState: Diagram UUID string is illegal (%s)",TAG,diagramId);
		}
		return state;
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
	

	
	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * database.  
	 * @param nodeId identifier for node
	 * @return database name
	 */
	public String getDatabaseForUUID(String nodeId) {
		// Search up the tree for a parent diagram or application. Determine the
		// state. Unless we find a diagram, we don't return any connection name.
		String db = "NONE";
		DiagramState ds = DiagramState.DISABLED;
		try {
			UUID uuid = UUID.fromString(nodeId);
			
			ProcessNode node = controller.getProcessNode(uuid);
			while( node!=null ) {
				if( node instanceof ProcessDiagram ) {
					ds = ((ProcessDiagram)node).getState();
					//log.infof("%s.getApplication, found application = %s ",TAG,app.getName());
					break;
				}
				else if( node instanceof ProcessApplication ) {
					ds = ((ProcessApplication)node).getState();
					//log.infof("%s.getApplication, found application = %s ",TAG,app.getName());
					break;
				}
				node = controller.getProcessNode(node.getParent());
			}
			if(ds.equals(DiagramState.ACTIVE)) {
				db = getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_DATABASE);
			}
			else if(ds.equals(DiagramState.ISOLATED)) {
				db = getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getApplication: %s is an illegal UUID (%s)",TAG,nodeId,iae.getMessage());
		}
		return db;
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
	/**
	 * @param diagramId String representation of the diagram's internal Id.
	 * @return a descriptor for the diagram that corresponds to that Id.
	 */
	@Override
	public SerializableResourceDescriptor getDiagram(String diagramId) {
		SerializableResourceDescriptor descriptor = null;
		UUID uuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(uuid);
		if( diagram!=null ) {
			descriptor = diagram.toResourceDescriptor();
		}
		return descriptor;
	}
	/**
	 * When called from the gateway, we have no project. Get them all.
	 */
	public List<SerializableResourceDescriptor> getDiagramDescriptors() {
		List<SerializableResourceDescriptor> descriptors = controller.getDiagramDescriptors();
		return descriptors;
	}
	// Search all diagrams for the target block.
	@Override
	public SerializableResourceDescriptor getDiagramForBlock(String blockId) {
		UUID uuid = makeUUID(blockId);
		List<ProcessDiagram> diagrams = controller.getDelegate().getDiagrams();
		for(ProcessDiagram diagram:diagrams) {
			if( diagram.getBlock(uuid)!=null) return diagram.toResourceDescriptor();
		}
		return null;
	}
	
	/**
	 * @param projectId
	 * @param resourceId
	 * @return the current state of the specified diagram as a DiagramState.
	 */
	@Override
	public DiagramState getDiagramState(Long projectId,Long resourceId) {
		DiagramState state = DiagramState.ACTIVE;
		ProcessDiagram diagram = controller.getDiagram(projectId, resourceId);
		if( diagram!=null ) {
			state = diagram.getState();
		}
		return state;
	}
	/**
	 * @param diagramId diagram identifier
	 * @return the current state of the specified diagram as a DiagramState.
	 */
	@Override
	public DiagramState getDiagramState(String diagramId) {
		DiagramState state = DiagramState.ACTIVE;
		UUID diagramuuid=makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diagramuuid);
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
	 * Use the UUID of this block to signify that this is the source.
	 * @param diagramId
	 * @param blockId
	 * @return an explanation for the block's current state
	 */
	@Override
	public String getExplanation(String diagramId,String blockId) {
		String explanation = "";
		ProcessDiagram diagram = controller.getDiagram(UUID.fromString(diagramId));
		if(diagram!=null) {
			UUID uuid = UUID.fromString(blockId);
			ProcessBlock block = controller.getBlock(diagram,uuid );
			List<UUID> members = new ArrayList<>();
			members.add(uuid);
			if( block!=null ) explanation = block.getExplanation(diagram,members); 
		}
		return explanation;
	}
	/**
	 * Query a block for its internal state. This allows a read-only display in the
	 * designer to be useful for block debugging.
	 * 
	 * @param diagramId
	 * @param blockId
	 * @return a SerializableBlockStateDescriptor
	 */
	@Override
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
	public Object getPropertyBinding(String diagramId, String blockId,String propertyName) {
		BlockProperty property = null;
		UUID diagramUUID;
		UUID blockUUID;
		try {
			diagramUUID = UUID.fromString(diagramId);
			blockUUID = UUID.fromString(blockId);
			property = getBlockProperty(diagramUUID,blockUUID,propertyName);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getPropertyBinding: Diagram or block UUID string is illegal (%s,%s),",TAG,diagramId,blockId);
		}
		String binding = property.getBinding();
		if( binding==null ) binding = "";
		return binding;
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
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockName name of the block within the diagram
	 * @return the time at which the block last changed its state
	 */
	@Override
	public Date getTimeOfLastBlockStateChange(String diagramId, String blockName) {
		Date date = null;
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
			ProcessDiagram diagram = controller.getDiagram(diagramUUID);
			for(ProcessBlock block:diagram.getProcessBlocks()) {
				if( block.getName().equalsIgnoreCase(blockName)) {
					date = block.getTimeOfLastStateChange();
					break;
				}
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getTimeOfLastBlockStateChange: Diagram UUID string is illegal (%s)",TAG,diagramId);
		}
		return date;
	}
	/**
	 * On a failure to find the property, an empty string is returned.
	 */
	@Override
	public String getToolkitProperty(String propertyName) {
		return toolkitRecordHandler.getToolkitProperty(propertyName);
	}
	@Override
	public boolean isControllerRunning() {
		return getExecutionState().equalsIgnoreCase("running");
	}
	@Override
	public boolean isAlerting(Long projectId, Long resid) {
		ProcessDiagram diagram = controller.getDiagram(projectId.longValue(), resid.longValue());
		if( diagram==null ) {
			// Node is most likely an application or family
			log.debugf("%s.isAlerting: No diagram found in project %d with id = %d",TAG,projectId.longValue(),resid.longValue());
			return false;
		}
		return pyHandler.isAlerting(diagram);
	}
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(String diagramId,String blockId,String portName) {
		List<SerializableBlockStateDescriptor> descriptors = new ArrayList<>();
		UUID diauuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diauuid);
		if( diagram!=null ) {
			ProcessBlock blk = diagram.getBlock(UUID.fromString(blockId));
			if(blk!=null) {
				List<ProcessBlock>connectedBlocks =  diagram.getConnectedBlocksAtPort(blk,portName);
				for( ProcessBlock pb:connectedBlocks ) {
					descriptors.add(pb.toDescriptor());
				}
			}
		}
		return descriptors;
	}
	
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(String diagramId,String blockName) {
		List<SerializableBlockStateDescriptor> descriptors = new ArrayList<>();
		UUID diauuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diauuid);
		if( diagram!=null ) {
			ProcessBlock blk = diagram.getBlockByName(blockName);
			if(blk!=null) descriptors = controller.listBlocksDownstreamOf(diauuid,blk.getBlockId(),false);
		}
		return descriptors;
	}
	
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksForTag(String tagpath) {
		List<SerializableBlockStateDescriptor> results = new ArrayList<>();
		List<SerializableResourceDescriptor> descriptors = controller.getDiagramDescriptors();
		for(SerializableResourceDescriptor descriptor:descriptors) {
			UUID diagId = makeUUID(descriptor.getId());

			ProcessDiagram diagram = controller.getDiagram(diagId);
			if( diagram!=null) {
				Collection<ProcessBlock> blocks = diagram.getProcessBlocks();
				for(ProcessBlock block:blocks) {
					if( block.usesTag(tagpath)) {
						results.add(block.toDescriptor());
					}
				}
			}
			else {
				log.warnf("%s.queryDiagramForBlocks: no diagram found for descriptor %s",TAG,descriptor.getId());
			}
		}
		return results;
	}
	public List<SerializableBlockStateDescriptor> listBlocksGloballyDownstreamOf(String diagramId, String blockName) {
		List<SerializableBlockStateDescriptor> descriptors = new ArrayList<>();
		UUID diauuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diauuid);
		if( diagram!=null ) {
			ProcessBlock blk = diagram.getBlockByName(blockName);
			if(blk!=null) descriptors = controller.listBlocksDownstreamOf(diauuid,blk.getBlockId(),true);
		}
		return descriptors;
	}
	public List<SerializableBlockStateDescriptor> listBlocksGloballyUpstreamOf(String diagramId, String blockName) {
		List<SerializableBlockStateDescriptor> descriptors = new ArrayList<>();
		UUID diauuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diauuid);
		if( diagram!=null ) {
			ProcessBlock blk = diagram.getBlockByName(blockName);
			if(blk!=null) descriptors = controller.listBlocksUpstreamOf(diauuid,blk.getBlockId(),true);
		}
		return descriptors;
	}
	
	/**
	 * Query the ModelManager for a list of the project resources that it is currently
	 * managing. This is a debugging service.
	 * @return
	 */
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksInDiagram(String diagramId) {
		List<SerializableBlockStateDescriptor> descriptors = new ArrayList<>();
		UUID diauuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diauuid);
		if( diagram!=null) {
			Collection<ProcessBlock> blocks = diagram.getProcessBlocks();
			for(ProcessBlock block:blocks) {
				SerializableBlockStateDescriptor desc = block.toDescriptor();
				Map<String,String> attributes = desc.getAttributes();
				attributes.put(BLTProperties.BLOCK_ATTRIBUTE_ID,block.getClass().getName());
				attributes.put(BLTProperties.BLOCK_ATTRIBUTE_ID,block.getBlockId().toString());
				descriptors.add(desc);
			}
		}
		else {
			log.warnf("%s.queryDiagramForBlocks: no diagram found for %s",TAG,diagramId);
		}
		return descriptors;
	}
	
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(String diagramId, String blockName) {
		List<SerializableBlockStateDescriptor> descriptors = new ArrayList<>();
		UUID diauuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diauuid);
		if( diagram!=null ) {
			ProcessBlock blk = diagram.getBlockByName(blockName);
			if(blk!=null) descriptors = controller.listBlocksUpstreamOf(diauuid,blk.getBlockId(),false);
		}
		return descriptors;
	}

	@Override
	public List<SerializableBlockStateDescriptor> listConfigurationErrors() {
		List<SerializableBlockStateDescriptor> result = new ArrayList<>();
		List<SerializableResourceDescriptor> descriptors = controller.getDiagramDescriptors();
		try {
			for(SerializableResourceDescriptor res:descriptors) {
				UUID diagramId = makeUUID(res.getId());
				ProcessDiagram diagram = controller.getDiagram(diagramId);
				for( ProcessBlock block:diagram.getProcessBlocks() ) {
					String problem = block.validate();
					if( problem!=null) {
						SerializableBlockStateDescriptor descriptor = block.toDescriptor();
						descriptor.getAttributes().put(BLTProperties.BLOCK_ATTRIBUTE_PATH, pathForBlock(diagramId.toString(),block.getName()));
						descriptor.getAttributes().put(BLTProperties.BLOCK_ATTRIBUTE_ISSUE, problem);
						result.add(descriptor);
					}
				}
			}
		}
		catch(Exception ex) {
			log.info(TAG+".listConfigurationErrors: Exception ("+ex.getMessage()+")",ex);
		}
		return result;
	}
	
	/**
	 * Query an application in the gateway for list of descendants down to the level of a diagram. 
	 * The list does not include the application itself.
	 * @param appName of the parent application
	 * @return a list of nodes under the named application
	 */
	public List<SerializableResourceDescriptor> listDescriptorsForApplication(String appName) {
		List<SerializableResourceDescriptor> result = new ArrayList<>();
		ProcessApplication app = controller.getDelegate().getApplication(appName);
		if(app!=null) {
			List<ProcessNode> descendants = new ArrayList<>();
			app.collectDescendants(descendants);
			for( ProcessNode child:descendants) {
				if( child instanceof ProcessApplication ) continue;
				SerializableResourceDescriptor desc = child.toResourceDescriptor();
				if( child instanceof ProcessDiagram )     desc.setType(BLTProperties.DIAGRAM_RESOURCE_TYPE);
				else if( child instanceof ProcessFamily ) desc.setType(BLTProperties.FAMILY_RESOURCE_TYPE);
				result.add(desc);
			}
		}
		
		return result;
		
	}
	/**
	 * Query a family in the gateway for list of descendants down to the level of a diagram. 
	 * @param appName of the parent application
	 * @return a list of nodes under the named application
	 */
	public List<SerializableResourceDescriptor> listDescriptorsForFamily(String appName,String famName) {
		List<SerializableResourceDescriptor> result = new ArrayList<>();
		ProcessFamily fam = controller.getDelegate().getFamily(appName,famName);
		if(fam!=null) {
			List<ProcessNode> descendants = new ArrayList<>();
			fam.collectDescendants(descendants);
			for( ProcessNode child:descendants) {
				if( child instanceof ProcessFamily ) continue;
				SerializableResourceDescriptor desc = child.toResourceDescriptor();
				if( child instanceof ProcessDiagram ) desc.setType(BLTProperties.DIAGRAM_RESOURCE_TYPE);
				result.add(desc);
			}
		}
		
		return result;
	}
	
	@Override
	public List<SerializableBlockStateDescriptor> listDiagramBlocksOfClass(String diagramId, String className) {
		UUID diagramuuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diagramuuid);
		List<SerializableBlockStateDescriptor> result = new ArrayList<>();
		if( diagram!=null ) {
			for(ProcessBlock block:diagram.getProcessBlocks()) {
				if( block.getClassName().equalsIgnoreCase(className)) {
					SerializableBlockStateDescriptor rd = block.toDescriptor();
					result.add(rd);
				}
			}
		}
		return result;
	}
	@Override
	public List<SerializableResourceDescriptor> listDiagramDescriptors(String projectName) {
		List<SerializableResourceDescriptor> descriptors = controller.getDiagramDescriptors(projectName);
		return descriptors;
	}
	/**
	 * Query the ModelManager for a list of the project resources that it is currently
	 * managing. This is a debugging service.
	 * @return
	 */
	@Override
	public List<SerializableResourceDescriptor> listResourceNodes() {
		return controller.queryControllerResources();
	}
	
	/**
	 * Do an exhaustive search for all sink blocks that have the same binding
	 * as the specified block. We cover all diagrams in the system.
	 * 
	 * @param diagramId identifier for the diagram
	 * @param blockName name of the source
	 * @return
	 */
	@Override
	public List<SerializableBlockStateDescriptor> listSinksForSource(String diagramId,String blockName) {
		List<SerializableBlockStateDescriptor> results = new ArrayList<>();
		UUID diagramuuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diagramuuid);
		ProcessBlock source = null;
		if(diagram!=null) {
			source = diagram.getBlockByName(blockName);
		}
	
		String tagPath = null;
		if( source!=null && source.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SOURCE) ) {
			BlockProperty prop = source.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
			if( prop!=null ) tagPath = fcns.providerlessPath(prop.getBinding());
		}

		if( tagPath!=null && tagPath.length()>0 ) {
			List<SerializableResourceDescriptor> descriptors = controller.getDiagramDescriptors();
			for(SerializableResourceDescriptor desc:descriptors) {
				UUID diaguuid = makeUUID(desc.getId());
				diagram = controller.getDiagram(diaguuid);
				for(ProcessBlock sink:diagram.getProcessBlocks()) {
					if( sink.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SINK) ) {
						BlockProperty prop = sink.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
						if( prop!=null && tagPath.equalsIgnoreCase(fcns.providerlessPath(prop.getBinding()))  ) {
							results.add(sink.toDescriptor());
						}
					}
				}
			}
		}
		else {
			log.warnf("%s.listSinksForSource: Block %s not found, not a source or not bound",TAG,blockName);
		}
		return results;
	}
	/**
	 * Do an exhaustive search for all source blocks that have the same binding
	 * as the specified block. We cover all diagrams in the system.
	 * @param diagramId identifier for the diagram
	 * @param blockName name of the sink
	 * @return
	 */
	@Override
	public List<SerializableBlockStateDescriptor> listSourcesForSink(String diagramId,String blockName) {
		List<SerializableBlockStateDescriptor> results = new ArrayList<>();
		UUID diagramuuid = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diagramuuid);
		ProcessBlock sink = null;
		if(diagram!=null) {
			sink = diagram.getBlockByName(blockName);
		}

		String tagPath = null;
		if( sink!=null && sink.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SINK)) {
			BlockProperty prop = sink.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
			if( prop!=null ) tagPath = fcns.providerlessPath(prop.getBinding());
		}
		
		if( tagPath!=null && tagPath.length()>0 ) {
			List<SerializableResourceDescriptor> descriptors = controller.getDiagramDescriptors();
			for(SerializableResourceDescriptor descriptor:descriptors) {
				UUID diaguuid = makeUUID(descriptor.getId());
				diagram = controller.getDiagram(diaguuid);
				for(ProcessBlock source:diagram.getProcessBlocks()) {
					if( source.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SOURCE) ) {
						BlockProperty prop = source.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
						if( prop!=null && tagPath.equalsIgnoreCase(fcns.providerlessPath(prop.getBinding()))  ) {
							results.add(source.toDescriptor());
						}
					}
				}
			}
		}
		else {
			log.warnf("%s.listSourcesForSink: Block %s not found, not a sink or not bound",TAG,blockName);
		}
		return results;
	}
	
	@Override
	public String pathForBlock(String diagramId,String blockName) {
		UUID uuid = makeUUID(diagramId);
		String path = controller.pathForNode(uuid);
		return String.format("%s:%s",path,blockName);
	}
	/** 
	 * @param nodeId UUID as a String of a node in the navigation tree
	 * @return a slash-separated path to the specified node. The path 
	 *         root is a slash representing the top node of the navigation tree.
	 */
	@Override
	public String pathForNode(String nodeId) {
		UUID uuid = makeUUID(nodeId);
		String path = controller.pathForNode(uuid);
		return path;
	}
	/**
	 * Handle the block placing a new value on its output. This minimalist version
	 * is likely called from an external source through an RPC.
	 * 
	 * @param parentId identifier for the parent
	 * @param blockId identifier for the block
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 */
	public void postValue(String parentId,String blockId,String port,String value)  {
		log.tracef("%s.postValue - %s = %s on %s",TAG,blockId,value,port);
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
		log.tracef("%s.postValue - %s = %s (%s) on %s",TAG,blockId,value,quality,port);
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
	 * Tell the block to send its current value on the outputs.
	 */
	@Override
	public void propagateBlockState(String diagramId, String blockId) {
		UUID diagramUUID = null;
		UUID blockUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
			blockUUID = UUID.fromString(blockId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.propagateBlockState: Diagram or block UUID string is illegal (%s, %s), creating new",TAG,diagramId,blockId);
			diagramUUID = UUID.nameUUIDFromBytes(diagramId.getBytes());
			blockUUID = UUID.nameUUIDFromBytes(blockId.getBytes());
		}
		controller.propagateBlockState(diagramUUID, blockUUID);
	}
	@Override
	public void resetBlock(String diagramId, String blockName) {
		UUID diagramUUID = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diagramUUID);
		if( diagram!=null) {
			ProcessBlock block = diagram.getBlockByName(blockName);
			if( block!=null ) block.reset();
			else log.warnf("%s.resetBlock: block %s not found on diagram %s",TAG,blockName,diagram.getName());
		}
		else {
			log.warnf("%s.resetBlock: no diagram found for %s",TAG,diagramId);
		}
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
	public void restartBlock(String diagramId, String blockName) {
		UUID diagramUUID = makeUUID(diagramId);
		ProcessDiagram diagram = controller.getDiagram(diagramUUID);
		if( diagram!=null) {
			ProcessBlock block = diagram.getBlockByName(blockName);
			if( block!=null ) {
				block.stop();
				block.start();
			}
			else log.warnf("%s.restartBlock: block %s not found on diagram %s",TAG,blockName,diagram.getName());
		}
		else {
			log.warnf("%s.restartBlock: no diagram found for %s",TAG,diagramId);
		}
	}
	@Override
	public boolean resourceExists(Long projectId, Long resid) {
		ProcessDiagram diagram = controller.getDiagram(projectId.longValue(), resid.longValue());
		return diagram!=null;
	}


	@Override
	public boolean sendLocalSignal(String diagramId, String command, String message, String argument) {
		return sendTimestampedSignal( diagramId, command, message, argument,new Date().getTime());
	}
	
	/**
	 * We wrap a signal into a Qualified value and send it to a particular block.
	 * @param diagramId
	 * @param blockName
	 * @param command
	 * @param message
	 * @return
	 */
	@Override
	public boolean sendSignal(String diagramId,String blockName,String command,String message) {
		boolean success = Boolean.TRUE;
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.sendSignal: Diagram UUID string is illegal (%s), creating new",TAG,diagramId);
			diagramUUID = UUID.nameUUIDFromBytes(diagramId.getBytes());
		}
		ProcessDiagram diagram = BlockExecutionController.getInstance().getDiagram(diagramUUID);
		if( diagram!=null ) {
			// Create an output notification
			Signal sig = new Signal(command,message,"");
			BroadcastNotification broadcast = new BroadcastNotification(diagram.getSelf(),blockName,new BasicQualifiedValue(sig));
			BlockExecutionController.getInstance().acceptBroadcastNotification(broadcast);
		}
		else {
			log.warnf("%s.sendSignal: Unable to find diagram %s for %s command to %s",TAG,diagramId,command,blockName);
			success = Boolean.FALSE;
		}
		return success;
	}
	@Override
	public boolean sendTimestampedSignal(String diagramId, String command, String message, String argument,long time) {
		boolean success = Boolean.TRUE;
		UUID diagramUUID = null;
		try {
			diagramUUID = UUID.fromString(diagramId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.sendTimestampedSignal: Diagram UUID string is illegal (%s), creating new",TAG,diagramId);
			diagramUUID = UUID.nameUUIDFromBytes(diagramId.getBytes());
		}
		ProcessDiagram diagram = BlockExecutionController.getInstance().getDiagram(diagramUUID);
		if( diagram!=null ) {
			// Create a broadcast notification
			Signal sig = new Signal(command,message,argument);
			BroadcastNotification broadcast = new BroadcastNotification(diagram.getSelf(),TransmissionScope.LOCAL,
					                              new BasicQualifiedValue(sig,new BasicQuality(),new Date(time)));
			BlockExecutionController.getInstance().acceptBroadcastNotification(broadcast);
		}
		else {
			log.warnf("%s.sendTimestampedSignal: Unable to find diagram %s for %s command",TAG,diagramId,command);
			success = Boolean.FALSE;
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
					updateProperty(diagram.getState(),block,existingProperty,property);
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
				updateProperty(diagram.getState(),block,existingProperty,property);
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

	/** Change the value of a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname 
	 * @param pname the changed property
	 * @param value the new value of the property. The value will be coerced into the correct data type in the gateway 
	 */
	public void setBlockPropertyValue(String diagramId,String bname,String pname,String value )  {
		ProcessDiagram diagram = null;
		UUID uuid = UUID.fromString(diagramId);
		if( uuid!=null ) diagram = controller.getDiagram(uuid);
		if( diagram!=null ) {
			ProcessBlock block = diagram.getBlockByName(bname);
			if( block!=null ) {
				BlockProperty prop = block.getProperty(pname);
				if( prop!=null ) {
					BlockPropertyChangeEvent bpe = new BlockPropertyChangeEvent(bname,pname,prop.getValue(),value);
					if( prop.getType().equals(PropertyType.BOOLEAN) ) prop.setValue(fcns.coerceToBoolean(value));
					if( prop.getType().equals(PropertyType.DOUBLE) ) prop.setValue(fcns.coerceToDouble(value));
					if( prop.getType().equals(PropertyType.INTEGER) ) prop.setValue(fcns.coerceToInteger(value));
					else  prop.setValue(fcns.coerceToString(value));
					block.propertyChange(bpe);
					controller.sendPropertyNotification(block.getBlockId().toString(),pname,new BasicQualifiedValue(prop.getValue()));
				}
				else{
					log.warnf("%s.setBlockPropertyValue: Unable to find property %s in block %s:%s",TAG,pname,diagramId,bname,diagram.getName());
				}
			}
			else{
				log.warnf("%s.setBlockPropertyValue: Unable to find block %s in diagram %s",TAG,diagramId,bname,diagram.getName());
			}
		}
		else{
			log.warnf("%s.setBlockPropertyValue: Unable to find diagram %s for block %s",TAG,diagramId,bname);
		}
	}
	public void setBlockState(String diagramId,String bname,String stateName ) {
		ProcessDiagram diagram = null;
		UUID uuid = UUID.fromString(diagramId);
		if( uuid!=null ) diagram = controller.getDiagram(uuid);
		if( diagram!=null ) {
			ProcessBlock block = diagram.getBlockByName(bname);
			if( block!=null ) {
				try {
					TruthValue state = TruthValue.valueOf(stateName.toUpperCase());
					block.setState(state);
					block.notifyOfStatus();
				}
				catch(IllegalArgumentException iae) {
					log.warnf("%s.setBlockState: State %s in block %s:%s is not a legal truth value",TAG,stateName,bname,diagram.getName());
				}
			}
			else{
				log.warnf("%s.setBlockState: Unable to find block %s in diagram %s",TAG,diagramId,bname,diagram.getName());
			}
		}
		else{
			log.warnf("%s.setBlockState: Unable to find diagram %s for block %s",TAG,diagramId,bname);
		}
	}
	
	/**
	 * The gateway context must be specified before the instance is useful.
	 * @param cntx the GatewayContext
	 */
	public void setContext(GatewayContext cntx) {
		this.context = cntx;
		toolkitRecordHandler = new ToolkitRecordHandler(context); 
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

	/**
	 * Tell the testing timer about the difference between test time
	 * and current time. Apply this automatically to the test timer
	 * @param offset the difference between test time and current time
	 *        ~ msecs. A positive number implies that the test time is
	 *        in the past.
	 */
	public void setTestTimeOffset(long offset) {
		AcceleratedWatchdogTimer timer = controller.getSecondaryTimer();
		timer.setTestTimeOffset(offset);
	}
	
	public void setTimeFactor(Double factor) {
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
		toolkitRecordHandler.setToolkitProperty(propertyName, value);
		sem.setModulePath(propertyName, value);  // Does nothing for properties not part of Python external interface 
		controller.clearCache();                 // Force retrieval production/isolation constants from HSQLdb on next access.
	}

	/**
	 * Define a watermark for a diagram. 
	 */
	public void setWatermark(String diagramId,String text) {
		controller.sendWatermarkNotification(diagramId,text);
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
		log.debugf("%s.triggerStatusNotifications: Complete.",TAG);
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
	 * @param diagramUUID the uniqueId of the parent diagram
	 * @param blockUUID the uniqueId of the block
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


	private UUID makeUUID(String name) {
		UUID uuid = null;
		try {
			uuid = UUID.fromString(name);
		}
		catch(IllegalArgumentException iae) {
			uuid = UUID.nameUUIDFromBytes(name.getBytes());
		}
		return uuid;
	}


	// Handle all the intricasies of a property change
	private void updateProperty(DiagramState ds,ProcessBlock block,BlockProperty existingProperty,BlockProperty newProperty) {
		if( !existingProperty.isEditable() )  return;
		
		log.debugf("%s.updateProperty old: %s, new:%s",TAG,existingProperty.toString(),newProperty.toString());
		if( !existingProperty.getBindingType().equals(newProperty.getBindingType()) ) {
			// If the binding has changed - fix subscriptions.
			controller.removeSubscription(block, existingProperty);
			existingProperty.setBindingType(newProperty.getBindingType());
			existingProperty.setBinding(newProperty.getBinding());
			controller.startSubscription(ds,block,newProperty);
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
			controller.startSubscription(ds,block,newProperty);
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
		// Inform the designer of the change
		controller.sendPropertyNotification(block.getBlockId().toString(), newProperty.getName(), new BasicQualifiedValue(newProperty.getValue()));
	}
}

