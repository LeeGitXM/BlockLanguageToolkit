/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.gateway;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 *  The RPC Dispatcher is the point of entry for incoming RPC requests.
 *  Its purpose is simply to parse out a request and send it to the
 *  right handler. This class supports the aggregate of RPC interfaces.
 *  
 *  Make use of the ControllerRequestHandler as a delegate so as to provide
 *  a common handler for both the RPC and scripting interfaces.
 *  
 *  Note: We cannot implement ToolkitRequestHandler exactly because
 *        all returns must be serializable. But we come close.
 */
public class GatewayRpcDispatcher   {
	private static String CLSS = "GatewayRpcDispatcher";
	private final LoggerEx log;
	private final ControllerRequestHandler requestHandler;

	/**
	 * Constructor. There is a separate dispatcher for each project.
	 * @param ctx Gateway context
	 */
	public GatewayRpcDispatcher(GatewayContext ctx) {
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.requestHandler = ControllerRequestHandler.getInstance();
	}
	
	public List<SerializableResourceDescriptor> childNodes(ProjectResourceId nodeId) {
		return requestHandler.childNodes(nodeId);
	}
	
	/**
	 * Clear any watermark on a diagram. 
	 */
	public void clearWatermark(ProjectResourceId diagramId) {
		requestHandler.clearWatermark(diagramId);
	}
	
	
	public void clearController() {
		requestHandler.clearController();
	}
	public void createTag(String projectName,DataType type,String path) {
		requestHandler.createTag(projectName,type, path);
	}
	public void deleteTag(String projectName,String path) {
		requestHandler.deleteTag(projectName,path);
	}
	/**
	 * This should always succeed because we create a block in the gateway whenever we 
	 * create one from the palette.
	 * @param id diagram id as a string
	 * @return True if we've discovered the specified block.
	 */
	public Boolean diagramExists(ProjectResourceId id) {
		return requestHandler.diagramExists(id);
	}

	public String getApplicationName(ProjectResourceId id) {
		return requestHandler.getApplicationName(id);
	}
	
	/**
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockName name of the block within the diagram
	 * @return the id of the specified block.
	 */
	public String getBlockId(ProjectResourceId diagramId, String blockName) {
		return requestHandler.getBlockId(diagramId,blockName);
	}
	/**
	 * Query the specified block for its properties. If the block does not exist, create it, given the
	 * specified class name. In the case of a new block, its diagram may also need to be created. 
	 * 
	 * @param resourceId resource identifier
	 * @param blockId block UUID as a string
	 * @param className of the block
	 * @return properties for the block
	 */
	public List<String> getBlockProperties(String className,ProjectResourceId resourceId,String blockId) {
		log.debugf("%s.getBlockProperties: %s %d:%d %s",CLSS,className,resourceId,blockId);
		UUID blockUUID = null;
		try {
			blockUUID = UUID.fromString(blockId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getBlockProperties: Block UUID string is illegal (%s), creating new",CLSS,blockId);
			blockUUID = UUID.nameUUIDFromBytes(blockId.getBytes());
		}
		List<BlockProperty> propertyList = requestHandler.
					getBlockProperties(className,resourceId,blockUUID);
		List<String> result = null;
		if( propertyList!=null ) {
			result = new ArrayList<String>();
			for( BlockProperty prop:propertyList ) {
				// Python can return some nulls in the array
				if( prop!=null ) {
					result.add(prop.toJson());
				}
			}			
		}
		else {
			log.warnf("%s: getBlockProperties: %s block %d:%s has no properties",CLSS,className,resourceId.getProjectName(),
					resourceId.getResourcePath().getPath().toString());
		}
		if( result!=null) log.debugf("%s.getBlockProperties: %s = %s",CLSS,className,result.toString());
		return result;
	}
	/** The blocks implemented in Java are expected to reside in a jar named "block-definition.jar".
	 *  We add the blocks implemented in Python to this list. We consider only classes that are in
	 *  a "com/ils/block" package.
	 *  
	 *  @return a list of prototypes suitable for constructing a palette
	 */
	public List<String> getBlockPrototypes() {
		List<String> results = new ArrayList<String>();
		List<PalettePrototype> prototypes = requestHandler.getBlockPrototypes();
		for(PalettePrototype pp:prototypes) {
			results.add(pp.toJson());
		}
		return results;
	}
	
	public String getBlockState(ProjectResourceId diagramId, String blockName) {
		return requestHandler.getBlockState(diagramId, blockName);
	}

	public String getControllerState() {
		return requestHandler.getExecutionState();
	}

	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * database.  
	 * @param id of the subject node, a ProjectResourceId
	 * @return database name
	 */
	public String getDatabaseForId(ProjectResourceId id) {
		return requestHandler.getDatabaseForId(id);
	}
	

	public List<String> getDatasourceNames() {
		return requestHandler.getDatasourceNames();
	}
	
	/**
	 * @param diagramId String representation of the diagram's internal Id.
	 * @return a descriptor for the diagram that corresponds to that Id.
	 */
	public SerializableResourceDescriptor getDiagram(ProjectResourceId diagramId) {
		return requestHandler.getDiagram(diagramId);
	}
	/**
	 * @param projectName name of the project
	 * @return a list of descriptors for the diagrams in the project.
	 */
	public List<String> getDiagramDescriptors(String projectName) {
		List<String> results = new ArrayList<String>();
		List<SerializableResourceDescriptor> descriptors = requestHandler.listDiagramDescriptors(projectName);
		ObjectMapper mapper = new ObjectMapper();
		for(SerializableResourceDescriptor descriptor:descriptors) {
			try {
				String json =  mapper.writeValueAsString(descriptor);
				results.add(json);
			} 
			catch (JsonParseException jpe) {
				log.warnf("%s: getDiagramDescriptors: parsing exception (%s)",CLSS,jpe.getLocalizedMessage());
			} 
			catch (JsonMappingException jme) {
				log.warnf("%s: getDiagramDescriptors: mapping exception(%s)",CLSS,jme.getLocalizedMessage());
			} 
			catch (IOException ioe) {
				log.warnf("%s: getDiagramDescriptors: io exception(%s)",CLSS,ioe.getLocalizedMessage());
			}

		}
		return results;
	}
	public SerializableResourceDescriptor getDiagramForBlock(String blockId) {
		return requestHandler.getDiagramForBlock(blockId);
	}

	public String getDiagramState(ProjectResourceId resourceId) {
		return requestHandler.getDiagramState(resourceId).name();
	}

	/**
	 * @return an explanation for the state of a block.
	 */
	public String getExplanation(ProjectResourceId diagramId,String blockId) {
		return requestHandler.getExplanation(diagramId,blockId);
	}
	public String getFamilyName(ProjectResourceId id) {
		return requestHandler.getFamilyName(id);
	}
	/**
	 * @return the hostname for the gateway
	 */
	public String getHostname() {
		String hostname = "localhost"; 
		try {
			InetAddress host = InetAddress.getLocalHost();
			hostname = host.getHostName();
		} 
		catch (UnknownHostException ex) {
			log.warnf("%s: getHostname: unknown host exception (%s)",CLSS,ex.getLocalizedMessage());
		}
		return hostname;
	}
	/**
	 * Query a block for its internal state. This allows a read-only display in the
	 * designer to be useful for block debugging.
	 * 
	 * @param diagramId
	 * @param blockId
	 * @return a JSON-serialized SerializableBlockStateDescriptor
	 */
	public String getInternalState(ProjectResourceId diagramId,String blockId) {
		SerializableBlockStateDescriptor desc = requestHandler.getInternalState(diagramId,blockId);
		ObjectMapper mapper = new ObjectMapper();
		String json = "";
		try {
			json = mapper.writeValueAsString(desc);
		}
		catch (JsonProcessingException jpe) {
			log.warnf("%s.getInternalState: Exception (%s)",CLSS,jpe.getLocalizedMessage());
		}
		return json;
	}
	public Object getPropertyBinding(ProjectResourceId diagramId,String blockId,String propertyName) {
		return requestHandler.getPropertyBinding(diagramId, blockId, propertyName);
	}
	
	public Object getPropertyValue(ProjectResourceId diagramId,String blockId,String propertyName) {
		return requestHandler.getPropertyValue(diagramId, blockId, propertyName);
	}
	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * provider.  
	 * @param uuid id of the subject node as a String
	 * @return provider name
	 */
	public String getProviderForId(ProjectResourceId id) {
		return requestHandler.getProviderForId(id);
	}
	public Date getTimeOfLastBlockStateChange(ProjectResourceId diagramId, String blockName) {
		return requestHandler.getTimeOfLastBlockStateChange(diagramId,blockName);
	}
	public String getProjectToolkitProperty(String projectName,String propertyName) {
		return requestHandler.getProjectToolkitProperty(projectName,propertyName);
	}
	public String getToolkitProperty(String propertyName) {
		return requestHandler.getToolkitProperty(propertyName);
	}
	/**
	 * @return the configured browser path (for Windows) from the ORM database HelpRecord 
	 */
	public String getWindowsBrowserPath() {
		return requestHandler.getWindowsBrowserPath();
	}

    public Boolean isAlerting(ProjectResourceId resourceId) {
    	boolean result = requestHandler.isAlerting(resourceId);
    	return result;
    }

	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(ProjectResourceId diagramId,String blockId,String portName) {
		return requestHandler.listBlocksConnectedAtPort(diagramId,blockId,portName);
	}
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(ProjectResourceId diagramId, String blockName) {
		return requestHandler.listBlocksDownstreamOf(diagramId, blockName);
	}
	
	public List<SerializableBlockStateDescriptor> listBlocksForTag(String projectName,String tagpath) {
		return requestHandler.listBlocksForTag(projectName,tagpath);
	}
	public List<SerializableBlockStateDescriptor> listBlocksGloballyDownstreamOf(ProjectResourceId diagramId, String blockName) {
		return requestHandler.listBlocksGloballyDownstreamOf(diagramId, blockName);
	}
	public List<SerializableBlockStateDescriptor> listBlocksGloballyUpstreamOf(ProjectResourceId diagramId, String blockName) {
		return requestHandler.listBlocksGloballyUpstreamOf(diagramId, blockName);
	}
	public List<SerializableBlockStateDescriptor> listBlocksInDiagram(ProjectResourceId diagramId) {
		return requestHandler.listBlocksInDiagram(diagramId);
	}
	public List<SerializableBlockStateDescriptor> listBlocksOfClass(String projectName,String className) {
		return requestHandler.listBlocksOfClass(projectName,className);
	}
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(ProjectResourceId diagramId, String blockName) {
		return requestHandler.listBlocksUpstreamOf(diagramId, blockName);
	}
	
	public List<SerializableBlockStateDescriptor> listConfigurationErrors() {
		return requestHandler.listConfigurationErrors();
	}
	public List<SerializableBlockStateDescriptor> listSubscriptionErrors() {
		return requestHandler.listSubscriptionErrors();
	}
	public List<SerializableBlockStateDescriptor> listUnresponsiveBlocks(Double hours, String clss) {
		return requestHandler.listUnresponsiveBlocks(hours.doubleValue(),clss);
	}
	public List<SerializableBlockStateDescriptor> listDiagramBlocksOfClass(ProjectResourceId diagramId, String className) {
		return requestHandler.listDiagramBlocksOfClass(diagramId, className);
	}
	public List<SerializableResourceDescriptor> listDiagramDescriptors(String projectName) {
		return requestHandler.listDiagramDescriptors(projectName);
	}

	public List<SerializableResourceDescriptor> listResourceNodes() {
		return requestHandler.listResourceNodes();
	}

	public List<SerializableBlockStateDescriptor> listSinksForSource(ProjectResourceId diagramId,String blockId) {
		return requestHandler.listSinksForSource(diagramId,blockId);
	}

	public List<SerializableBlockStateDescriptor> listSourcesForSink(ProjectResourceId diagramId,String blockId) {
		return requestHandler.listSourcesForSink(diagramId,blockId);
	}
	
	public String pathForBlock(ProjectResourceId diagramId,String blockName) {
		return requestHandler.pathForBlock(diagramId,blockName);
	}
	/** 
	 * @param nodeId UUID as a String of a node in the navigation tree
	 * @return a slash-separated path to the specified node. The path 
	 *         root is a slash representing the top node of the navigation tree.
	 */
	public String pathForNode(ProjectResourceId nodeId) {
		return requestHandler.pathForNode(nodeId);
	}
	/**
	 * Post a block result on its output. The transaction is from an external source.
	 * @param blockId
	 * @param port
	 * @param value
	 */
	public void postResult(ProjectResourceId diagramId,String blockId,String port,String value) {
		requestHandler.postValue(diagramId,blockId, port, value);
	}
	/**
	 * Execute the propagate method on a block
	 * @param diagramIdString
	 * @param blockIdString
	 */
	public void propagateBlockState(ProjectResourceId diagramIdString,String blockIdString) {
		requestHandler.propagateBlockState(diagramIdString,blockIdString);
	}
	/** 
	 *  @return
	 */
	public List<SerializableResourceDescriptor> queryControllerResources() {
		return  requestHandler.listResourceNodes();
	}
	/** 
	 * @param diagId the identifier of the diagram of interest
	 * @return a list of descriptors for blocks in the diagram
	 */
	public List<SerializableBlockStateDescriptor> queryDiagram(ProjectResourceId diagId) {
		return  requestHandler.listBlocksInDiagram(diagId);
	}
	/**
	 * Execute the getAux extension function in Gateway scope for the indicated resource.
	 * @param resid the resourceId of an application to be refreshed
	 * @param provider tag provider
	 * @param db datasource
	 * @return aux data from database
	 */
	public GeneralPurposeDataContainer readAuxData(ProjectResourceId resid,String nodeId,String provider,String db) {
		return requestHandler.readAuxData(resid, nodeId,provider, db);
	}
	/** 
	 * Change the name of a block
	 */
	public void renameBlock(ProjectResourceId diagramId,String blockIdString,String name) {
		requestHandler.renameBlock(diagramId, blockIdString, name);
	}
	/**
	 * Rename a SQLTag given its path and new name. The path must contain the
	 * provider name in brackets.
	 */
	public void renameTag(String projectName,String name,String path) {
		requestHandler.renameTag(projectName,name,path);
	}
	/**
	 * Reset a block in a diagram given string forms of their UUID
	 * @param diagramId id of the block's parent diagram
	 * @param blockName name of the block
	 */
	public void resetBlock(ProjectResourceId diagramId,String blockName) {
		requestHandler.resetBlock(diagramId,blockName);
	}
	/** 
	 *  Reset every block in a diagram specified by id.
	 * @param diagramId id of the diagram
	 */
	public void resetDiagram(ProjectResourceId diagramId) {
		requestHandler.resetDiagram(diagramId);
	}
	/**
	 * Execute stop() then start() on the specified block
	 */
	public void restartBlock(ProjectResourceId diagramId,String blockId) {
		requestHandler.restartBlock(diagramId,blockId);
	}
	public Boolean resourceExists(ProjectResourceId resourceId) {
		return requestHandler.resourceExists(resourceId);
	}
	/**
	 * 
	 * @param id identifier of the diagram for which the signal is local
	 * @param command
	 * @param message
	 * @param arg
	 * @return true if the signal was sent successfully
	 */
	public Boolean sendLocalSignal(ProjectResourceId id, String command,String message,String arg) {
		log.tracef("%s.sendLocalSignal: %s %s %s %s",CLSS,id,command,message,arg);
		return requestHandler.sendLocalSignal(id,command,message,arg);
	}
	public boolean sendSignal(ProjectResourceId diagramId,String blockName,String command,String message) {
		return requestHandler.sendSignal(diagramId,blockName,command,message);
	}
	/**
	 * 
	 * @param id identifier of the diagram for which the signal is local
	 * @param command
	 * @param message
	 * @param arg
	 * @param time the time to be assigned to the signal
	 * @return true if the signal was sent successfully
	 */
	public Boolean sendTimestampedSignal(ProjectResourceId id, String command,String message,String arg,Long time) {
		return requestHandler.sendTimestampedSignal(id,command,message,arg,time.longValue());
	}

	/** Set all changed properties for a block. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param json JSON representation of the complete property list for the block.
	 */
	public void setBlockProperties(ProjectResourceId diagramId,String blockId, String json) {
		log.debugf("%s.setBlockProperties: %s %s: %s", CLSS, diagramId, blockId, json);
		// Deserialize the JSON
		ObjectMapper mapper = new ObjectMapper();
		try {
			Collection<BlockProperty> properties = mapper.readValue(json, 
					new TypeReference<Collection<BlockProperty>>(){});
			requestHandler.setBlockProperties(diagramId,getBlockUUID(blockId),properties);
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s.setBlockProperties: parse exception (%s)",CLSS,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			log.warnf("%s.setBlockProperties: mapping exception (%s)",CLSS,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			log.warnf("%s.setBlockProperties: IO exception (%s)",CLSS,ioe.getLocalizedMessage());
		}; 
	}

	public void setBlockProperties(ProjectResourceId diagId, String blockId,Collection<BlockProperty> props) {
		UUID buuid = getBlockUUID(blockId);
		requestHandler.setBlockProperties(diagId,buuid,props);
	}

	/** Set a new value for the specified block property. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param json JSON representation of the property
	 */
	public void setBlockProperty(ProjectResourceId diagramId,String blockId, String json) {
		log.debugf("%s.setBlockProperty: %s %s: %s", CLSS, diagramId, blockId, json);
		// Deserialize the JSON
		ObjectMapper mapper = new ObjectMapper();
		try {
			BlockProperty property = mapper.readValue(json, BlockProperty.class);
			requestHandler.setBlockProperty(diagramId,getBlockUUID(blockId),property);
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s.setBlockProperty: parse exception (%s)",CLSS,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			log.warnf("%s.setBlockProperty: mapping exception (%s)",CLSS,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			log.warnf("%s.setBlockProperty: IO exception (%s)",CLSS,ioe.getLocalizedMessage());
		}; 
	}
	
	/** Change the value of a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param blockId Id of the block as a String
	 * @param pname the changed property
	 * @param bind the new binding value of the property. The binding is a tag path.
	 */
	public void setBlockPropertyBinding(ProjectResourceId diagramId,String blockId,String pname,String binding )  {
		requestHandler.setBlockPropertyBinding(diagramId,blockId,pname,binding);
	}

	/** Change the value of a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname name of the block
	 * @param pname the changed property
	 * @param value the new value of the property. The value will be coerced into the correct data type in the gateway 
	 */
	public void setBlockPropertyValue(ProjectResourceId diagramId,String bname,String pname,String value )  {
		requestHandler.setBlockPropertyValue(diagramId,bname,pname,value);
	}
	
	public void setBlockState(ProjectResourceId diagramId,String bname,String state ) {
		requestHandler.setBlockState(diagramId,bname,state);
	}
	
	public void setDiagramState(ProjectResourceId resourceId,String state) {
		requestHandler.setDiagramState(resourceId,state);
	}

	/**
	 * Tell the testing timer about the difference between test time
	 * and current time.
	 * @param offset the difference between test time and current time
	 *        ~ msecs. A positive number implies that the test time is
	 *        in the past.
	 */
	public void setTestTimeOffset(Long offset) {
		requestHandler.setTestTimeOffset(offset.longValue());
	}

	public void setTimeFactor(Double factor) {
		requestHandler.setTimeFactor(factor);
	}

	public void setProjectToolkitProperty(String projectName,String propertyName,String value) {
		requestHandler.setProjectToolkitProperty(projectName,propertyName,value);
	}
	public void setToolkitProperty(String propertyName,String value) {
		requestHandler.setToolkitProperty(propertyName,value);
	}
	
	/**
	 * Define a watermark for a diagram. 
	 */
	public void setWatermark(ProjectResourceId diagramId,String text) {
		requestHandler.setWatermark(diagramId,text);
	}

	public void startController() {
		log.infof("%s.startController ...",CLSS);
		requestHandler.startController();
	}

	public void stopController() {
		log.infof("%s.stopController ...",CLSS);
		requestHandler.stopController();
	}

	/**
	 * Trigger status notifications for all current diagrams and their blocks.
	 */
	public void triggerStatusNotifications(String projectName) {
		try {
			requestHandler.triggerStatusNotifications(projectName);
		}
		catch( Exception ex) {
			log.errorf(CLSS+".triggerStatusNotification: EXCEPTION", ex);
		}
	}

	/** Change the properties of anchors for a block. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param json JSON representation of the complete anchor list for the block.
	 */
	public void updateBlockAnchors(ProjectResourceId diagramId,String blockId, String json) {
		//log.infof("%s.updateBlockAnchors: %s %s: %s", TAG, diagramId, blockId, json);
		// Deserialize the JSON
		ObjectMapper mapper = new ObjectMapper();
		try {
			Collection<SerializableAnchor> anchors = mapper.readValue(json, 
					new TypeReference<Collection<SerializableAnchor>>(){});
			requestHandler.updateBlockAnchors(diagramId,blockId,anchors);
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s.updateBlockAnchors: parse exception (%s)",CLSS,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			log.warnf("%s.updateBlockAnchors: mapping exception (%s)",CLSS,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			log.warnf("%s.updateBlockAnchors: IO exception (%s)",CLSS,ioe.getLocalizedMessage());
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.sendLocalSignal: Diagram or block UUID string is illegal (%s,%s), creating new",CLSS,diagramId,blockId);
		}
	}

	/** Convert a string to a UUID. */
	private UUID getBlockUUID(String blockId) {
		UUID blockUUID;
		try {
			blockUUID = UUID.fromString(blockId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s: getBlockProperties: Block UUID string is illegal (%s), creating new",CLSS,blockId);
			blockUUID = UUID.nameUUIDFromBytes(blockId.getBytes());
		}
		return blockUUID;
	}
	/**
	 * Execute the setAux extension function in Gateway scope for the indicated resource.
	 * @param resid the resourceId of an application to be refreshed
	 * @param provider tag provider
	 * @param db datasource
	 */
	public void writeAuxData(ProjectResourceId resid,String nodeId,GeneralPurposeDataContainer container,String provider,String db) {
		requestHandler.writeAuxData(resid, nodeId,container,provider, db);
	}
}
