/**
 *   (c) 2014-2016  ILS Automation. All rights reserved.
 */
package com.ils.blt.gateway;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
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
	private static String TAG = "GatewayRpcDispatcher";
	private final LoggerEx log;
	private final ControllerRequestHandler requestHandler;

	/**
	 * Constructor. There is a separate dispatcher for each project.
	 * @param ctx Gateway context
	 */
	public GatewayRpcDispatcher(GatewayContext ctx) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.requestHandler = ControllerRequestHandler.getInstance();
	}
	
	public List<SerializableResourceDescriptor> childNodes(String nodeId) {
		return requestHandler.childNodes(nodeId);
	}
	
	/**
	 * Clear any watermark on a diagram. 
	 */
	public void clearWatermark(String diagramId) {
		requestHandler.clearWatermark(diagramId);
	}
	
	
	public void clearController() {
		requestHandler.clearController();
	}
	public void createTag(DataType type,String path) {
		requestHandler.createTag(type, path);
	}
	public void deleteTag(String path) {
		requestHandler.deleteTag(path);
	}
	/**
	 * This should always succeed because we create a block in the gateway whenever we 
	 * create one from the palette.
	 * @param uuidString diagram id as a string
	 * @return True if we've discovered the specified block.
	 */
	public Boolean diagramExists(String uuidString) {
		return new Boolean(requestHandler.diagramExists(uuidString));
	}

	public String getApplicationName(String uuid) {
		return requestHandler.getApplicationName(uuid);
	}
	
	/**
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockName name of the block within the diagram
	 * @return the id of the specified block.
	 */
	public String getBlockId(String diagramId, String blockName) {
		return requestHandler.getBlockId(diagramId,blockName);
	}
	/**
	 * Query the specified block for its properties. If the block does not exist, create it, given the
	 * specified class name. In the case of a new block, its diagram may also need to be created. 
	 * 
	 * @param projectId project identifier
	 * @param resourceId resource identifier
	 * @param blockId block UUID as a string
	 * @param className of the block
	 * @return properties for the block
	 */
	public List<String> getBlockProperties(String className,Long projectId,Long resourceId,String blockId) {
		log.debugf("%s.getBlockProperties: %s %d:%d %s",TAG,className,projectId.longValue(),resourceId.longValue(),blockId);
		UUID blockUUID = null;
		try {
			blockUUID = UUID.fromString(blockId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getBlockProperties: Block UUID string is illegal (%s), creating new",TAG,blockId);
			blockUUID = UUID.nameUUIDFromBytes(blockId.getBytes());
		}
		List<BlockProperty> propertyList = requestHandler.
					getBlockProperties(className,projectId.longValue(),resourceId.longValue(),blockUUID);
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
			log.warnf("%s: getBlockProperties: %s block %d:%d has no properties",TAG,className,projectId.longValue(),resourceId.longValue());
		}
		if( result!=null) log.debugf("%s.getBlockProperties: %s = %s",TAG,className,result.toString());
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
	
	public String getBlockState(String diagramId, String blockName) {
		return requestHandler.getBlockState(diagramId, blockName);
	}
	/**
	 * Deserialize the incoming defaults, add/update from model, re-serialize.
	 * @param proj project identifier
	 * @param res resource identifier
	 * @param connectionId id of the connection
	 * @param json connection attributes as JSON
	 * @return the JSON string
	 */
	public String getConnectionAttributes(Long proj, Long res,String connectionId,String json) {
		long projectId = proj.longValue();
		long resourceId = res.longValue();
		log.debugf("%s.getConnectionAttributes: %d:%d:%s =\n%s",TAG,projectId,resourceId,connectionId,json);
		
		ObjectMapper mapper = new ObjectMapper();
		Hashtable<String, Hashtable<String, String>> propertiesTable;
		try {
			propertiesTable = mapper.readValue(json, new TypeReference<Hashtable<String,Hashtable<String,String>>>(){});
			Hashtable<String,Hashtable<String,String>> results = requestHandler.getConnectionAttributes(projectId,resourceId,connectionId,propertiesTable);
			log.debugf("%s: created table = %s",TAG,results);
			json =  mapper.writeValueAsString(results);
			log.debugf("%s: JSON=%s",TAG,json);
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s: getConnectionAttributes: parsing exception (%s)",TAG,jpe.getLocalizedMessage());
		} 
		catch (JsonMappingException jme) {
			log.warnf("%s: getConnectionAttributes: mapping exception(%s)",TAG,jme.getLocalizedMessage());
		} 
		catch (IOException ioe) {
			log.warnf("%s: getConnectionAttributes: io exception(%s)",TAG,ioe.getLocalizedMessage());
		}
		return json;
	}
	
	
	public String getControllerState() {
		return requestHandler.getExecutionState();
	}

	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * database.  
	 * @param uuid id of the subject node as a String
	 * @return database name
	 */
	public String getDatabaseForUUID(String uuid) {
		return requestHandler.getDatabaseForUUID(uuid);
	}
	

	public List<String> getDatasourceNames() {
		return requestHandler.getDatasourceNames();
	}
	
	/**
	 * @param diagramId String representation of the diagram's internal Id.
	 * @return a descriptor for the diagram that corresponds to that Id.
	 */
	public SerializableResourceDescriptor getDiagram(String diagramId) {
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
				log.warnf("%s: getDiagramDescriptors: parsing exception (%s)",TAG,jpe.getLocalizedMessage());
			} 
			catch (JsonMappingException jme) {
				log.warnf("%s: getDiagramDescriptors: mapping exception(%s)",TAG,jme.getLocalizedMessage());
			} 
			catch (IOException ioe) {
				log.warnf("%s: getDiagramDescriptors: io exception(%s)",TAG,ioe.getLocalizedMessage());
			}

		}
		return results;
	}
	public SerializableResourceDescriptor getDiagramForBlock(String blockId) {
		return requestHandler.getDiagramForBlock(blockId);
	}

	public String getDiagramState(Long projectId,Long resourceId) {
		return requestHandler.getDiagramState(projectId,resourceId).name();
	}

	public String getDiagramState(String diagramId) {
		return requestHandler.getDiagramState(diagramId).name();
	}
	/**
	 * @return an explanation for the state of a block.
	 */
	public String getExplanation(String diagramId,String blockId) {
		return requestHandler.getExplanation(diagramId,blockId);
	}
	public String getFamilyName(String uuid) {
		return requestHandler.getFamilyName(uuid);
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
			log.warnf("%s: getHostname: unknown host exception (%s)",TAG,ex.getLocalizedMessage());
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
	public String getInternalState(String diagramId,String blockId) {
		SerializableBlockStateDescriptor desc = requestHandler.getInternalState(diagramId,blockId);
		ObjectMapper mapper = new ObjectMapper();
		String json = "";
		try {
			json = mapper.writeValueAsString(desc);
		}
		catch (JsonProcessingException jpe) {
			log.warnf("%s.getInternalState: Exception (%s)",TAG,jpe.getLocalizedMessage());
		}
		return json;
	}
	public Object getPropertyBinding(String diagramId,String blockId,String propertyName) {
		return requestHandler.getPropertyBinding(diagramId, blockId, propertyName);
	}
	
	public Object getPropertyValue(String diagramId,String blockId,String propertyName) {
		return requestHandler.getPropertyValue(diagramId, blockId, propertyName);
	}
	public Date getTimeOfLastBlockStateChange(String diagramId, String blockName) {
		return requestHandler.getTimeOfLastBlockStateChange(diagramId,blockName);
	}
	public String getToolkitProperty(String propertyName) {
		return requestHandler.getToolkitProperty(propertyName);
	}

    public Boolean isAlerting(Long projectId,Long resourceId) {
    	boolean result = requestHandler.isAlerting(projectId, resourceId);
    	return new Boolean(result);
    }

	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(String diagramId,String blockId,String portName) {
		return requestHandler.listBlocksConnectedAtPort(diagramId,blockId,portName);
	}
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(String diagramId, String blockName) {
		return requestHandler.listBlocksDownstreamOf(diagramId, blockName);
	}
	
	public List<SerializableBlockStateDescriptor> listBlocksForTag(String tagpath) {
		return requestHandler.listBlocksForTag(tagpath);
	}
	public List<SerializableBlockStateDescriptor> listBlocksGloballyDownstreamOf(String diagramId, String blockName) {
		return requestHandler.listBlocksGloballyDownstreamOf(diagramId, blockName);
	}
	public List<SerializableBlockStateDescriptor> listBlocksGloballyUpstreamOf(String diagramId, String blockName) {
		return requestHandler.listBlocksGloballyUpstreamOf(diagramId, blockName);
	}
	public List<SerializableBlockStateDescriptor> listBlocksInDiagram(String diagramId) {
		return requestHandler.listBlocksInDiagram(diagramId);
	}
	public List<SerializableBlockStateDescriptor> listBlocksOfClass(String className) {
		return requestHandler.listBlocksOfClass(className);
	}
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(String diagramId, String blockName) {
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
	public List<SerializableBlockStateDescriptor> listDiagramBlocksOfClass(String diagramId, String className) {
		return requestHandler.listDiagramBlocksOfClass(diagramId, className);
	}
	public List<SerializableResourceDescriptor> listDiagramDescriptors(String projectName) {
		return requestHandler.listDiagramDescriptors(projectName);
	}

	public List<SerializableResourceDescriptor> listResourceNodes() {
		return requestHandler.listResourceNodes();
	}

	public List<SerializableBlockStateDescriptor> listSinksForSource(String diagramId,String blockId) {
		return requestHandler.listSinksForSource(diagramId,blockId);
	}

	public List<SerializableBlockStateDescriptor> listSourcesForSink(String diagramId,String blockId) {
		return requestHandler.listSourcesForSink(diagramId,blockId);
	}
	
	public String pathForBlock(String diagramId,String blockName) {
		return requestHandler.pathForBlock(diagramId,blockName);
	}
	/** 
	 * @param nodeId UUID as a String of a node in the navigation tree
	 * @return a slash-separated path to the specified node. The path 
	 *         root is a slash representing the top node of the navigation tree.
	 */
	public String pathForNode(String nodeId) {
		return requestHandler.pathForNode(nodeId);
	}
	/**
	 * Post a block result on its output. The transaction is from an external source.
	 * @param blockId
	 * @param port
	 * @param value
	 */
	public void postResult(String diagramId,String blockId,String port,String value) {
		requestHandler.postValue(diagramId,blockId, port, value);
	}
	/**
	 * Execute the propagate method on a block
	 * @param diagramIdString
	 * @param blockIdString
	 */
	public void propagateBlockState(String diagramIdString,String blockIdString) {
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
	public List<SerializableBlockStateDescriptor> queryDiagram(String diagId) {
		return  requestHandler.listBlocksInDiagram(diagId);
	}
	/** Change the name of a block
	 * 
	 */
	public void renameBlock(String diagramIdString,String blockIdString,String name) {
		requestHandler.renameBlock(diagramIdString, blockIdString, name);
	}
	/**
	 * Rename a SQLTag given its path and new name. The path must contain the
	 * provider name in brackets.
	 */
	public void renameTag(String name,String path) {
		requestHandler.renameTag(name,path);
	}
	/**
	 * Reset a block in a diagram given string forms of their UUID
	 * @param diagramIdString id of the block's parent diagram
	 * @param blockName name of the block
	 */
	public void resetBlock(String diagramIdString,String blockName) {
		requestHandler.resetBlock(diagramIdString,blockName);
	}
	/** 
	 *  Reset every block in a diagram specified by id.
	 * @param uuidString id of the diagram as a string
	 */
	public void resetDiagram(String uuidString) {
		requestHandler.resetDiagram(uuidString);
	}
	/**
	 * Execute stop() then start() on the specified block
	 */
	public void restartBlock(String diagramId,String blockId) {
		requestHandler.restartBlock(diagramId,blockId);
	}
	public Boolean resourceExists(Long projectId,Long resourceId) {
		return new Boolean(requestHandler.resourceExists(projectId.longValue(), resourceId.longValue()));
	}
	/**
	 * 
	 * @param uuidString identifier of the diagram for which the signal is local
	 * @param command
	 * @param message
	 * @param arg
	 * @return true if the signal was sent successfully
	 */
	public Boolean sendLocalSignal(String uuidString, String command,String message,String arg) {
		log.tracef("%s.sendLocalSignal: %s %s %s %s",TAG,uuidString,command,message,arg);
		return new Boolean(requestHandler.sendLocalSignal(uuidString,command,message,arg));
	}
	public boolean sendSignal(String diagramId,String blockName,String command,String message) {
		return new Boolean(requestHandler.sendSignal(diagramId,blockName,command,message));
	}
	/**
	 * 
	 * @param uuidString identifier of the diagram for which the signal is local
	 * @param command
	 * @param message
	 * @param arg
	 * @param time the time to be assigned to the signal
	 * @return true if the signal was sent successfully
	 */
	public Boolean sendTimestampedSignal(String uuidString, String command,String message,String arg,Long time) {
		return new Boolean(requestHandler.sendTimestampedSignal(uuidString,command,message,arg,time.longValue()));
	}


	/**
	 * Set the state of every diagram in an application to the specified value.
	 * @param appname
	 * @param state new state as a String
	 */
	public void setApplicationState(String appname, String state) {
		requestHandler.setApplicationState(appname,state);
	}

	/** Set all changed properties for a block. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param json JSON representation of the complete property list for the block.
	 */
	public void setBlockProperties(String diagramId,String blockId, String json) {
		log.debugf("%s.setBlockProperties: %s %s: %s", TAG, diagramId, blockId, json);
		// Deserialize the JSON
		ObjectMapper mapper = new ObjectMapper();
		try {
			Collection<BlockProperty> properties = mapper.readValue(json, 
					new TypeReference<Collection<BlockProperty>>(){});
			requestHandler.setBlockProperties(getBlockUUID(diagramId),getBlockUUID(blockId),properties);
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s.setBlockProperties: parse exception (%s)",TAG,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			log.warnf("%s.setBlockProperties: mapping exception (%s)",TAG,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			log.warnf("%s.setBlockProperties: IO exception (%s)",TAG,ioe.getLocalizedMessage());
		}; 
	}

	public void setBlockProperties(String diagId, String blockId,Collection<BlockProperty> props) {
		UUID duuid = getBlockUUID(diagId);
		UUID buuid = getBlockUUID(blockId);
		requestHandler.setBlockProperties(duuid,buuid,props);
	}

	/** Set a new value for the specified block property. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param json JSON representation of the property
	 */
	public void setBlockProperty(String diagramId,String blockId, String json) {
		log.debugf("%s.setBlockProperty: %s %s: %s", TAG, diagramId, blockId, json);
		// Deserialize the JSON
		ObjectMapper mapper = new ObjectMapper();
		try {
			BlockProperty property = mapper.readValue(json, BlockProperty.class);
			requestHandler.setBlockProperty(getBlockUUID(diagramId),getBlockUUID(blockId),property);
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s.setBlockProperty: parse exception (%s)",TAG,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			log.warnf("%s.setBlockProperty: mapping exception (%s)",TAG,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			log.warnf("%s.setBlockProperty: IO exception (%s)",TAG,ioe.getLocalizedMessage());
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
	public void setBlockPropertyBinding(String diagramId,String blockId,String pname,String binding )  {
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
	public void setBlockPropertyValue(String diagramId,String bname,String pname,String value )  {
		requestHandler.setBlockPropertyValue(diagramId,bname,pname,value);
	}
	
	public void setBlockState(String diagramId,String bname,String state ) {
		requestHandler.setBlockState(diagramId,bname,state);
	}
	
	public void setDiagramState(Long projectId,Long resourceId,String state) {
		requestHandler.setDiagramState(projectId,resourceId,state);
	}

	public void setDiagramState(String diagramId,String state) {
		requestHandler.setDiagramState(diagramId,state);
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

	public void setToolkitProperty(String propertyName,String value) {
		requestHandler.setToolkitProperty(propertyName,value);
	}
	
	/**
	 * Define a watermark for a diagram. 
	 */
	public void setWatermark(String diagramId,String text) {
		requestHandler.setWatermark(diagramId,text);
	}

	public void startController() {
		log.infof("%s.startController ...",TAG);
		requestHandler.startController();
	}

	public void stopController() {
		log.infof("%s.stopController ...",TAG);
		requestHandler.stopController();
	}

	/**
	 * Trigger status notifications for all current diagrams and their blocks.
	 */
	public void triggerStatusNotifications() {
		try {
			requestHandler.triggerStatusNotifications();
		}
		catch( Exception ex) {
			log.errorf(TAG+".triggerStatusNotification: EXCEPTION", ex);
		}
	}

	/** Change the properties of anchors for a block. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param json JSON representation of the complete anchor list for the block.
	 */
	public void updateBlockAnchors(String diagramId,String blockId, String json) {
		//log.infof("%s.updateBlockAnchors: %s %s: %s", TAG, diagramId, blockId, json);
		// Deserialize the JSON
		ObjectMapper mapper = new ObjectMapper();
		try {
			Collection<SerializableAnchor> anchors = mapper.readValue(json, 
					new TypeReference<Collection<SerializableAnchor>>(){});
			UUID diagramUUID = UUID.fromString(diagramId);
			UUID blockUUID = UUID.fromString(blockId);
			requestHandler.updateBlockAnchors(diagramUUID,blockUUID,anchors);
		} 
		catch (JsonParseException jpe) {
			log.warnf("%s.updateBlockAnchors: parse exception (%s)",TAG,jpe.getLocalizedMessage());
		}
		catch(JsonMappingException jme) {
			log.warnf("%s.updateBlockAnchors: mapping exception (%s)",TAG,jme.getLocalizedMessage());
		}
		catch(IOException ioe) {
			log.warnf("%s.updateBlockAnchors: IO exception (%s)",TAG,ioe.getLocalizedMessage());
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.sendLocalSignal: Diagram or block UUID string is illegal (%s,%s), creating new",TAG,diagramId,blockId);
		}
	}

	/** Convert a string to a UUID. */
	private UUID getBlockUUID(String blockId) {
		UUID blockUUID;
		try {
			blockUUID = UUID.fromString(blockId);
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s: getBlockProperties: Block UUID string is illegal (%s), creating new",TAG,blockId);
			blockUUID = UUID.nameUUIDFromBytes(blockId.getBytes());
		}
		return blockUUID;
	}
}
