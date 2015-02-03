/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.gateway;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.common.ClassList;
import com.inductiveautomation.ignition.common.script.ScriptManager;
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
 */
public class GatewayRpcDispatcher   {
	private static String TAG = "GatewayRpcDispatcher";
	private final LoggerEx log;
	private final GatewayContext context;
	private final ControllerRequestHandler requestHandler;

	/**
	 * Constructor. There is a separate dispatcher for each project.
	 */
	public GatewayRpcDispatcher(GatewayContext ctx) {
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.requestHandler = ControllerRequestHandler.getInstance();
	}
	public void clearController() {
		requestHandler.clearController();
	}
	
	/**
	 * This should always succeed because we create a block in the gateway whenever we 
	 * create one from the palette.
	 * @param uuidString
	 * @return True if we've discovered the specified block.
	 */
	public Boolean diagramExists(String uuidString) {
		log.infof("%s.diagramExists ...",TAG);
		return new Boolean(requestHandler.diagramExists(uuidString));
	}

	public String getApplicationName(String uuid) {
		return requestHandler.getApplicationName(uuid);
	}


	/**
	 * Query the specified block for its properties. If the block does not exist, create it, given the
	 * specified class name. In the case of a new block, its diagram may also need to be created. 
	 * 
	 * @param projectId
	 * @param resourceId
	 * @param blockId
	 * @param className
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
		BlockProperty[] propertyArray = requestHandler.
					getBlockProperties(className,projectId.longValue(),resourceId.longValue(),blockUUID);
		List<String> result = null;
		if( propertyArray!=null ) {
			result = new ArrayList<String>();
			for( BlockProperty prop:propertyArray ) {
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
	 *  @return
	 */
	public List<String> getBlockPrototypes() {
		log.infof("%s.getBlockPrototypes ...",TAG);
		List<String> results = new ArrayList<String>();
		List<PalettePrototype> prototypes = requestHandler.getBlockPrototypes();
		for(PalettePrototype pp:prototypes) {
			results.add(pp.toJson());
		}
		log.infof("%s.getBlockPrototypes: returning %d palette prototypes",TAG,results.size());
		return results;
	}
	/**
	 * Deserialize the incoming defaults, add/update from model, re-serialize.
	 * @param proj
	 * @param res
	 * @param connectionId
	 * @param json
	 * @return
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
	public List getDiagramBlocksOfClass(String diagramId,String className) {
		return requestHandler.getDiagramBlocksOfClass(diagramId,className);
	}
	public List<String> getDiagramDescriptors(String projectName) {
		log.infof("%s.getDiagramDescriptors ...",TAG);
		List<String> results = new ArrayList<String>();
		List<SerializableResourceDescriptor> descriptors = requestHandler.getDiagramDescriptors(projectName);
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
	
	public String getDiagramState(Long projectId,Long resourceId) {
		return requestHandler.getDiagramState(projectId,resourceId).name();
	}
	
	public String getFamilyName(String uuid) {
		return requestHandler.getFamilyName(uuid);
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
		log.infof("%s.getInternalState: (%s:%s)",TAG,diagramId,blockId);
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
	

	public Object getPropertyValue(String diagramId,String blockId,String propertyName) {
		return requestHandler.getPropertyValue(diagramId, blockId, propertyName);
	}
	
	public String getToolkitProperty(String propertyName) {
		return requestHandler.getToolkitProperty(propertyName);
	}
	
	/** 
	 *  @return
	 */
	public List<SerializableResourceDescriptor> queryControllerResources() {
		log.infof("%s.queryControllerResources ...",TAG);
		return  requestHandler.queryControllerResources();
	}
	/** 
	 * @param diagId the identifier of the diagram of interest
	 *  @return
	 */
	public List<SerializableResourceDescriptor> queryDiagram(String diagId) {
		log.infof("%s.queryDiagram ... %s",TAG,diagId);
		return  requestHandler.queryDiagramForBlocks(diagId);
	}
	/**
	 * Reset a block or diagram given its UUID
	 * @param uuidString
	 */
	public void resetBlock(String diagramIdString,String blockIdString) {
		log.infof("%s.resetBlock ...",TAG);
		requestHandler.resetBlock(diagramIdString,blockIdString);
	}
	/** 
	 *  Reset every block in a diagram specified by id.
	 */
	public void resetDiagram(String uuidString) {
		log.infof("%s: resetDiagram ...",TAG);
		requestHandler.resetDiagram(uuidString);
	}

	public Boolean resourceExists(Long projectId,Long resourceId) {
		return new Boolean(requestHandler.resourceExists(projectId.longValue(), resourceId.longValue()));
	}
	/**
	 * 
	 * @param uuidString identifier of the diagram for which the signal is local
	 * @param className
	 * @param command
	 * @return
	 */
	public Boolean sendLocalSignal(String uuidString, String className, String command) {
		log.infof("%s.sendLocalSignal: %s %s %s",TAG,uuidString,className,command);
		return new Boolean(requestHandler.sendLocalSignal(uuidString, className, command));
	}
	
	/** Set all changed properties for a block. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param json JSON representation of the complete property list for the block.
	 */
	public void setBlockProperties(String diagramId,String blockId, String json) {
		log.infof("%s.setBlockProperties: %s %s: %s", TAG, diagramId, blockId, json);
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
	
	/** Set a new value for the specified block property. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param json JSON representation of the property
	 */
	public void setBlockProperty(String diagramId,String blockId, String json) {
		log.infof("%s.setBlockProperty: %s %s: %s", TAG, diagramId, blockId, json);
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
	public void setDiagramState(Long projectId,Long resourceId,String state) {
		requestHandler.setDiagramState(projectId,resourceId,state);
	}
	
	public void setToolkitProperty(String propertyName,String value) {
		requestHandler.setToolkitProperty(propertyName,value);
	}
	public void startController() {
		requestHandler.startController();
	}
	
	public void stopController() {
		requestHandler.stopController();
	}
	
	/**
	 * Trigger status notifications for all current diagrams and their blocks.
	 */
	public void triggerStatusNotifications() {
		log.infof("%s.triggerStatusNotifications ...",TAG);
		requestHandler.triggerStatusNotifications();
	}
	
	/** Change the properties of anchors for a block. 
	 * @param diagramId the uniqueId of the parent diagram
	 * @param blockId the uniqueId of the block
	 * @param json JSON representation of the complete anchor list for the block.
	 */
	public void updateBlockAnchors(String diagramId,String blockId, String json) {
		log.infof("%s.updateBlockAnchors: %s %s: %s", TAG, diagramId, blockId, json);
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
