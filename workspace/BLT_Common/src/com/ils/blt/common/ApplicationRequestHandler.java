/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.common.persistence.ToolkitProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;



/**
 *  This class is a common point for managing requests to the gateway dealing with the
 *  execution engine and block status. It is designed for use by Java code in the designer 
 *  as well as Python scripting. It provides a way to request/set properties of 
 *  diagrams, blocks and connections.
 *  
 *  Each request is relayed to the Gateway scope via an RPC call.
 */
public class ApplicationRequestHandler implements ToolkitRequestHandler {
	private final static String TAG = "ApplicationRequestHandler";
	private final LoggerEx log;

	/**
	 * Constructor adds common attributes that are needed to generate unique keys to identify
	 * blocks and connectors.
	 */
	public ApplicationRequestHandler()  {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	@Override
	public List<SerializableResourceDescriptor> childNodes(String nodeId) {
		return null;
	}

	/**
	 * Remove all current diagrams from the controller.
	 */
	@Override
	public void clearController() {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "clearController");
			log.debugf("%s.clearController ...",TAG);
		}
		catch(Exception ge) {
			log.infof("%s.clearController: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Clear any watermark on a diagram. 
	 */
	public void clearWatermark(String diagramId) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
									BLTProperties.MODULE_ID, "clearWatermark",diagramId);
		}
		catch(Exception ge) {
			log.infof("%s.clearWatermark: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Determine whether or not the indicated diagram is known to the controller.
	 */
	@Override
	public boolean diagramExists(String uuidString) {
		boolean result = false;
		try {
			Boolean value = (Boolean)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "diagramExists",uuidString);
			log.debugf("%s.diagramExists  ...%s = %s",TAG,uuidString,result);
			if( value!=null ) result = value.booleanValue();
		}
		catch(Exception ge) {
			log.infof("%s.diagramExists: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * Execute evaluate() on a specified block
	 */
	@Override
	public void evaluateBlock(String diagramId,String blockId) {
		log.debugf("%s.evaluateBlock ...",TAG);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "evaluateBlock",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.evaluateBlock: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	@Override
	public String getApplicationName(String uuid) {
		String name = "NULL UUID";
		if( uuid!=null) {
			log.debugf("%s.getApplicationName... %s",TAG,uuid);
			try {
				name = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
						BLTProperties.MODULE_ID, "getApplicationName",uuid);
			}
			catch(Exception ex) {
				log.infof("%s.getApplicationName: Exception (%s)",TAG,ex.getMessage());
			};
		}
		return name;
	}

	/**
	 * @return the id of the name block.
	 */
	@Override
	public String getBlockId(String diagramId, String blockName) {
		String id = "UNKNOWN";
		try {
			id = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockId",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.getBlockId: GatewayException (%s)",TAG,ge.getMessage());
		}
		return id;
	}
	/**
	 * Obtain a list of BlockProperty objects for the specified block. If the block is not known to the gateway
	 * it will be created.
	 * 
	 * @param projectId
	 * @param resourceId
	 * @param blockId
	 * @param className
	 * @return an array of block properties for the subject block
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<BlockProperty> getBlockProperties(String className,long projectId,long resourceId,UUID blockId) {
		log.debugf("%s.getBlockProperties: for block %s (%s)",TAG,blockId.toString(),className);
		List<BlockProperty> result = null;
		List<String> jsonList = new ArrayList<String>();
		try {
			jsonList = (List<String>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockProperties",className,new Long(projectId),new Long(resourceId),blockId.toString());
		}
		catch(Exception ge) {
			log.infof("%s.getBlockProperties: GatewayException (%s)",TAG,ge.getMessage());
		}
		result = new ArrayList<>();	
		if( jsonList!=null) {
			for( String json:jsonList ) {
				log.tracef("%s: property: %s",TAG,json);
				BlockProperty bp = BlockProperty.createProperty(json);
				log.debugf("%s.getBlockProperties: %s",TAG, bp.toString());
				result.add(bp);
			}
		}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<PalettePrototype> getBlockPrototypes() {
		log.tracef("%s.getBlockPrototypes ...",TAG);
		List<PalettePrototype> result = new ArrayList<PalettePrototype>();
		List<String> jsonList = new ArrayList<String>();
		try {
			jsonList = (List<String> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockPrototypes");
		}
		catch(Exception ge) {
			log.infof("%s.getBlockPrototypes: GatewayException (%s)",TAG,ge.getMessage());
		}
		
		if( jsonList!=null) {
			for( String json:jsonList ) {
				log.tracef("%s.getBlockPrototypes: %s",TAG,json);
				PalettePrototype bp = PalettePrototype.createPrototype(json);
				result.add(bp);
			}
		}
		return result;
	}


	/**
	 * @return the current state of the specified block.
	 */
	@Override
	public String getBlockState(String diagramId, String blockName) {
		String state = "UNKNOWN";
		try {
			state = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockState",diagramId,blockName);
			log.debugf("%s.getBlockState %s = %s",TAG,blockName,state);
		}
		catch(Exception ge) {
			log.infof("%s.getBlockState: GatewayException (%s)",TAG,ge.getMessage());
		}
		return state;
	}
	
	
	/**
	 * Determine whether or not the engine is running.
	 */
	@Override
	public String getControllerState() {
		String state = "";
		try {
			// Returns either "running" or "stopped"
			state = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getControllerState");
			log.debugf("%s.getControllerState ... %s",TAG,state);
		}
		catch(Exception ge) {
			log.infof("%s.getControllerState: GatewayException (%s)",TAG,ge.getMessage());
		}
		return state;
	}
	
	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * database.  
	 * @param uuid
	 * @return database name
	 */
	@Override
	public String getDatabaseForUUID(String uuid) {
		String db = "NONE";
		if( uuid!=null) {
			log.infof("%s.getDatabaseForUUID... %s",TAG,uuid);
			try {
				db = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
						BLTProperties.MODULE_ID, "getDatabaseForUUID",uuid);
			}
			catch(Exception ex) {
				log.infof("%s.getDatabaseForUUID: Exception (%s)",TAG,ex.getMessage());
			};
		}
		return db;
	}
	/**
	 * @param diagramId String representation of the diagram's internal Id.
	 * @return a descriptor for the diagram that corresponds to that Id.
	 */
	@Override
	public SerializableResourceDescriptor getDiagram(String diagramId)  {
		SerializableResourceDescriptor result = null;
		try {
			result = (SerializableResourceDescriptor)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getDiagram",diagramId);
		}
		catch(Exception ge) {
			log.infof("%s.getDiagram: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<String> getDatasourceNames() {
		List<String> names = new ArrayList<>();
		try {
			names = (List<String>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getDatasourceNames");
		}
		catch(Exception ge) {
			log.infof("%s.getControllerState: GatewayException (%s)",TAG,ge.getMessage());
		}
		return names;
	}
	@Override
	public SerializableResourceDescriptor getDiagramForBlock(String blockId) {
		SerializableResourceDescriptor result = null;
		try {
			result = (SerializableResourceDescriptor)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getDiagramForBlock",blockId);
		}
		catch(Exception ge) {
			log.infof("%s.getDiagramForBlock: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * @return the current state of the specified diagram.
	 */
	@Override
	public DiagramState getDiagramState(Long projectId, Long resourceId) {
		DiagramState result = DiagramState.ACTIVE;
		try {
			String state = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getDiagramState",projectId,resourceId);
			log.debugf("%s.getDiagramState ... %s",TAG,result.toString());
			result = DiagramState.valueOf(state);
		}
		catch(Exception ge) {
			log.infof("%s.getDiagramState: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * @return the current state of the specified diagram.
	 */
	@Override
	public DiagramState getDiagramState(String diagramId) {
		DiagramState result = DiagramState.ACTIVE;
		try {
			String state = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getDiagramState",diagramId);
			log.debugf("%s.getDiagramState ... %s",TAG,result.toString());
			result = DiagramState.valueOf(state);
		}
		catch(Exception ge) {
			log.infof("%s.getDiagramState: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	@Override
	public String getFamilyName(String uuid) {
		String name = "NULL UUID";
		if( uuid!=null ) {
			log.infof("%s.getFamilyName... %s",TAG,uuid);
			try {
				name = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
						BLTProperties.MODULE_ID, "getFamilyName",uuid);
			}
			catch(Exception ex) {
				log.infof("%s.getFamilyName: Exception (%s)",TAG,ex.getMessage());
			};
		}
		return name;
	}
	/**
	 * @return an explanation for the state of a block.
	 */
	@Override
	public String getExplanation(String diagramId,String blockId) {
		String reason="";

		try {
			reason = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getExplanation",diagramId,blockId);
		}
		catch(Exception ex) {
			log.infof("%s.getExplanation: Exception (%s)",TAG,ex.getMessage());
		};
		return reason;
	}
	/**
	 * @return internal details of a block for debugging purposes.
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalState(String diagramId,String blockId) {
		//log.infof("%s.getInternalState ... %s,%s",TAG,diagramId,blockId);
		SerializableBlockStateDescriptor result = new SerializableBlockStateDescriptor();
		String json = null;
		try {
			// Returns either "running" or "stopped"
			json = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getInternalState",diagramId,blockId);
			log.debugf("%s.getInternalState ... %s",TAG,json);
		}
		catch(Exception ge) {
			log.infof("%s.getInternalState: GatewayException (%s)",TAG,ge.getMessage());
		}
		if( json!=null) {
			ObjectMapper mapper = new ObjectMapper();

			try {
				result = mapper.readValue(json, SerializableBlockStateDescriptor.class);
			} 
			catch (JsonParseException jpe) {
				log.warnf("%s: getInternalState parse exception (%s)",TAG,jpe.getLocalizedMessage());
			}
			catch(JsonMappingException jme) {
				log.warnf("%s: getInternalState mapping exception (%s)",TAG,jme.getLocalizedMessage());
			}
			catch(IOException ioe) {
				log.warnf("%s: getInternalState IO exception (%s)",TAG,ioe.getLocalizedMessage());
			}
		}
		return result;
	}
	/**
	 * @return the name of the Isolation-mode datasource
	 */
	public String getIsolationDatabase() {
		return getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE);
	}
	/**
	 * @return the name of the Production-mode datasource
	 */
	public String getProductionDatabase() {
		return getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_DATABASE);
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the binding (tag path) of a specified block property. If there is no binding,
	 *         return an empty string.
	 */
	public Object getPropertyBinding(String diagramId,String blockId,String propertyName) {
		Object result = null;
		try {
			result = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getPropertyBinding",diagramId,blockId,propertyName);
			log.debugf("%s.getPropertyBinding ... %s",TAG,result.toString());
		}
		catch(Exception ge) {
			log.infof("%s.getPropertyBinding: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of a specified block property.
	 */
	public Object getPropertyValue(String diagramId,String blockId,String propertyName) {
		Object result = null;
		try {
			result = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getPropertyValue",diagramId,blockId,propertyName);
			log.debugf("%s.getPropertyValue ... %s",TAG,result.toString());
		}
		catch(Exception ge) {
			log.infof("%s.getPropertyValue: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	public Date getTimeOfLastBlockStateChange(String diagramId, String blockName) {
		Date result = null;
		try {
			result = (Date)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getTimeOfLastBlockStateChange",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.getTimeOfLastBlockStateChange: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * Acquire a value from the SQLite database table associated with the toolkit. A
	 * empty string is returned if the string is not found, null if an exception is thrown.
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of the specified property.
	 */
	public String getToolkitProperty(String propertyName) {
		String result = null;
		//log.infof("%s.getToolkitProperty ... %s",TAG,propertyName);
		try {
			result = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getToolkitProperty",propertyName);
			log.tracef("%s.getToolkitProperty ... %s = %s",TAG,propertyName,result.toString());
		}
		catch(Exception ge) {
			log.infof("%s.getToolkitProperty: GatewayException (%s:%s)",TAG,ge.getClass().getName(),ge.getMessage());
		}
		return result;
	}
	/**
	 * Determine whether or not the engine is running.
	 */
	@Override
	public boolean isControllerRunning() {
		boolean isRunning = false;
		String state = getControllerState();
		if( state.equalsIgnoreCase("running")) isRunning = true;
		return isRunning;
	}
	/**
	 * Determine whether or not the diagram is alerting.
	 */
	@Override
	public boolean isAlerting(Long projectId,Long resid) {
		Boolean result = null;
		try {
			result = (Boolean)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "isAlerting",projectId,resid);
			log.debugf("%s.isAlerting ...%d:%d = %s",TAG,projectId,resid,result);
		}
		catch(Exception ge) {
			log.infof("%s.isAlerting: GatewayException (%s) for project %d, resource %d",TAG,ge.getMessage(),projectId.longValue(),resid.longValue());
		}
		if( result==null ) return false;
		return result.booleanValue();
	}
	
	/**
	 * Query a block in the gateway for list of the blocks connected to the named port. 
	 * @param diagramId of the parent diagram
	 * @param blockId identifier of the block
	 * @param port name of the anchor of interest
	 * @return a list of blocks connected to the named port.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(String diagramId,String blockId,String portName) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksConnectedAtPort",diagramId,blockId,portName);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksConnectedAtPort: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(String diagramId, String blockName) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksDownstreamOf",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksDownstreamOf: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksForTag(String tagpath) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksForTag",tagpath);
		}
		catch(Exception ge) {
			log.infof("%s.queryDiagram: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * Query a diagram in the gateway for list of blocks that it knows about. 
	 * This is a debugging aid. 
	 * 
	 * @return a list of blocks known to the diagram.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksInDiagram(String diagramId) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "queryDiagram",diagramId);
		}
		catch(Exception ge) {
			log.infof("%s.queryDiagram: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * Query a diagram in the gateway for list of its blocks that are downstream
	 * of the specified block. If any of those blocks are sinks, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks downstream of the specified block.
	 */
	@SuppressWarnings("unchecked")
	public List<SerializableBlockStateDescriptor> listBlocksGloballyDownstreamOf(String diagramId,String blockId) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksGloballyDownstreamOf",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksGloballyDownstreamOf: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * Query a diagram in the gateway for list of its blocks that are upstream
	 * of the specified block. If any of those blocks are sources, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks upstream of the specified block.
	 */
	@SuppressWarnings("unchecked")
	public List<SerializableBlockStateDescriptor> listBlocksGloballyUpstreamOf(String diagramId,String blockId) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksGloballyUpstreamOf",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksGloballyUpstreamOf: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(String diagramId, String blockId) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksUpstreamOf",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksUpstreamOf: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listConfigurationErrors() {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listConfigurationErrors");
		}
		catch(Exception ge) {
			log.infof("%s.listConfigurationErrors: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}

	
	/**
	 * @param diagramId identifier of the diagram to be queried, a String
	 * @param className fully qualified class name of blocks to be listed
	 * @return a list of ids for blocks owned by a specified diagram that are of a
	 *         specified class.
	 */
	@SuppressWarnings({ "unchecked" })
	@Override
	public List<SerializableBlockStateDescriptor> listDiagramBlocksOfClass(String diagramId,String className) {
		log.debugf("%s.getDiagramBlocksOfClass: for diagram %s (%s)",TAG,diagramId,className);
		List<SerializableBlockStateDescriptor> blockList = new ArrayList<>();
		try {
			blockList = (List<SerializableBlockStateDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listDiagramBlocksOfClass",diagramId,className);
		}
		catch(Exception ge) {
			log.infof("%s.listDiagramBlocksOfClass: GatewayException (%s)",TAG,ge.getMessage());
		}
		return blockList;
	}
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableResourceDescriptor> listDiagramDescriptors(String projectName) {
		log.debugf("%s.listDiagramDescriptors for %s ...",TAG,projectName);
		List<SerializableResourceDescriptor> result = new ArrayList<>();
		try {
			result = (List<SerializableResourceDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listDiagramDescriptors",projectName);
		}
		catch(Exception ge) {
			log.infof("%s.listDiagramDescriptors: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * Query the gateway for list of resources that the block controller knows about. 
	 * This is a debugging aid. 
	 * 
	 * @return a list of resources known to the BlockController.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableResourceDescriptor> listResourceNodes() {
		List<SerializableResourceDescriptor> result = null;
		try {
			result = (List<SerializableResourceDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listResourceNodes");
		}
		catch(Exception ge) {
			log.infof("%s.listResourceNodes: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}


	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listSinksForSource(String diagramId, String blockName) {
		List<SerializableBlockStateDescriptor> blockList = new ArrayList<>();
		try {
			blockList = (List<SerializableBlockStateDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listSinksForSource",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.listSinksForSource: GatewayException (%s)",TAG,ge.getMessage());
		}
		return blockList;
	}
	
	
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listSourcesForSink(String diagramId, String blockName) {
		List<SerializableBlockStateDescriptor> blockList = new ArrayList<>();
		try {
			blockList = (List<SerializableBlockStateDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listSourcesForSink",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.listSourcesForSink: GatewayException (%s)",TAG,ge.getMessage());
		}
		return blockList;
	}
	
	@Override
	public String pathForBlock(String diagramId,String blockName) {
		String path = "";
		try {
			path = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID,"pathForBlock",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.pathForBlock: GatewayException (%s)",TAG,ge.getMessage());
		}
		return path;
	}
	/** 
	 * @param nodeId UUID as a String of a node in the navigation tree
	 * @return a slash-separated path to the specified node. The path 
	 *         root is a slash representing the top node of the navigation tree.
	 */
	@Override
	public String pathForNode(String nodeId) {
		String path = "";
		try {
			path = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID,"pathForNode",nodeId);
		}
		catch(Exception ge) {
			log.infof("%s.pathForNode: GatewayException (%s)",TAG,ge.getMessage());
		}
		return path;
	}
	/**
	 * Post a (simulated) block result on its output.
	 * @param diagramId the parent diagram
	 * @param blockId
	 * @param port
	 * @param value
	 */
	public void postResult(String diagramId,String blockId,String port,String value) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "postResult",diagramId,blockId,port,value);
		}
		catch(Exception ge) {
			log.infof("%s.postResult: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	
	/**
	 * Execute reset() on a specified block
	 */
	@Override
	public void resetBlock(String diagramId,String blockName) {
		log.debugf("%s.resetBlock ...",TAG);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "resetBlock",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.resetBlock: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	
	/**
	 * Execute reset() on every block on the diagram
	 */
	@Override
	public void resetDiagram(String diagramId) {
		log.debugf("%s.resetDiagram ...",TAG);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "resetDiagram",diagramId);
		}
		catch(Exception ge) {
			log.infof("%s.resetDiagram: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Execute reset() on a specified block
	 */
	@Override
	public void restartBlock(String diagramId,String blockName) {
		log.debugf("%s.restartBlock ...",TAG);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "restartBlock",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.restartBlock: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Determine whether or not the indicated resource is known to the controller.
	 */
	@Override
	public boolean resourceExists(Long projectId,Long resid) {
		Boolean result = null;
		try {
			result = (Boolean)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "resourceExists",projectId,resid);
			log.debugf("%s.resourceExists ...%d:%d = %s",TAG,projectId,resid,result);
		}
		catch(Exception ge) {
			log.infof("%s.resourceExists: GatewayException (%s)",TAG,ge.getMessage());
		}
		if( result==null ) return false;
		return result.booleanValue();
	}

	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a tree-path.
	 * There may be no successful recipients.
	 * 
	 * @param diagramId
	 * @param className filter of the receiver blocks to be targeted.
	 * @param command string of the signal.
	 */
	@Override
	public boolean sendLocalSignal(String diagramId, String command,String message,String arg) {
		log.infof("%s.sendLocalSignal for %s %s %s %s...",TAG,diagramId,command,message,arg);
		boolean result = false;
		try {
			Boolean value = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "sendLocalSignal",diagramId,command,message,arg);
			if( value!=null ) result = value.booleanValue();
		}
		catch(Exception ex) {
			log.infof("%s.sendLocalSignal: Exception (%s)",TAG,ex.getMessage());
		}
		return result;
	}
	/**
	 * Send a signal directly to a specified block.
	 * This is a "local" transmission. The signal timestamp is "now".
	 * 
	 * @param diagramId diagram identifier
	 * @param command string of the signal.
	 * @param message command payload
	 * @return true on success
	 */
	@Override
	public boolean sendSignal(String diagramId,String blockName,String command,String message) {
		log.infof("%s.sendSignal for %s:%s %s %s...",TAG,diagramId,blockName,command,message);
		boolean result = false;
		try {
			Boolean value = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "sendSignal",diagramId,blockName,command,message);
			if( value!=null ) result = value.booleanValue();
		}
		catch(Exception ex) {
			log.infof("%s.sendSignal: Exception (%s)",TAG,ex.getMessage());
		}
		return result;
	}


	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a tree-path.
	 * There may be no successful recipients. The signals are timestamped with
	 * the specified time
	 * 
	 * @param diagramId
	 * @param arg filter of the receiver blocks to be targeted.
	 * @param command string of the signal.
	 */
	@Override
	public boolean sendTimestampedSignal(String diagramId, String command,String message,String arg,long time) {
		log.infof("%s.sendTimestampedSignal for %s %s %s %s...",TAG,diagramId,command,message,arg);
		boolean result = false;
		try {
			Boolean value = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "sendTimestampedSignal",diagramId,command,message,arg,new Long(time));
			if( value!=null ) result = value.booleanValue();
		}
		catch(Exception ex) {
			log.infof("%s.sendTimestampedSignal: Exception (%s)",TAG,ex.getMessage());
		}
		return result;
	}
	/**
	 * Change the state of every diagram in the named application
	 * to the specified state.
	 * @param appname name of the application
	 * @param state new diagram state
	 */
	@Override
	public void setApplicationState(String appname, String state) {
		log.infof("%s.setApplicationState for %s to %s...",TAG,appname,state);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setApplicationState",appname,state);
		}
		catch(Exception ex) {
			log.infof("%s.setApplicationState: Exception (%s)",TAG,ex.getMessage());
		}
	}

	/** Update all changed properties for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 */
	@Override
	public void setBlockProperties(UUID duuid,UUID buuid, Collection<BlockProperty> props ) {
		String diagId  = duuid.toString();
		String blockId = buuid.toString();
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		String json="";
		try {
			json = mapper.writeValueAsString(props);
		}
		catch(Exception ge) {
			log.warnf("%s: toJson (%s)",TAG,ge.getMessage());
		}
		log.tracef("%s: json properties = %s",TAG,json);
		log.debugf("%s.setBlockProperties: %s %s %s %s: %s", TAG, diagId,blockId, json);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "setBlockProperties", diagId,blockId, json);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockProperties: GatewayException (%s)",TAG,ge.getMessage());
		}		
	}

	/** Update a single changed property for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 * @param property the changed property
	 */
	@Override
	public void setBlockProperty(UUID duuid,UUID buuid,BlockProperty property ) {
		String diagId  = duuid.toString();
		String blockId = buuid.toString();
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		String json="";
		try {
			json = mapper.writeValueAsString(property);
		}
		catch(Exception ge) {
			log.warnf("%s: toJson (%s)",TAG,ge.getMessage());
		}
		log.tracef("%s: json property = %s",TAG,json);
		log.debugf("%s.setBlockProperty: %s %s %s", TAG, diagId,blockId, json);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "setBlockProperty", diagId,blockId, json);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockProperty: GatewayException (%s)",TAG,ge.getMessage());
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
	@Override
	public void setBlockPropertyValue(String diagramId,String bname,String pname,String value )  {
		log.debugf("%s.setBlockPropertyValue: %s %s %s=%s", TAG, diagramId,bname, pname,value);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "setBlockPropertyValue", diagramId,bname, pname,value);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockPropertyValue: GatewayException (%s)",TAG,ge.getMessage());
		}		
	}
	/** 
	 * Drive a block to the specified state. 
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname 
	 * @param state the new state of the block. The value will be coerced into a truth-value in the gateway 
	 */
	@Override
	public void setBlockState(String diagramId,String bname,String state ) {
		log.debugf("%s.setBlockState ... %s:%s %s",TAG,diagramId,bname,state);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setBlockState",diagramId,bname,state);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockState: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	@Override
	public void setDiagramState(Long projectId, Long resourceId, String state) {
		log.debugf("%s.setDiagramState ... %d:%d %s",TAG,projectId.longValue(),resourceId.longValue(),state);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setDiagramState",projectId,resourceId,state);

		}
		catch(Exception ge) {
			log.infof("%s.setDiagramState: GatewayException (%s)",TAG,ge.getMessage());
		}
	}

	@Override
	public void setDiagramState(String diagramId, String state) {
		log.debugf("%s.setDiagramState ... %s %s",TAG,diagramId,state);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setDiagramState",diagramId,state);

		}
		catch(Exception ge) {
			log.infof("%s.setDiagramState: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Tell the testing timer about the difference between test time
	 * and current time.
	 * @param offset the difference between test time and current time
	 *        ~ msecs. A positive number implies that the test time is
	 *        in the past.
	 */
	public void setTestTimeOffset(long offset) {
		log.infof("%s.setTestTimeOffset ... %s",TAG,String.valueOf(offset));
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setTestTimeOffset",new Long(offset));
		}
		catch(Exception ge) {
			log.infof("%s.setTestTimeOffset: GatewayException (%s:%s)",TAG,ge.getClass().getName(),ge.getMessage());
		}
	}
	
	/**
	 * Set a clock rate factor for isolation mode only. We set in the SFC module
	 * as well. If that module is not present, then we simply ignore the exception.
	 * @param factor the amount to speed up or slow down the clock. A value greater
	 *        than one implies an accelerated clock.
	 */
	public void setTimeFactor(double factor) {
		log.infof("%s.setTimeFactor ... %s",TAG,String.valueOf(factor));
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setTimeFactor",new Double(factor));
		}
		catch(Exception ge) {
			log.infof("%s.setTimeFactor: GatewayException (%s:%s)",TAG,ge.getClass().getName(),ge.getMessage());
		}
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.SFC_MODULE_ID, "setTimeFactor",new Double(factor));
		}
		catch(Exception ignore) {}
	}

	/**
	 * Save a value into the HSQL database table associated with the toolkit. The 
	 * table contains name-value pairs, so any name is allowable. We also execute
	 * this method on behalf of the SFC-module in case there are any side-effects
	 * of saving particular parameters.
	 * @param propertyName name of the property for which a value is to be set
	 * @param the new value of the property.
	 */
	@Override
	public void setToolkitProperty(String propertyName,String value) {
		log.tracef("%s.setToolkitProperty ... %s=%s",TAG,propertyName,value);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setToolkitProperty",propertyName,value);
		}
		catch(Exception ge) {
			log.infof("%s.setToolkitProperty: GatewayException (%s:%s)",TAG,ge.getClass().getName(),ge.getMessage());
		}
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.SFC_MODULE_ID, "setToolkitProperty",propertyName,value);
		}
		catch(Exception ignore) {}
	}

	/**
	 * Define a watermark for a diagram. This is shown only in the designer. 
	 */
	public void setWatermark(String diagramId,String text) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
									BLTProperties.MODULE_ID, "setWatermark",diagramId,text);
		}
		catch(Exception ge) {
			log.infof("%s.setWatermark: GatewayException (%s)",TAG,ge.getMessage());
		}
	}
	/**
	 * Start the block execution engine in the gateway.
	 */
	@Override
	public void startController() {
		log.debugf("%s.startController ...",TAG);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "startController");
		}
		catch(Exception ge) {
			log.infof("%s.startController: GatewayException (%s)",TAG,ge.getMessage());
		}
	}

	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	@Override
	public void stopController() {
		log.debugf("%s.stopController ...",TAG);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "stopController");
		}
		catch(Exception ge) {
			log.infof("%s.stopController: GatewayException (%s)",TAG,ge.getMessage());
		}
	}

	/**
	 * Direct the blocks in a specified diagram to report their
	 * status values. This is in order to update the UI. 
	 */
	@Override
	public void triggerStatusNotifications() {
		//log.infof("%s.triggerStatusNotifications...",TAG);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "triggerStatusNotifications");
		}
		catch(Exception ex) {
			log.infof("%s.triggerStatusNotifications: Exception (%s:%s)",TAG,ex.getClass().getName(),ex.getMessage());
		}
	}

	/** Update connections for a block. New connections will be added, old connections
	 * may undergo a type conversion.  
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 */
	@Override
	public void updateBlockAnchors(UUID duuid,UUID buuid, Collection<SerializableAnchor> anchors ) {
		String diagId  = duuid.toString();
		String blockId = buuid.toString();
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		String json="";
		try {
			json = mapper.writeValueAsString(anchors);
		}
		catch(Exception ge) {
			log.warnf("%s: toJson (%s)",TAG,ge.getMessage());
		}
		log.tracef("%s: json properties = %s",TAG,json);
		log.debugf("%s.setBlockProperties: %s %s %s %s: %s", TAG, diagId,blockId, json);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "updateBlockAnchors", diagId,blockId, json);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockProperties: GatewayException (%s)",TAG,ge.getMessage());
		}		
	}
}
