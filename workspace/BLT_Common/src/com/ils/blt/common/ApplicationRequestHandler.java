/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
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
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.persistence.ToolkitProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
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
	private final static String CLSS = "ApplicationRequestHandler";
	private final LoggerEx log;

	/**
	 * Constructor adds common attributes that are needed to generate unique keys to identify
	 * blocks and connectors.
	 */
	public ApplicationRequestHandler()  {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	@Override
	public List<SerializableResourceDescriptor> childNodes(ProjectResourceId nodeId) {
		return null;
	}

	/**
	 * Remove all current diagrams from the controller.
	 * @param projectName project to which this applies
	 */
	@Override
	public void clearController() {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "clearController");
			log.debugf("%s.clearController ...",CLSS);
		}
		catch(Exception ge) {
			log.infof("%s.clearController: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Clear any watermark on a diagram.
	 * @param diagramId unique identifier of he diagram as a string 
	 */
	@Override
	public void clearWatermark(ProjectResourceId diagramId) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
									BLTProperties.MODULE_ID, "clearWatermark",diagramId);
		}
		catch(Exception ge) {
			log.infof("%s.clearWatermark: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Create a ProjectResourceId object from String components. This is designed for Python
	 * scripts to easily generate resourceId inputs to the various methods.
	 */
	@Override
	public ProjectResourceId createResourceId(String projectName, String path, String type) {
		ResourceType rtype = new ResourceType(BLTProperties.MODULE_ID,type);
		ProjectResourceId resourceId = new ProjectResourceId(projectName,rtype,path);
		return resourceId;
	}
	/**
	 * Create a SQLTag memory tag given its path and data type.
	 * Create in both production and isolation
	 */
	@Override
	public void createTag(String projectName,DataType type,String path) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
									BLTProperties.MODULE_ID, "createTag",projectName,type,path);
		}
		catch(Exception ge) {
			log.infof("%s.createTag: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Delete a SQLTag given its path. The path must contain the
	 * provider name in brackets. Delete from both production and isolation
	 */
	@Override
	public void deleteTag(String projectName,String path) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
									BLTProperties.MODULE_ID, "deleteTag",projectName,path);
		}
		catch(Exception ge) {
			log.infof("%s.deleteTag: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Determine whether or not the indicated diagram is known to the controller.
	 * @param uuidString unique identifier of he diagram as a string
	 */
	@Override
	public boolean diagramExists(ProjectResourceId resourceId) {
		boolean result = false;
		
		try {
			Boolean value = (Boolean)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "diagramExists",resourceId);
			log.debugf("%s.diagramExists  ...%s = %s",CLSS,resourceId,result);
			if( value!=null ) result = value.booleanValue();
		}
		catch(Exception ge) {
			log.infof("%s.diagramExists: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	/**
	 * @param uuid unique identifier of he application as a string
	 */
	@Override
	public String getApplicationName(ProjectResourceId resourceId) {
		String name = "NULL";
		if( resourceId!=null) {
			log.infof("%s.getApplicationName... %s",CLSS,resourceId);
			try {
				name = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
						BLTProperties.MODULE_ID, "getApplicationName",resourceId);
			}
			catch(Exception ex) {
				log.infof("%s.getApplicationName: Exception (%s)",CLSS,ex.getMessage());
			};
		}
		return name;
	}

	/**
	 * @param diagramId unique identifier of the diagram as a string 
	 * @param blockName name of the block 
	 * @return the id of the named block.
	 */
	@Override
	public String getBlockId(ProjectResourceId diagramId, String blockName) {
		String id = "UNKNOWN";
		try {
			id = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockId",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.getBlockId: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return id;
	}
	/**
	 * Obtain a list of BlockProperty objects for the specified block. If the block is not known to the gateway
	 * it will be created.
	 * 
	 * @param className class of the block
	 * @param resourceId corresponding to the diagram
	 * @param blockId UUID of the block

	 * @return an array of block properties for the subject block
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<BlockProperty> getBlockProperties(String className,ProjectResourceId resourceId,UUID blockId) {
		log.debugf("%s.getBlockProperties: for block %s (%s)",CLSS,blockId.toString(),className);
		List<BlockProperty> result = null;
		List<String> jsonList = new ArrayList<String>();
		try {
			jsonList = (List<String>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockProperties",className,resourceId,blockId.toString());
		}
		catch(Exception ge) {
			log.infof("%s.getBlockProperties: GatewayException (%s)",CLSS,ge.getMessage());
		}
		result = new ArrayList<>();	
		if( jsonList!=null) {
			for( String json:jsonList ) {
				log.tracef("%s: property: %s",CLSS,json);
				BlockProperty bp = BlockProperty.createProperty(json);
				log.debugf("%s.getBlockProperties: %s",CLSS, bp.toString());
				result.add(bp);
			}
		}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<PalettePrototype> getBlockPrototypes() {
		log.infof("%s.getBlockPrototypes ...",CLSS);
		List<PalettePrototype> result = new ArrayList<PalettePrototype>();
		List<String> jsonList = new ArrayList<String>();
		try {
			jsonList = (List<String> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockPrototypes");
		}
		catch(Exception ge) {
			log.infof("%s.getBlockPrototypes: GatewayException (%s)",CLSS,ge.getMessage());
		}
		
		if( jsonList!=null) {
			for( String json:jsonList ) {
				log.infof("%s.getBlockPrototypes: %s",CLSS,json);
				PalettePrototype bp = PalettePrototype.createPrototype(json);
				result.add(bp);
			}
		}
		return result;
	}


	/**
	 * @param diagramId unique identifier of the diagram as a string 
	 * @param blockName name of the block 
	 * @return the current state of the specified block.
	 */
	@Override
	public String getBlockState(ProjectResourceId diagramId, String blockName) {
		String state = "UNKNOWN";
		try {
			state = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockState",diagramId,blockName);
			log.debugf("%s.getBlockState %s = %s",CLSS,blockName,state);
		}
		catch(Exception ge) {
			log.infof("%s.getBlockState: GatewayException (%s)",CLSS,ge.getMessage());
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
			log.debugf("%s.getControllerState ... %s",CLSS,state);
		}
		catch(Exception ge) {
			log.infof("%s.getControllerState: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return state;
	}

	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * database.  
	 * @param uuid identifier of an application or diagram
	 * @return database name
	 */
	@Override
	public String getDatabaseForId(ProjectResourceId uuid) {
		String db = "NONE";
		if( uuid!=null) {
			log.infof("%s.getDatabaseForUUID... %s",CLSS,uuid);
			try {
				db = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
						BLTProperties.MODULE_ID, "getDatabaseForUUID",uuid);
			}
			catch(Exception ex) {
				log.infof("%s.getDatabaseForUUID: Exception (%s)",CLSS,ex.getMessage());
			};
		}
		return db;
	}
	/**
	 * @param diagramId String representation of the diagram's internal Id.
	 * @return a descriptor for the diagram that corresponds to that Id.
	 */
	@Override
	public SerializableResourceDescriptor getDiagram(ProjectResourceId diagramId)  {
		SerializableResourceDescriptor result = null;
		try {
			result = (SerializableResourceDescriptor)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getDiagram",diagramId);
		}
		catch(Exception ge) {
			log.infof("%s.getDiagram: GatewayException (%s)",CLSS,ge.getMessage());
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
			log.infof("%s.getDatasourceNames: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return names;
	}
	/**
	 * @param blockId unique identifier of the block as a string 
	 */
	@Override
	public SerializableResourceDescriptor getDiagramForBlock(String blockId) {
		SerializableResourceDescriptor result = null;
		try {
			result = (SerializableResourceDescriptor)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getDiagramForBlock",blockId);
		}
		catch(Exception ge) {
			log.infof("%s.getDiagramForBlock: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	/**
	 * @return the current state of the specified diagram.
	 */
	@Override
	public DiagramState getDiagramState(ProjectResourceId resourceId) {
		DiagramState result = DiagramState.ACTIVE;
		try {
			String state = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getDiagramState",resourceId);
			log.debugf("%s.getDiagramState ... %s",CLSS,result.toString());
			result = DiagramState.valueOf(state);
		}
		catch(Exception ge) {
			log.infof("%s.getDiagramState: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}

	/**
	 * @return an explanation for the state of a block.
	 */
	@Override
	public String getExplanation(ProjectResourceId diagramId,String blockId) {
		String reason="";

		try {
			reason = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getExplanation",diagramId,blockId);
		}
		catch(Exception ex) {
			log.infof("%s.getExplanation: Exception (%s)",CLSS,ex.getMessage());
		};
		return reason;
	}
	@Override
	public String getFamilyName(ProjectResourceId uuid) {
		String name = "NULL UUID";
		if( uuid!=null ) {
			//log.infof("%s.getFamilyName... %s",CLSS,uuid);
			try {
				name = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
						BLTProperties.MODULE_ID, "getFamilyName",uuid);
			}
			catch(Exception ex) {
				log.infof("%s.getFamilyName: Exception (%s)",CLSS,ex.getMessage());
			};
		}
		return name;
	}
	/**
	 * @return the hostname of the gateway
	 */
	public String getGatewayHostname() {
		String hostname="";

		try {
			hostname = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getHostname");
		}
		catch(Exception ex) {
			log.infof("%s.getGatewayHostname: Exception (%s)",CLSS,ex.getMessage());
		};
		return hostname;
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @return internal details of a block for debugging purposes.
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalState(ProjectResourceId diagramId,String blockId) {
		//log.infof("%s.getInternalState ... %s,%s",TAG,diagramId,blockId);
		SerializableBlockStateDescriptor result = new SerializableBlockStateDescriptor();
		String json = null;
		try {
			// Returns either "running" or "stopped"
			json = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getInternalState",diagramId,blockId);
			log.debugf("%s.getInternalState ... %s",CLSS,json);
		}
		catch(Exception ge) {
			log.infof("%s.getInternalState: GatewayException (%s)",CLSS,ge.getMessage());
		}
		if( json!=null) {
			ObjectMapper mapper = new ObjectMapper();

			try {
				result = mapper.readValue(json, SerializableBlockStateDescriptor.class);
			} 
			catch (JsonParseException jpe) {
				log.warnf("%s: getInternalState parse exception (%s)",CLSS,jpe.getLocalizedMessage());
			}
			catch(JsonMappingException jme) {
				log.warnf("%s: getInternalState mapping exception (%s)",CLSS,jme.getLocalizedMessage());
			}
			catch(IOException ioe) {
				log.warnf("%s: getInternalState IO exception (%s)",CLSS,ioe.getLocalizedMessage());
			}
		}
		return result;
	}
	/**
	 * Find the name of the isolation datasource from the internal SQLite database. 
	 * @return isolation database name
	 */
	@Override
	public String getProjectIsolationDatabase(String projectName) {
		return getProjectToolkitProperty(projectName,ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE);
	}

	/**
	 * Find the name of the isolation tag provider from the internal SQLite database. 
	 * @return isolation tag provider name
	 */
	@Override
	public String getProjectIsolationTagProvider(String projectName) {
		return getProjectToolkitProperty(projectName,ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER);
	}
	/**
	 * Find the name of the production datasource from the internal SQLite database. 
	 * @return production database name
	 */
	@Override
	public String getProjectProductionDatabase(String projectName) {
		return getProjectToolkitProperty(projectName,ToolkitProperties.TOOLKIT_PROPERTY_DATABASE);
	}
	/**
	 * Find the name of the isolation tag provider from the internal SQLite database. 
	 * @return production tag provider name
	 */
	@Override
	public String getProjectProductionTagProvider(String projectName) {
		return getProjectToolkitProperty(projectName,ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER);
	}
	
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the binding (tag path) of a specified block property. If there is no binding,
	 *         return an empty string.
	 */
	@Override
	public Object getPropertyBinding(ProjectResourceId diagramId,String blockId,String propertyName) {
		Object result = null;
		try {
			result = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getPropertyBinding",diagramId,blockId,propertyName);
			log.debugf("%s.getPropertyBinding ... %s",CLSS,result.toString());
		}
		catch(Exception ge) {
			log.infof("%s.getPropertyBinding: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of a specified block property.
	 */
	@Override
	public Object getPropertyValue(ProjectResourceId diagramId,String blockId,String propertyName) {
		Object result = null;
		try {
			result = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getPropertyValue",diagramId,blockId,propertyName);
			log.debugf("%s.getPropertyValue ... %s",CLSS,result.toString());
		}
		catch(Exception ge) {
			log.infof("%s.getPropertyValue: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * tag provider.  
	 * @param uuid identifier of an application or diagram
	 * @return database name
	 */
	@Override
	public String getProviderForId(ProjectResourceId uuid) {
		String provider = "NONE";
		if( uuid!=null) {
			log.infof("%s.getProviderForUUID... %s",CLSS,uuid);
			try {
				provider = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
						BLTProperties.MODULE_ID, "getProviderForUUID",uuid);
			}
			catch(Exception ex) {
				log.infof("%s.getProviderForUUID: Exception (%s)",CLSS,ex.getMessage());
			};
		}
		return provider;
	}
	@Override
	public Date getTimeOfLastBlockStateChange(ProjectResourceId diagramId, String blockName) {
		Date result = null;
		try {
			result = (Date)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getTimeOfLastBlockStateChange",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.getTimeOfLastBlockStateChange: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	/**
	 * Retrieve the configured browser path from the ORM database HelpRecord. This is used for 
	 * context-sensitive help.
	 * @return the configured browser path (for Windows)
	 */
	public String getWindowsBrowserPath() {
		String result = null;
		try {
			result = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getWindowsBrowserPath");
			log.tracef("%s.getWindowsBrowserPath ... %s",CLSS,result);
		}
		catch(Exception ge) {
			log.infof("%s.getWindowsBrowserPath: GatewayException (%s:%s)",CLSS,ge.getClass().getName(),ge.getMessage());
		}
		return result;
	}
	/**
	 * Acquire a value from the SQLite database table associated with the toolkit. A
	 * empty string is returned if the string is not found, null if an exception is thrown.
	 * @param projectName name of the project to which the property is associated
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of the specified property.
	 */
	public String getProjectToolkitProperty(String projectName,String propertyName) {
		String result = null;
		//log.infof("%s.getToolkitProperty ... %s",TAG,propertyName);
		try {
			result = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getProjectToolkitProperty",projectName,propertyName);
			log.tracef("%s.getProjectToolkitProperty ... %s:%s = %s",CLSS,projectName,propertyName,result);
		}
		catch(Exception ge) {
			log.infof("%s.getProjectToolkitProperty: GatewayException (%s:%s)",CLSS,ge.getClass().getName(),ge.getMessage());
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
			log.tracef("%s.getToolkitProperty ... %s:%s = %s",CLSS,propertyName,result);
		}
		catch(Exception ge) {
			log.infof("%s.getToolkitProperty: GatewayException (%s:%s)",CLSS,ge.getClass().getName(),ge.getMessage());
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
	public boolean isAlerting(ProjectResourceId resid) {
		Boolean result = null;
		try {
			result = (Boolean)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "isAlerting",resid);
			log.debugf("%s.isAlerting ...%s:%s = %s",CLSS,resid.getProjectName(),resid.getResourcePath().getPath().toString(),result);
		}
		catch(Exception ge) {
			log.infof("%s.isAlerting: GatewayException (%s) for project %s, resource %s",CLSS,ge.getMessage(),resid.getProjectName(),resid.getResourcePath().getPath().toString());
		}
		if( result==null ) return false;
		return result.booleanValue();
	}
	
	/**
	 * Query a block in the gateway for list of the blocks connected to the named port. 
	 * @param diagramId of the parent diagram
	 * @param blockId identifier of the block
	 * @param portName of the anchor of interest
	 * @return a list of blocks connected to the named port.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(ProjectResourceId diagramId,String blockId,String portName) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksConnectedAtPort",diagramId,blockId,portName);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksConnectedAtPort: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(ProjectResourceId diagramId, String blockName) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksDownstreamOf",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksDownstreamOf: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksForTag(String projectName,String tagpath) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksForTag",projectName,tagpath);
		}
		catch(Exception ge) {
			log.infof("%s.queryDiagram: GatewayException (%s)",CLSS,ge.getMessage());
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
	public List<SerializableBlockStateDescriptor> listBlocksInDiagram(ProjectResourceId diagramId) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "queryDiagram",diagramId);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksInDiagram: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	/**
	 * @param className fully qualified class name of blocks to be listed
	 * @return a list of state descriptors for blocks that are of the specified class.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksOfClass(String projectName,String className) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksOfClass",projectName,className);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksOfClass: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	/**
	 * Query a diagram in the gateway for list of its blocks that are downstream
	 * of the specified block. If any of those blocks are sinks, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockId id of the block within the diagram
	 * @return a list of blocks downstream of the specified block.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksGloballyDownstreamOf(ProjectResourceId diagramId,String blockId) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksGloballyDownstreamOf",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksGloballyDownstreamOf: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	/**
	 * Query a diagram in the gateway for list of its blocks that are upstream
	 * of the specified block. If any of those blocks are sources, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockId id of the block within the diagram
	 * @return a list of blocks upstream of the specified block.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksGloballyUpstreamOf(ProjectResourceId diagramId,String blockId) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksGloballyUpstreamOf",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksGloballyUpstreamOf: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(ProjectResourceId diagramId, String blockId) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listBlocksUpstreamOf",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.listBlocksUpstreamOf: GatewayException (%s)",CLSS,ge.getMessage());
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
			log.infof("%s.listConfigurationErrors: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listSubscriptionErrors() {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listSubscriptionErrors");
		}
		catch(Exception ge) {
			log.infof("%s.listSubscriptionErrors: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}	
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listUnresponsiveBlocks(double hours, String className) {
		List<SerializableBlockStateDescriptor> result = null;
		try {
			result = (List<SerializableBlockStateDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listUnresponsiveBlocks",hours,className);
		}
		catch(Exception ge) {
			log.infof("%s.listUnresponsiveBlocks: GatewayException (%s)",CLSS,ge.getMessage());
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
	public List<SerializableBlockStateDescriptor> listDiagramBlocksOfClass(ProjectResourceId diagramId,String className) {
		log.debugf("%s.getDiagramBlocksOfClass: for diagram %s (%s)",CLSS,diagramId,className);
		List<SerializableBlockStateDescriptor> blockList = new ArrayList<>();
		try {
			blockList = (List<SerializableBlockStateDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listDiagramBlocksOfClass",diagramId,className);
		}
		catch(Exception ge) {
			log.infof("%s.listDiagramBlocksOfClass: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return blockList;
	}
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableResourceDescriptor> listDiagramDescriptors(String projectName) {
		log.debugf("%s.listDiagramDescriptors for %s ...",CLSS,projectName);
		List<SerializableResourceDescriptor> result = new ArrayList<>();
		try {
			result = (List<SerializableResourceDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listDiagramDescriptors",projectName);
		}
		catch(Exception ge) {
			log.infof("%s.listDiagramDescriptors: GatewayException (%s)",CLSS,ge.getMessage());
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
			log.infof("%s.listResourceNodes: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return result;
	}


	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listSinksForSource(ProjectResourceId diagramId, String blockId) {
		List<SerializableBlockStateDescriptor> blockList = new ArrayList<>();
		try {
			blockList = (List<SerializableBlockStateDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listSinksForSource",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.listSinksForSource: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return blockList;
	}
	
	
	@SuppressWarnings("unchecked")
	@Override
	public List<SerializableBlockStateDescriptor> listSourcesForSink(ProjectResourceId diagramId, String blockId) {
		List<SerializableBlockStateDescriptor> blockList = new ArrayList<>();
		try {
			blockList = (List<SerializableBlockStateDescriptor>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "listSourcesForSink",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.listSourcesForSink: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return blockList;
	}
	
	@Override
	public String pathForBlock(ProjectResourceId diagramId,String blockName) {
		String path = "";
		try {
			path = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID,"pathForBlock",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.pathForBlock: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return path;
	}
	/** 
	 * @param nodeId UUID as a String of a node in the navigation tree
	 * @return a slash-separated path to the specified node. The path 
	 *         root is a slash representing the top node of the navigation tree.
	 */
	@Override
	public String pathForNode(ProjectResourceId nodeId) {
		String path = "";
		try {
			path = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID,"pathForNode",nodeId);
		}
		catch(Exception ge) {
			log.infof("%s.pathForNode: GatewayException (%s)",CLSS,ge.getMessage());
		}
		return path;
	}
	/**
	 * Post a (simulated) block result on its output.
	 * @param diagramId the parent diagram
	 * @param blockId the block
	 * @param port anchor for the incoming connection
	 * @param value new value
	 */
	@Override
	public void postResult(ProjectResourceId diagramId,String blockId,String port,String value) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "postResult",diagramId,blockId,port,value);
		}
		catch(Exception ge) {
			log.infof("%s.postResult: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Execute propagate() on a specified block. This sends its current value to
	 * the output connections.
	 */
	@Override
	public void propagateBlockState(ProjectResourceId diagramId,String blockId) {
		log.debugf("%s.propagateBlockState ...",CLSS);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "propagateBlockState",diagramId,blockId);
		}
		catch(Exception ge) {
			log.infof("%s.propagateBlockState: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Not implemented. The only time that the application should read from the database is in the case of an import -
	 * snd that is a Gateway function.
	 */
	@Override
	public GeneralPurposeDataContainer readAuxData(ProjectResourceId resid, String nodeId, String provider, String db) {
		// TODO Auto-generated method stub
		return null;
	}
	/** Change the name of a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 * @param name the new name
	 */
	@Override
	public void renameBlock(ProjectResourceId duuid,String buuid,String name ) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "renameBlock",duuid,buuid,name);
		}
		catch(Exception ge) {
			log.infof("%s.renameBlock: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Rename a SQLTag given its path and new name. The path must contain the
	 * provider name in brackets.
	 */
	public void renameTag(String projectName,String name,String path) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "renameTag",projectName,name,path);
		}
		catch(Exception ge) {
			log.infof("%s.renameTag: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Execute reset() on a specified block
	 */
	@Override
	public void resetBlock(ProjectResourceId diagramId,String blockName) {
		log.debugf("%s.resetBlock ...",CLSS);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "resetBlock",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.resetBlock: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	
	/**
	 * Execute reset() on every block on the diagram
	 */
	@Override
	public void resetDiagram(ProjectResourceId diagramId) {
		log.debugf("%s.resetDiagram ...",CLSS);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "resetDiagram",diagramId);
		}
		catch(Exception ge) {
			log.infof("%s.resetDiagram: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Execute reset() on a specified block
	 */
	@Override
	public void restartBlock(ProjectResourceId diagramId,String blockName) {
		log.debugf("%s.restartBlock ...",CLSS);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "restartBlock",diagramId,blockName);
		}
		catch(Exception ge) {
			log.infof("%s.restartBlock: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Determine whether or not the indicated resource is known to the controller.
	 */
	@Override
	public boolean resourceExists(ProjectResourceId resid) {
		Boolean result = null;
		try {
			result = (Boolean)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "resourceExists",resid);
			log.debugf("%s.resourceExists ...%s:%s = %s",CLSS,resid.getProjectName(),resid.getResourcePath().getPath().toString(),result);
		}
		catch(Exception ge) {
			log.infof("%s.resourceExists: GatewayException (%s)",CLSS,ge.getMessage());
		}
		if( result==null ) return false;
		return result.booleanValue();
	}

	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a tree-path.
	 * There may be no successful recipients.
	 * 
	 * @param diagramId diagram identifier
	 * @param command string of the signal.
	 * @param message command payload
	 * @param arg an argument.
	 */
	@Override
	public boolean sendLocalSignal(ProjectResourceId diagramId, String command,String message,String arg) {
		log.infof("%s.sendLocalSignal for %s %s %s %s...",CLSS,diagramId,command,message,arg);
		boolean result = false;
		try {
			Boolean value = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "sendLocalSignal",diagramId,command,message,arg);
			if( value!=null ) result = value.booleanValue();
		}
		catch(Exception ex) {
			log.infof("%s.sendLocalSignal: Exception (%s)",CLSS,ex.getMessage());
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
	public boolean sendSignal(ProjectResourceId diagramId,String blockName,String command,String message) {
		log.infof("%s.sendSignal for %s:%s %s %s...",CLSS,diagramId,blockName,command,message);
		boolean result = false;
		try {
			Boolean value = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "sendSignal",diagramId,blockName,command,message);
			if( value!=null ) result = value.booleanValue();
		}
		catch(Exception ex) {
			log.infof("%s.sendSignal: Exception (%s)",CLSS,ex.getMessage());
		}
		return result;
	}


	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a tree-path.
	 * There may be no successful recipients. The signals are timestamped with
	 * the specified time
	 * 
	 * @param diagramId identifier of the diagram as a String
	 * @param arg filter of the receiver blocks to be targeted.
	 * @param command string of the signal.
	 */
	@Override
	public boolean sendTimestampedSignal(ProjectResourceId diagramId, String command,String message,String arg,long time) {
		log.infof("%s.sendTimestampedSignal for %s %s %s %s...",CLSS,diagramId,command,message,arg);
		boolean result = false;
		try {
			Boolean value = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "sendTimestampedSignal",diagramId,command,message,arg,time);
			if( value!=null ) result = value.booleanValue();
		}
		catch(Exception ex) {
			log.infof("%s.sendTimestampedSignal: Exception (%s)",CLSS,ex.getMessage());
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
	public void setApplicationState(String projectName,String appname, String state) {
		log.infof("%s.setApplicationState for %s:%s to %s...",CLSS,projectName,appname,state);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setApplicationState",projectName,appname,state);
		}
		catch(Exception ex) {
			log.infof("%s.setApplicationState: Exception (%s)",CLSS,ex.getMessage());
		}
	}

	/** Update all changed properties for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 */
	@Override
	public void setBlockProperties(ProjectResourceId duuid,UUID buuid, Collection<BlockProperty> props ) {
		String diagId  = duuid.toString();
		String blockId = buuid.toString();
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		String json="";
		try {
			json = mapper.writeValueAsString(props);
		}
		catch(Exception ge) {
			log.warnf("%s: toJson (%s)",CLSS,ge.getMessage());
		}
		log.debugf("%s.setBlockProperties: %s %s: %s", CLSS, diagId,blockId, json);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "setBlockProperties", diagId,blockId, json);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockProperties: GatewayException (%s)",CLSS,ge.getMessage());
		}		
	}

	/** Update a single changed property for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 * @param property the changed property
	 */
	@Override
	public void setBlockProperty(ProjectResourceId duuid,UUID buuid,BlockProperty property ) {
		String diagId  = duuid.toString();
		String blockId = buuid.toString();
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		String json="";
		try {
			json = mapper.writeValueAsString(property);
		}
		catch(Exception ge) {
			log.warnf("%s: toJson (%s)",CLSS,ge.getMessage());
		}
		log.tracef("%s: json property = %s",CLSS,json);
		log.debugf("%s.setBlockProperty: %s %s %s", CLSS, diagId,blockId, json);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "setBlockProperty", diagId,blockId, json);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockProperty: GatewayException (%s)",CLSS,ge.getMessage());
		}		
	}
	/** Change the binding on a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param blockId Id of the block as a string
	 * @param pname the changed property
	 * @param value the new binding of the property. The value must be a legal tag path 
	 */
	public void setBlockPropertyBinding(ProjectResourceId diagramId,String blockId,String pname,String value ) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "setBlockPropertyBinding", diagramId,blockId, pname,value);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockPropertyBinding: GatewayException (%s)",CLSS,ge.getMessage());
		}	
	}
	/** Change the value of a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname the name of the block
	 * @param pname the changed property
	 * @param value the new value of the property. The value will be coerced into the correct data type in the gateway 
	 */
	@Override
	public void setBlockPropertyValue(ProjectResourceId diagramId,String bname,String pname,String value )  {
		log.debugf("%s.setBlockPropertyValue: %s %s %s=%s", CLSS, diagramId,bname, pname,value);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "setBlockPropertyValue", diagramId,bname, pname,value);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockPropertyValue: GatewayException (%s)",CLSS,ge.getMessage());
		}		
	}
	/** 
	 * Drive a block to the specified state. 
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname name of the block
	 * @param state the new state of the block. The value will be coerced into a truth-value in the gateway 
	 */
	@Override
	public void setBlockState(ProjectResourceId diagramId,String bname,String state ) {
		log.debugf("%s.setBlockState ... %s:%s %s",CLSS,diagramId,bname,state);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setBlockState",diagramId,bname,state);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockState: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	@Override
	public void setDiagramState(ProjectResourceId resourceId, String state) {
		log.debugf("%s.setDiagramState ... %s:%s %s",CLSS,resourceId.getProjectName(),resourceId.getResourcePath().getPath().toString(),state);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setDiagramState",resourceId,state);

		}
		catch(Exception ge) {
			log.infof("%s.setDiagramState: GatewayException (%s)",CLSS,ge.getMessage());
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
		log.infof("%s.setTestTimeOffset ... %s",CLSS,String.valueOf(offset));
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setTestTimeOffset",offset);
		}
		catch(Exception ge) {
			log.infof("%s.setTestTimeOffset: GatewayException (%s:%s)",CLSS,ge.getClass().getName(),ge.getMessage());
		}
	}
	
	/**
	 * Set a clock rate factor for isolation mode only. We set in the SFC module
	 * as well. If that module is not present, then we simply ignore the exception.
	 * @param factor the amount to speed up or slow down the clock. A value greater
	 *        than one implies an accelerated clock.
	 */
	public void setProjectTimeFactor(String projectName,double factor) {
		log.infof("%s.setProjectTimeFactor ... %s",CLSS,String.valueOf(factor));
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setProjectTimeFactor",projectName,factor);
		}
		catch(Exception ge) {
			log.infof("%s.setProjectTimeFactor: GatewayException (%s:%s)",CLSS,ge.getClass().getName(),ge.getMessage());
		}
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.SFC_MODULE_ID, "setProjectTimeFactor",factor);
		}
		catch(Exception ignore) {}
	}

	/**
	 * Save a project-dependent value into the HSQL database table associated with the toolkit. The 
	 * table contains name-value pairs, so any name is allowable. We also execute
	 * this method on behalf of the SFC-module in case there are any side-effects
	 * of saving particular parameters.
	 * @param projectName name of project with which property is associated
	 * @param propertyName name of the property for which a value is to be set
	 * @param value the new value of the property.
	 */
	@Override
	public void setProjectToolkitProperty(String projectName,String propertyName,String value) {
		log.tracef("%s.setProjectToolkitProperty ... %s:%s=%s",CLSS,projectName,propertyName,value);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setProjectToolkitProperty",projectName,propertyName,value);
		}
		catch(Exception ge) {
			log.infof("%s.setProjectToolkitProperty: GatewayException (%s:%s)",CLSS,ge.getClass().getName(),ge.getMessage());
		}
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.SFC_MODULE_ID, "setProjectToolkitProperty",projectName,propertyName,value);
		}
		catch(Exception ignore) {}
	}
	/**
	 * Save a value into the HSQL database table associated with the toolkit. The 
	 * table contains name-value pairs, so any name is allowable. We also execute
	 * this method on behalf of the SFC-module in case there are any side-effects
	 * of saving particular parameters.
	 * @param projectName name of project with which property is associated
	 * @param propertyName name of the property for which a value is to be set
	 * @param value the new value of the property.
	 */
	@Override
	public void setToolkitProperty(String propertyName,String value) {
		log.tracef("%s.setToolkitProperty ... %s:%s=%s",CLSS,propertyName,value);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "setToolkitProperty",propertyName,value);
		}
		catch(Exception ge) {
			log.infof("%s.setToolkitProperty: GatewayException (%s:%s)",CLSS,ge.getClass().getName(),ge.getMessage());
		}
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.SFC_MODULE_ID, "setToolkitProperty",propertyName,value);
		}
		catch(Exception ignore) {}
	}
	/**
	 * Define a watermark for a diagram. This is shown only in the designer. 
	 * @param diagramId identifier of diagram to get the watermark
	 * @param text to be displayed
	 */
	@Override
	public void setWatermark(ProjectResourceId diagramId,String text) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
									BLTProperties.MODULE_ID, "setWatermark",diagramId,text);
		}
		catch(Exception ge) {
			log.infof("%s.setWatermark: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Start the block execution engine in the gateway.
	 */
	@Override
	public void startController() {
		log.debugf("%s.startController ...",CLSS);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "startController");
		}
		catch(Exception ge) {
			log.infof("%s.startController: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}

	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	@Override
	public void stopController() {
		log.debugf("%s.stopController ...",CLSS);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "stopController");
		}
		catch(Exception ge) {
			log.infof("%s.stopController: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}

	/**
	 * Direct the blocks in diagrams in a specified project to report their
	 * status values. This is in order to update the UI. 
	 */
	@Override
	public void triggerStatusNotifications(String projectName) {
		//log.infof("%s.triggerStatusNotifications...",TAG);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "triggerStatusNotifications",projectName);
		}
		catch(Exception ex) {
			log.infof("%s.triggerStatusNotifications: Exception (%s:%s)",CLSS,ex.getClass().getName(),ex.getMessage());
		}
	}

	/** Update connections for a block. New connections will be added, old connections
	 * may undergo a type conversion.  
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 */
	@Override
	public void updateBlockAnchors(ProjectResourceId diagId,String blockId, Collection<SerializableAnchor> anchors ) {
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		String json="";
		try {
			json = mapper.writeValueAsString(anchors);
		}
		catch(Exception ge) {
			log.warnf("%s: toJson (%s)",CLSS,ge.getMessage());
		}
		log.debugf("%s.updateBlockAnchors: %s %s = %s", CLSS, diagId,blockId,json);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
				BLTProperties.MODULE_ID, "updateBlockAnchors", diagId,blockId, json);
		}
		catch(Exception ge) {
			log.infof("%s.setBlockProperties: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
	/**
	 * Execute the setAux extension function in Gateway scope, for the specified resource.
	 * @param resid the resourceId of a node to be written
	 * @param nodeId
	 * @param container
	 * @param provider tag provider
	 * @param db data source
	 */
	@Override
	public synchronized void writeAuxData(ProjectResourceId resid,String nodeId,GeneralPurposeDataContainer container,String provider,String database) {
		//log.infof("%s.writeAuxData: proj %d, res %d, (%s,%s)",CLSS,projId,root,provider,database);
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "writeAuxData",resid,nodeId,container,provider,database);
		}
		catch(Exception ge) {
			log.infof("%s.writeAuxData: GatewayException (%s)",CLSS,ge.getMessage());
		}
	}
}
