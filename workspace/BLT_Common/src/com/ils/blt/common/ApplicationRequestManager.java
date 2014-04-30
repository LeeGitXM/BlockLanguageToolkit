/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.ils.block.common.BlockProperty;
import com.ils.block.common.PalettePrototype;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
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
public class ApplicationRequestManager  {
	private final static String TAG = "ApplicationRequestManager";
	private final LoggerEx log;

	/**
	 * Constructor adds common attributes that are needed to generate unique keys to identify
	 * blocks and connectors.
	 */
	public ApplicationRequestManager()  {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Determine whether or not the engine is running.
	 */
	public boolean isControllerRunning() {
		boolean isRunning = false;
		String state = getControllerState();
		if( state.equalsIgnoreCase("running")) isRunning = true;
		return isRunning;
	}

	/**
	 * Determine whether or not the engine is running.
	 */
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
	
	
	

	public void enableDiagram(Long projectId, Long resourceId, Boolean flag) {
		log.debugf("%s.enableDiagram ... %d:%d %s",TAG,projectId.longValue(),resourceId.longValue(),flag.toString());
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "enableDiagram",projectId,resourceId,flag);

		}
		catch(Exception ge) {
			log.infof("%s.enableDiagram: GatewayException (%s)",TAG,ge.getMessage());
		}
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
	public BlockProperty[] getBlockProperties(String className,long projectId,long resourceId,UUID blockId) {
		log.infof("%s.getBlockProperties: for block %s (%s)",TAG,blockId.toString(),className);
		BlockProperty[] result = null;
		List<String> jsonList = new ArrayList<String>();
		try {
			jsonList = (List<String>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockProperties",className,new Long(projectId),new Long(resourceId),blockId.toString());
		}
		catch(Exception ge) {
			log.infof("%s.getBlockProperties: GatewayException (%s)",TAG,ge.getMessage());
		}
				
		if( jsonList!=null) {
			result = new BlockProperty[jsonList.size()];
			int index = 0;
			for( String json:jsonList ) {
				log.tracef("%s: property: %s",TAG,json);
				BlockProperty bp = BlockProperty.createProperty(json);
				result[index]=bp;
				index++;
			}
		}
		else 
		{
			result = new BlockProperty[0];
		}
		return result;
	}


	@SuppressWarnings("unchecked")
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
	
	@SuppressWarnings("unchecked")
	public List<String> getDiagramTreePaths(String projectName) {
		log.infof("%s.getDiagramTreePaths for %s ...",TAG,projectName);
		List<String> result = null;
		try {
			result = (List<String> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getDiagramTreePaths",projectName);
		}
		catch(Exception ge) {
			log.infof("%s.getDiagramTreePaths: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	/**
	 * @return TRUE if the specified diagram is enabled.
	 */
	public boolean isDiagramEnabled(Long projectId, Long resourceId) {
		Boolean result = false;
		try {
			result = (Boolean)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "isDiagramEnabled",projectId,resourceId);
			log.debugf("%s.isDiagramEnabled ... %s",TAG,result.toString());
		}
		catch(Exception ge) {
			log.infof("%s.isDiagramEnabled: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result.booleanValue();
	}
	/**
	 * Query the gateway for list of resources that the block controller knows about. 
	 * This is a debugging aid. 
	 * 
	 * @return a list of resources known to the BlockController.
	 */
	@SuppressWarnings("unchecked")
	public List<SerializableResourceDescriptor> queryControllerResources() {
		List<SerializableResourceDescriptor> result = null;
		try {
			result = (List<SerializableResourceDescriptor> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "queryControllerResources");
		}
		catch(Exception ge) {
			log.infof("%s.queryControllerResources: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a tree-path.
	 * There may be no successful recipients.
	 * 
	 * @param projectName
	 * @param diagramPath
	 * @param className filter of the receiver blocks to be targeted.
	 * @param command string of the signal.
	 */
	public boolean sendLocalSignal(String projectName, String diagramPath,String className, String command) {
		log.infof("%s.sendLocalSignal for %s %s %s %s...",TAG,projectName,diagramPath,className,command);
		Boolean result = null;
		try {
			result = GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "sendLocalSignal",projectName,diagramPath,className,command);
		}
		catch(Exception ex) {
			log.infof("%s.sendLocalSignal: Exception (%s)",TAG,ex.getMessage());
		}
		return result.booleanValue();
	}
	
	
	/**
	 * Start the block execution engine in the gateway.
	 */
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

}
