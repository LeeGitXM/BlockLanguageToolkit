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
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;



/**
 *  This handler is designed for use by Java code in the designer that needs 
 *  facilities from the Gateway. It provides a way to request/set properties of 
 *  diagrams, blocks and connections.
 *  
 *  Each request is relayed to the Gateway scope via an RPC call.
 */
public class BlockRequestHandler  {
	private final static String TAG = "BlockRequestHandler";
	private final LoggerEx log;

	/**
	 * Constructor adds common attributes that are needed to generate unique keys to identify
	 * blocks and connectors.
	 */
	public BlockRequestHandler()  {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Start the block execution engine in the gateway.
	 */
	public boolean isControllerRunning() {

		boolean result = false;
		try {
			// Returns either "running" or "stopped"
			String state = (String)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getControllerState");
			if( state.equalsIgnoreCase("running")) result = true;
			log.infof("%s.isControllerRunning ... %s",TAG,state);
		}
		catch(Exception ge) {
			log.infof("%s.isControllerRunning: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	
	/**
	 * Start the block execution engine in the gateway.
	 */
	public void startController() {
		log.infof("%s.startController ...",TAG);

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
		log.infof("%s.stopController ...",TAG);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "stopController");
		}
		catch(Exception ge) {
			log.infof("%s.stopController: GatewayException (%s)",TAG,ge.getMessage());
		}
	}


	public void enableDiagram(Long projectId, Long resourceId, Boolean flag) {
		// TODO Auto-generated method stub
		
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
	public BlockProperty[] getBlockProperties(long projectId,long resourceId,UUID blockId,String className) {
		log.infof("%s.getBlockProperties: for block %s (%s)",TAG,blockId.toString(),className);
		BlockProperty[] result = null;
		List<String> jsonList = new ArrayList<String>();
		try {
			jsonList = (List<String>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockProperties",new Long(projectId),new Long(resourceId),blockId.toString(),className);
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
		log.infof("%s.getBlockPrototypes ...",TAG);
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
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a treepath.
	 * There may be no successful recipients.
	 * 
	 * @param projectName
	 * @param diagramPath
	 * @param className filter of the receiver blocks to be targeted.
	 * @param command string of the signal.
	 */
	public void sendLocalSignal(String projectName, String diagramPath,String className, String command) {
		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "sendLocalSignal",projectName,diagramPath,className,command);
		}
		catch(Exception ex) {
			log.infof("%s.sendLocalSignal: Exception (%s)",TAG,ex.getMessage());
		}
	}
}
