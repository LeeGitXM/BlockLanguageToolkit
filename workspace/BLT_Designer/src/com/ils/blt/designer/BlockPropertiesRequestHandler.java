/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.ils.block.common.BlockProperty;
import com.ils.block.common.PalettePrototype;
import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;



/**
 *  This handler provides a way to request properties of blocks shown in the diagram.
 *  The request is relayed to the Gateway scope via an RPC call.
 */
public class BlockPropertiesRequestHandler  {
	private final static String TAG = "BlockPropertiesRequestHandler";
	private final LoggerEx log;

	/**
	 * Constructor adds common attributes that are needed to generate unique keys to identify
	 * blocks and connectors.
	 */
	public BlockPropertiesRequestHandler()  {
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
			log.infof("%s: isControllerRunning ... %s",TAG,state);
		}
		catch(Exception ge) {
			log.infof("%s: isControllerRunning: GatewayException (%s)",TAG,ge.getMessage());
		}
		return result;
	}
	
	/**
	 * Start the block execution engine in the gateway.
	 */
	public void startController() {
		log.infof("%s: startController ...",TAG);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "startController");
		}
		catch(Exception ge) {
			log.infof("%s: startController: GatewayException (%s)",TAG,ge.getMessage());
		}
	}

	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	public void stopController() {
		log.infof("%s: stopController ...",TAG);

		try {
			GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "stopController");
		}
		catch(Exception ge) {
			log.infof("%s: stopController: GatewayException (%s)",TAG,ge.getMessage());
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
		log.infof("%s: getBlockProperties: for block %s (%s)",TAG,blockId.toString(),className);
		BlockProperty[] result = null;
		List<String> jsonList = new ArrayList<String>();
		try {
			jsonList = (List<String>)GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockProperties",new Long(projectId),new Long(resourceId),blockId.toString(),className);
		}
		catch(Exception ge) {
			log.infof("%s: getBlockProperties: GatewayException (%s)",TAG,ge.getMessage());
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


	/**
	 * Obtain a list of attribute-value pairs for the class represented by this connection.
	 * In particular we need a list of possible connection ports supported by the input and
	 * output blocks, as well as the ports currently used.
	 * 
	 * Before we pass on the request we beef up the key by pre-pending the tree-path
	 * to the diagram.
	 * 
	 * @param projectId
	 * @param resourceId
	 * @param connectionId UUID of the subject connection
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the connection object.
	 */
	public String getConnectionAttributes(Long projectId, Long resourceId,String connectionId, String json) {
		String result = "";
		try {
			//result = BlockPropertiesScriptFunctions.getConnectionAttributes(projectId,resourceId,connectionId,json);
		}
		catch(Exception ex) {
			log.info(TAG+"getConnectionAttributes: Exception ("+ex.getMessage()+")");
		}
		return result;
	}


	@SuppressWarnings("unchecked")
	public List<PalettePrototype> getBlockPrototypes() {
		log.infof("%s: getBlockPrototypes ...",TAG);
		List<PalettePrototype> result = new ArrayList<PalettePrototype>();
		List<String> jsonList = new ArrayList<String>();
		try {
			jsonList = (List<String> )GatewayConnectionManager.getInstance().getGatewayInterface().moduleInvoke(
					BLTProperties.MODULE_ID, "getBlockPrototypes");
		}
		catch(Exception ge) {
			log.infof("%s: getBlockPrototypes: GatewayException (%s)",TAG,ge.getMessage());
		}
		
		if( jsonList!=null) {
			
			for( String json:jsonList ) {
				log.tracef("%s getBlockPrototypes: %s",TAG,json);
				PalettePrototype bp = PalettePrototype.createPrototype(json);
				result.add(bp);
			}
		}
		return result;
	}

}
