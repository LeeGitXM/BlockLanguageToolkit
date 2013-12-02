/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.workspace;

import com.ils.blt.designer.BlockPropertiesScriptFunctions;
import com.ils.jgx.common.PropertiesHandlerInterface;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;



/**
 *  This handler provides a way to request properties of blocks shown in the diagram.
 *  The request is relayed to the Gateway scope via an RPC call.
 */
public class PropertiesRequestHandler implements PropertiesHandlerInterface  {
	private final static String TAG = "PropertiesRequestHandler";
	private final LoggerEx log;
	private final DesignerContext context;
	private final long projectId;
	private final long resourceId;

	/**
	 * Constructor adds common attributes that are needed to generate unique keys to identify
	 * blocks and connectors.
	 * 
	 * @param ctxt Designer context
	 */
	public PropertiesRequestHandler(DesignerContext ctxt,long proj,long res)  {
		this.context = ctxt;
		this.projectId = proj;
		this.resourceId= res;
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	
	
	/**
	 * Obtain a list of attribute-value pairs for the class represented by this block.
	 * Before we pass on the request we beef up the key by pre-pending the tree-path
	 * to the diagram.
	 * 
	 * @param json string representing an array of attributes
	 * @param cellId a string representing the id of the cell within the diagram.
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the block object.
	 */
	@Override
	public String getBlockAttributes(String cellId,String json) {
		log.info(TAG+"getBlockAttributes:"+cellId+"="+json);
		String result = "";
		try {
			result = BlockPropertiesScriptFunctions.getBlockAttributes(projectId,resourceId,cellId, json);
		}
		catch(Exception ex) {
			log.info(TAG+"getBlockAttributes: Exception ("+ex.getMessage()+")");
		}
		log.info(TAG+"getBlockAttributes:returned ="+result);
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
	 * @param json string representing an array of attributes
	 * @param cellId a string representing the id of the cell within the diagram.
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the connection object.
	 */
	@Override
	public String getConnectionAttributes(String cellId,String json) {
		String result = "";
		try {
			result = BlockPropertiesScriptFunctions.getConnectionAttributes(projectId,resourceId,cellId,json);
		}
		catch(Exception ex) {
			log.info(TAG+"getConnectionAttributes: Exception ("+ex.getMessage()+")");
		}
		return result;
	}

}
