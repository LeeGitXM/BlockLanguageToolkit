/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.gateway;

import java.util.Hashtable;

import com.ils.blt.common.BlockPropertiesInterface;
import com.ils.common.JavaToJson;
import com.ils.common.JsonToJava;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 *  The RPC Dispatcher is the point of entry for incoming RCP requests.
 *  Its purpose is simply to parse out a request and send it to the
 *  right handler. This class supports the aggregate of RPC interfaces.
 */
public class GatewayRpcDispatcher implements BlockPropertiesInterface  {
	private static String TAG = "GatewayRpcDispatcher";
	private final LoggerEx log;
	private final GatewayContext context;
	private final Long projectId;
	private final JsonToJava jsonToJava;
	private final JavaToJson javaToJson;

	/**
	 * Constructor. There is a separate dispatcher for each project.
	 */
	public GatewayRpcDispatcher(GatewayContext cntx,Long pid) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = cntx;
		this.projectId = pid;
		this.jsonToJava = new JsonToJava();
		this.javaToJson = new JavaToJson();
	}


	@Override
	public void enableDiagram(long projectId, long resourceId, boolean flag) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getBlockAttributes(long projectId, long resourceId,String blockId,String json) {
		log.debugf("%s: getBlockAttributes: %d:%d:%s =\n%s",TAG,projectId,resourceId,blockId,json);
		
		@SuppressWarnings("unchecked")
		Hashtable<String,Hashtable<String,String>> attributeTable = (Hashtable<String,Hashtable<String,String>>)jsonToJava.jsonToTable(json);
		Hashtable<String,Hashtable<String,String>> results = PropertiesUpdateHandler.getInstance().getBlockAttributes(projectId,resourceId,blockId,attributeTable);
		log.debug(TAG+"created table\n"+results);
		String gson =  javaToJson.tableToJson(results);
		log.trace(TAG+"JSON="+gson);
		return gson;
	}
	
	@Override
	public String getConnectionAttributes(long projectId, long resourceId,String connectionId,String json) {
		log.debugf("%s: getConnectionAttributes: %d:%d:%s =\n%s",TAG,projectId,resourceId,connectionId,json);
		
		@SuppressWarnings("unchecked")
		Hashtable<String,Hashtable<String,String>> attributeTable = (Hashtable<String,Hashtable<String,String>>)jsonToJava.jsonToTable(json);
		Hashtable<String,Hashtable<String,String>> results = PropertiesUpdateHandler.getInstance().getConnectionAttributes(projectId,resourceId,connectionId,attributeTable);
		log.debug(TAG+"created table\n"+results);
		String gson =  javaToJson.tableToJson(results);
		log.trace(TAG+": JSON="+gson);
		return gson;
	}
	

	@Override
	public String getPaletteBlockAttributes() {
		return null;
	}

	@Override
	public String getPaletteConnectionAttributes() {
		// TODO Auto-generated method stub
		return null;
	}
	
}
