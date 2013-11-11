/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.diagnostics.gateway;

import java.util.Hashtable;

import org.python.core.PyDictionary;
import org.python.core.PyList;

import com.ils.common.JavaToJson;
import com.ils.common.JsonToJava;
import com.ils.diagnostics.common.PropertiesRequestInterface;
import com.ils.diagnostics.common.StatusReportingInterface;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 *  The RPC Dispatcher is the point of entry for incoming RCP requests.
 *  Its purpose is simply to parse out a request and send it to the
 *  right handler. This class supports the aggregate of RPC interfaces.
 */
public class GatewayRpcDispatcher implements PropertiesRequestInterface,StatusReportingInterface  {
	private static String TAG = "GatewayRpcDispatcher: ";
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
	public void reportStatus(String key, PyList pyAttributes) throws Exception {
		log.debug(TAG+"reportStatus: key=\""+key+"\"\n"+pyAttributes);
		
	}

	@Override
	public void enableDiagram(String path, boolean flag) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public PyDictionary getRepository() {
		return ClassRepository.getInstance().getRepository();
	}



	@Override
	public String getBlockAttributes(String key,String json) {
		key = augmentKey(key);
		log.debug(TAG+"getBlockAttributes: key=\""+key+"\"\n"+json);
		
		Hashtable<String,?> attributeTable = jsonToJava.jsonToTable(json);
		Hashtable<String,?> results = PropertiesUpdateHandler.getInstance().getBlockAttributes(key,attributeTable);
		log.debug(TAG+"created table\n"+results);
		String gson =  javaToJson.tableToJson(results);
		log.trace(TAG+"JSON="+gson);
		return gson;
	}
	
	@Override
	public String getConnectionAttributes(String key,String json) {
		key = augmentKey(key);
		log.debug(TAG+"getConnectionAttributes: key=\""+key+"\"\n"+json);
		
		Hashtable<String,?> attributeTable = jsonToJava.jsonToTable(json);
		Hashtable<String,?> results = PropertiesUpdateHandler.getInstance().getConnectionAttributes(key,attributeTable);
		log.debug(TAG+"created table\n"+results);
		String gson =  javaToJson.tableToJson(results);
		log.trace(TAG+"JSON="+gson);
		return gson;
	}
	
	/**
	 * Make the key universally unique by 
	 * @param key
	 * @return
	 */
	private String augmentKey(String key) {
		return String.format("%s:%s", projectId.toString(),key);
	}

	


	
}
