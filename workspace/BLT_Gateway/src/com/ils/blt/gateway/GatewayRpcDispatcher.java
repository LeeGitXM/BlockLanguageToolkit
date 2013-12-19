/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.gateway;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import com.ils.block.ProcessBlock;
import com.ils.block.annotation.ExecutableBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.PalettePrototype;
import com.ils.common.ClassList;
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
public class GatewayRpcDispatcher   {
	private static String TAG = "GatewayRpcDispatcher";
	private final LoggerEx log;
	private final GatewayContext context;
	private final JsonToJava jsonToJava;
	private final JavaToJson javaToJson;

	/**
	 * Constructor. There is a separate dispatcher for each project.
	 */
	public GatewayRpcDispatcher(GatewayContext cntx) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = cntx;
		this.jsonToJava = new JsonToJava();
		this.javaToJson = new JavaToJson();
	}



	public void enableDiagram(Long projectId, Long resourceId, Boolean flag) {
		// TODO Auto-generated method stub
		
	}


	public String getBlockAttributes(Long proj, Long res,String blockId,String json) {
		long projectId = proj.longValue();
		long resourceId = res.longValue();
		log.debugf("%s: getBlockAttributes: %d:%d:%s =\n%s",TAG,projectId,resourceId,blockId,json);
		
		@SuppressWarnings("unchecked")
		Hashtable<String,BlockProperty> attributeTable = (Hashtable<String,BlockProperty>)jsonToJava.jsonToTable(json);
		Hashtable<String,BlockProperty> results = PropertiesUpdateHandler.getInstance().getBlockAttributes(projectId,resourceId,blockId,attributeTable);
		log.debug(TAG+"created table\n"+results);
		String gson =  javaToJson.tableToJson(results);
		log.trace(TAG+"JSON="+gson);
		return gson;
	}
	

	public String getConnectionAttributes(Long proj, Long res,String connectionId,String json) {
		long projectId = proj.longValue();
		long resourceId = res.longValue();
		log.debugf("%s: getConnectionAttributes: %d:%d:%s =\n%s",TAG,projectId,resourceId,connectionId,json);
		
		@SuppressWarnings("unchecked")
		Hashtable<String,Hashtable<String,String>> attributeTable = (Hashtable<String,Hashtable<String,String>>)jsonToJava.jsonToTable(json);
		Hashtable<String,Hashtable<String,String>> results = PropertiesUpdateHandler.getInstance().getConnectionAttributes(projectId,resourceId,connectionId,attributeTable);
		log.debugf("%s: created table = %s",TAG,results);
		String gson =  javaToJson.tableToJson(results);
		log.trace(TAG+": JSON="+gson);
		return gson;
	}


	// The blocks are expected to reside in a jar named "block-definition.jar"
	public List<String> getBlockPrototypes() {
		log.infof("%s: getBlockPrototypes ...",TAG);
		List<String> results = new ArrayList<String>();
		ClassList cl = new ClassList();
		List<Class<?>> classes = cl.getAnnotatedClasses("block-definition", ExecutableBlock.class);
		for( Class<?> cls:classes) {
			log.info("   found block class: "+cls.getName());
			try {
				Object obj = cls.newInstance();
				if( obj instanceof ProcessBlock ) {
					PalettePrototype bp = ((ProcessBlock)obj).getBlockPrototype();
					String json = bp.toJson();
					log.info("   json: "+json);
					results.add(json);
				}
				else {
					log.warnf("%s: Class %s not a ProcessBlock",TAG,cls.getName());
				}
			} 
			catch (InstantiationException ie) {
				log.warnf("%s:getBlockPrototypes: Exception instantiating block (%s)",TAG,ie.getLocalizedMessage());
			} 
			catch (IllegalAccessException iae) {
				log.warnf("%s:getBlockPrototypes: Access exception (%s)",TAG,iae.getMessage());
			}
			catch (Exception ex) {
				log.warnf("%s: getBlockPrototypes: Runtime exception (%s)",TAG,ex.getMessage(),ex);
			}
		}
		return results;
	}
	


	
}
