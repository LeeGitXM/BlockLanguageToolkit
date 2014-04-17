/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.test.gateway;

import java.io.IOException;
import java.util.Hashtable;
import java.util.List;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.gateway.engine.BlockExecutionController;
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

	/**
	 * Constructor. There is a separate dispatcher for each project.
	 */
	public GatewayRpcDispatcher(GatewayContext cntx) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = cntx;

	}

	public String getControllerState() {
		return BlockExecutionController.getExecutionState();
	}
	
	public void startController() {
		BlockExecutionController.getInstance().start(context);
	}
	
	public void stopController() {
		BlockExecutionController.getInstance().stop();
	}

	public void enableDiagram(Long projectId, Long resourceId, Boolean flag) {
		// TODO Auto-generated method stub
		
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
		log.debugf("%s: getConnectionAttributes: %d:%d:%s =\n%s",TAG,projectId,resourceId,connectionId,json);
		
		ObjectMapper mapper = new ObjectMapper();
		Hashtable<String, Hashtable<String, String>> attributeTable;
		try {
			attributeTable = mapper.readValue(json, new TypeReference<Hashtable<String,Hashtable<String,String>>>(){});
			Hashtable<String,Hashtable<String,String>> results = MockBlockPropertiesHandler.getInstance().getConnectionAttributes(projectId,resourceId,connectionId,attributeTable);
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


	
	public List<String> getDiagramTreePaths(String projectName) {
		List<String> results = BlockExecutionController.getInstance().getDiagramTreePaths(projectName);
		return results;
	}

}
