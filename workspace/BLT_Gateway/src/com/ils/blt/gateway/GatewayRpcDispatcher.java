/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.gateway;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.block.ProcessBlock;
import com.ils.block.annotation.ExecutableBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.PalettePrototype;
import com.ils.block.common.TransmissionScope;
import com.ils.block.control.BroadcastNotification;
import com.ils.block.control.Signal;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.common.ClassList;
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
	 * Query the specified block for its properties. If the block does not exist, create it, given the
	 * specified class name. In the case of a new block, its diagram may also need to be created. 
	 * 
	 * @param projectId
	 * @param resourceId
	 * @param blockId
	 * @param className
	 * @return properties for the block
	 */
	public List<String> getBlockProperties(Long projectId,Long resourceId,String blockId,String className) {
		log.infof("%s: getBlockProperties: %d:%d %s",TAG,projectId.longValue(),resourceId.longValue(),blockId);
		
		BlockProperty[] propertyArray = DiagramPropertiesHandler.getInstance().
					getBlockProperties(projectId,resourceId,UUID.fromString(blockId),className);
		List<String> result = null;
		if( propertyArray!=null ) {
			result = new ArrayList<String>();
			for( BlockProperty prop:propertyArray ) {
				// Python can return some nulls in the array
				if( prop!=null ) {
					result.add(prop.toJson());
				}
			}			
		}
		else {
			log.infof("%s: getBlockProperties: created new block %d:%d %s",TAG,projectId.longValue(),resourceId.longValue(),className);
		}
		log.debugf("%s: getBlockProperties: returns %s",TAG,result.toString());
		return result;
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
			Hashtable<String,Hashtable<String,String>> results = DiagramPropertiesHandler.getInstance().getConnectionAttributes(projectId,resourceId,connectionId,attributeTable);
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


	/** The blocks implemented in Java are expected to reside in a jar named "block-definition.jar".
	 *  We add the blocks implemented in Python to this list. We consider only classes that are in
	 *  a "com/ils/block" package.
	 *  @return
	 */
	public List<String> getBlockPrototypes() {
		log.infof("%s: getBlockPrototypes ...",TAG);
		List<String> results = new ArrayList<String>();
		ClassList cl = new ClassList();
		List<Class<?>> classes = cl.getAnnotatedClasses(BLTProperties.BLOCK_JAR_NAME, ExecutableBlock.class,"com/ils/block/");
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
		// Now add prototypes from Python-defined blocks
		ProxyHandler phandler = ProxyHandler.getInstance();
		List<PalettePrototype> prototypes = phandler.getPalettePrototypes();
		for( PalettePrototype pp:prototypes) {
			results.add(pp.toJson());
		}
		log.debugf("%s: getBlockPrototypes: returning %d palette prototypes",TAG,results.size());
		return results;
	}
	
	void sendLocalSignal(String projectName, String diagramPath,String className, String command) {
		ProcessDiagram diagram = BlockExecutionController.getInstance().getDelegate().getDiagram(projectName, diagramPath);
		if( diagram!=null ) {
			// Create a broadcast notification
			Signal sig = new Signal(command,"","");
			sig.setClassName(className);
			BroadcastNotification broadcast = new BroadcastNotification(diagram.getProjectId(),diagram.getResourceId(),TransmissionScope.LOCAL,sig);
			BlockExecutionController.getInstance().acceptBroadcastNotification(broadcast);
		}
		else {
			log.warnf("%s.sendLocalSignal: Unable to find %s:%s for %s command to %s",TAG,projectName,diagramPath,command,className);
		}
	}

}
