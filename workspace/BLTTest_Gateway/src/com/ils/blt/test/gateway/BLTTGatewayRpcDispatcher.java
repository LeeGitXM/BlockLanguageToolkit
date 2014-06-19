/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.test.gateway;

import java.util.Date;
import java.util.UUID;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.test.common.MockDiagramScriptingInterface;
import com.ils.blt.test.common.TagProviderScriptingInterface;
import com.ils.blt.test.gateway.tag.ProviderRegistry;
import com.ils.blt.test.gateway.tag.TagHandler;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 *  The RPC Dispatcher is the point of entry for incoming RCP requests.
 *  Its purpose is simply to parse out a request and send it to the
 *  right handler. This class supports the aggregate of RPC interfaces.
 */
public class BLTTGatewayRpcDispatcher implements MockDiagramScriptingInterface,
                                                 TagProviderScriptingInterface {
	private static String TAG = "BLTTGatewayRpcDispatcher";
	private final LoggerEx log;
	private final GatewayContext context;
	private final BlockExecutionController controller;
	private final MockDiagramRequestHandler requestHandler;
	private final TagHandler tagFactory;

	/**
	 * Constructor. On instantiation, the dispatcher creates instances
	 * of all required handlers.
	 */
	public BLTTGatewayRpcDispatcher(GatewayContext cntx,MockDiagramRequestHandler rh ) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = cntx;
		this.controller = BlockExecutionController.getInstance();
		this.requestHandler = rh;
		this.tagFactory = new TagHandler(context);
	}

	//=============================== Methods in the MockDiagramScriptingInterface ===================================
	/**
	 * Create, but do not activate, a mock diagram.
	 * @return the Id of the diagram
	 */
	@Override
	public UUID createMockDiagram(String blockClass) {
		return requestHandler.createMockDiagram(blockClass);
	}
	/**
	 * Add an input block to the mock diagram. Connect it to the block-under-test's 
	 * input port of the specified name.
	 */
	@Override
	public void addMockInput(UUID diagramId, String tagPath, String type, String port) {
		requestHandler.addMockInput(diagramId, tagPath, type, port);
	}
	@Override
	public void addMockOutput(UUID diagramId, String tagPath, String type, String port) {
		requestHandler.addMockOutput(diagramId, tagPath, type, port);
	}
	
	@Override
	public void deleteMockDiagram(UUID diagram) {
		stopMockDiagram(diagram);
		controller.removeTemporaryDiagram(diagram);

	}
	@Override
	public void forcePost(UUID diagramId, String port, String value) {
		log.infof("%s.forcePost: %s %s->%s",TAG,diagramId.toString(),port,value);
		try {
			requestHandler.forcePost(diagramId,port,value);
		}
		catch(Exception ex) {
			log.info(TAG+".forcePost: Exception ("+ex.getMessage()+")",ex);
		}
	}
	/**
	 * Return the execution state of the block under test.
	 * @param diagram
	 * @return the state of the block under test.
	 */
	public String getState(UUID diagramId) {
		log.infof("%s.getState: %s",TAG,diagramId.toString());
		String state = "";
		if( requestHandler!=null ) {
			state = requestHandler.getState(diagramId);
		}
		return state;
	}
	@Override
	public boolean isLocked(UUID diagram) {
		return requestHandler.isLocked(diagram);
	}
	/**
	 * Read the latest value from the output block with the named port.
	 * No reading is signified by an empty string.
	 */
	@Override
	public QualifiedValue readValue(UUID diagramId, String port) {
		log.infof("%s.readValue: %s on %s", TAG,diagramId.toString(),port);
		return requestHandler.readValue(diagramId, port);
	}
	@Override
	public void reset(UUID diagram) {
		log.infof("%s.reset", TAG);
		requestHandler.reset(diagram);
	}

	@Override
	public void setLocked(UUID diagramId, Boolean flag) {
		requestHandler.setLocked(diagramId,flag.booleanValue());
		
	}
	@Override
	public void setTestBlockProperty(UUID diagramId, String propertyName, String value) {
		log.infof("%s.setTestBlockProperty: %s %s is %s", TAG,diagramId.toString(),propertyName,value.toString());
		try {
			requestHandler.setTestBlockProperty(diagramId,propertyName,value);
		}
		catch(Exception ex) {
			log.info(TAG+".setTestBlockProperty: Exception ("+ex.getMessage()+")",ex);
		}
	}
	
	@Override
	public void setTestBlockPropertyBinding(UUID diagramId,String propertyName, String type, String binding) {
		log.infof("%s.setTestBlockPropertyBinding: %s.%s is %s:%s", TAG,diagramId.toString(),propertyName,type,binding);
		try {
			requestHandler.setTestBlockPropertyBinding(diagramId,propertyName,type,binding);
		}
		catch(Exception ex) {
			log.info(TAG+".setTestBlockPropertyBinding: Exception ("+ex.getMessage()+")",ex);
		}
	}

	/**
	 * Analyze connections in the diagram, then activate subscriptions.
	 */
	@Override
	public void startMockDiagram(UUID diagramId) {
		requestHandler.startMockDiagram(diagramId);
	}
	/**
	 * Deactivate all subscriptions within the mock diagram.
	 */
	@Override
	public void stopMockDiagram(UUID diagramId) {
		requestHandler.stopMockDiagram(diagramId);
	}

	/**
	 * Direct a MockInput block to transmit a value to the block-under-test.
	 */
	@Override
	public long writeValue(UUID diagramId, String port, Integer index, String value,String quality) {
		long timestamp = 0;
		if( index!=null ) {
			timestamp = requestHandler.writeValue(diagramId,port,index.intValue(),value,quality);
		}
		return timestamp;
	}
	//=============================== Methods in the TagProviderScriptingInterface ===================================
	@Override
	public void createProvider(String name) {
		log.debug(TAG+"createProvider: "+name);
		ProviderRegistry.getInstance().createProvider(context, name);
	}
	@Override
	public void createExpression(String provider, String tagPath, String dataType, String expr) {
		log.debug(TAG+"createExpression: ["+provider+"]"+tagPath+" = "+expr);
		tagFactory.createExpression(provider, tagPath, dataType, expr);
	}
	@Override
	public void createTag(String provider, String tagPath, String dataType) {
		log.debug(TAG+"createTag: ["+provider+"]"+tagPath);
		tagFactory.createTag(provider, tagPath, dataType);
	}

	@Override
	public void deleteTag(String provider, String tagPath) {
		tagFactory.deleteTag(provider, tagPath);
	}

	@Override
	public void removeProvider(String name) {
		ProviderRegistry.getInstance().removeProvider(name);
	}
	@Override
	public void updateExpression(String provider, String tagPath, String expr) {
		log.debug(TAG+"updateExpression: ["+provider+"]"+tagPath+" = "+expr);
		tagFactory.updateExpression(provider, tagPath, expr);
	}
	@Override
	public void updateTag(String provider, String tagPath, String value, Date timestamp) {
		tagFactory.updateTag(provider, tagPath, value, timestamp);
	}
}
