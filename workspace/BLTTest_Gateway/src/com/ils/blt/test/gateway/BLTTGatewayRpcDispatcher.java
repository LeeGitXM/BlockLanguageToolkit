/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.test.gateway;

import java.util.UUID;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.test.common.MockDiagramScriptingInterface;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 *  The RPC Dispatcher is the point of entry for incoming RCP requests.
 *  Its purpose is simply to parse out a request and send it to the
 *  right handler. This class supports the aggregate of RPC interfaces.
 */
public class BLTTGatewayRpcDispatcher implements MockDiagramScriptingInterface{
	private static String TAG = "BLTTGatewayRpcDispatcher";
	private final LoggerEx log;
	private final GatewayContext context;
	private final BlockExecutionController controller;
	private final MockDiagramRequestHandler requestHandler;
	
	/**
	 * Constructor. On instantiation, the dispatcher creates instances
	 * of all required handlers.
	 */
	public BLTTGatewayRpcDispatcher(GatewayContext cntx,MockDiagramRequestHandler rh ) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = cntx;
		this.controller = BlockExecutionController.getInstance();
		this.requestHandler = rh;
	}

	//=============================== Methods in the MockDiagramScriptingInterface ===================================
	/**
	 * Create, but do not activate, a mock diagram.
	 * @return the Id of the diagram
	 */
	public UUID createMockDiagram(String blockClass,String project) {
		return requestHandler.createMockDiagram(blockClass,project);
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
	public void clearOutput(UUID diagramId, String port) {
		requestHandler.clearOutput(diagramId, port);
		
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
	@Override
	public Object getTestBlockPropertyValue(UUID diagramId,String propertyName){ 
		log.infof("%s.getTestBlockPropertyValue: %s %s",TAG,diagramId.toString(),propertyName);
		Object result = null;
		if( requestHandler!=null ) {
			result = requestHandler.getTestBlockPropertyValue(diagramId,propertyName);
		}
		return result;
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
	@Override
	public QualifiedValue readTag(String tagPath) {
		return requestHandler.readTag(tagPath);
	}
	/**
	 * Read the latest value from the output block with the named port.
	 * No reading is signified by an empty string.
	 */
	@Override
	public QualifiedValue readValue(UUID diagramId, String port) {
		return requestHandler.readValue(diagramId, port);
	}
	@Override
	public void reset(UUID diagram) {
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
	 * Update the connection type of a block anchor
	 * @param diagramId
	 * @param port name
	 * @param type of the connection to be attached to this port
	 */
	public void updateBlockAnchor(UUID diagramId,String port,String type) {
		requestHandler.updateBlockAnchor(diagramId,port,type);
	}
	/**
	 * Transmit a signal with the specified command to the block-under-test.
	 *   
	 * @param diagram
	 * @param command
	 */
	@Override
	public long writeCommand(UUID diagram,String command,String arg,String msg) {
		return requestHandler.writeCommand(diagram,command,arg,msg);
	}
	/**
	 * Direct a MockInput block to transmit a value to the block-under-test.
	 */
	@Override
	public long writeValue(UUID diagramId, String port, Integer index, String value,String quality) {
		log.infof("%s.writeValue: %s %s is %s", TAG,diagramId.toString(),port,value);
		long timestamp = 0;
		if( index!=null ) {
			timestamp = requestHandler.writeValue(diagramId,port,index.intValue(),value,quality);
		}
		return timestamp;
	}

	@Override
	public void updateTag(Long projectId,String tagPath,QualifiedValue qv) {
		requestHandler.updateTag(projectId.longValue(),tagPath, qv);
	}
}
