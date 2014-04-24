/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.test.gateway;

import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.common.PropertyType;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.gateway.BlockRequestHandler;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.blt.test.common.MockDiagramScriptingInterface;
import com.ils.blt.test.gateway.mock.MockDiagram;
import com.ils.blt.test.gateway.mock.MockInputBlock;
import com.ils.blt.test.gateway.mock.MockOutputBlock;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 *  The RPC Dispatcher is the point of entry for incoming RCP requests.
 *  Its purpose is simply to parse out a request and send it to the
 *  right handler. This class supports the aggregate of RPC interfaces.
 */
public class BLTTGatewayRpcDispatcher implements MockDiagramScriptingInterface  {
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
	
	/**
	 * Create, but do not activate, a mock diagram.
	 * @return the Id of the diagram
	 */
	@Override
	public UUID createMockDiagram(String blockClass) {
		SerializableDiagram origin = new SerializableDiagram();
		origin.setId(UUID.randomUUID());
		origin.setName("Mock:"+blockClass);
		MockDiagram mock = new MockDiagram(origin,null);  // No parent
		// Instantiate a block from the class
		ProcessBlock uut = BlockRequestHandler.getInstance().createInstance(blockClass, mock.getSelf(), UUID.randomUUID());
		if( uut==null) {
			uut = ProxyHandler.getInstance().createBlockInstance(blockClass, mock.getSelf(), UUID.randomUUID());
		}
		if( uut!=null ) {
			mock.addBlock(uut);
			this.controller.addTemporaryDiagram(mock);
		}
		else {
			log.warnf("%s.createMockDiagram: Failed to create block of class %s",TAG,blockClass);
		}	
		return mock.getSelf();
	}
	/**
	 * Add an input block to the mock diagram. Connect it to the block-under-test's 
	 * input port of the specified name.
	 */
	@Override
	public void addMockInput(UUID diagramId, String tagPath, String type, String port) {
		PropertyType propertyType = PropertyType.OBJECT;   // Unknown
		try {
			propertyType = PropertyType.valueOf(type.toUpperCase());
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.addMockInput: Unrecognized property type %s (%s)", TAG,type,iae.getLocalizedMessage());
		}
		MockInputBlock input = new MockInputBlock(diagramId,tagPath,propertyType,port);
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null) mock.addBlock(input);
	}
	@Override
	public void addMockOutput(UUID diagramId, String tagPath, String type, String port) {
		PropertyType propertyType = PropertyType.OBJECT;   // Unknown
		try {
			propertyType = PropertyType.valueOf(type.toUpperCase());
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.addMockOutput: Unrecognized property type %s (%s)", TAG,type,iae.getLocalizedMessage());
		}
		MockOutputBlock output = new MockOutputBlock(diagramId,tagPath,propertyType,port);
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null) mock.addBlock(output);
	}
	@Override
	public void deleteMockDiagram(UUID diagram) {
		stopMockDiagram(diagram);
		controller.removeTemporaryDiagram(diagram);
		
	}
	
	/**
	 * Read the latest value from the output block with the named port.
	 * No reading is signified by an empty string.
	 */
	@Override
	public QualifiedValue readValue(UUID diagramId, String port) {
		log.infof("%s.readValue: %s on %s", TAG,diagramId.toString(),port);
		QualifiedValue qv = new BasicQualifiedValue("none");
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		MockOutputBlock block = null;
		if( mock!=null) block = mock.getOutputForPort(port);
		if( block!=null ) {
			qv = block.getValue();
			log.infof("%s.readValue: block value %s", TAG,qv.toString());
		}
		else {
			log.warnf("%s.readValue: Unknown output port %s", TAG,port);
		}
		return qv;
	}
	@Override
	public void setTestBlockProperty(UUID diagramId, String propertyName, String value) {
		requestHandler.setTestBlockProperty(diagramId,propertyName,value);
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
	public void writeValue(UUID diagramId, String port, Integer index, String value,String quality) {
		if( index!=null ) {
			requestHandler.writeValue(diagramId,port,index.intValue(),value,quality);
		}
	}
}
