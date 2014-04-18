/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.test.gateway;

import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.common.PropertyType;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.gateway.BlockPropertiesHandler;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.blt.test.common.MockDiagramScriptingInterface;
import com.ils.blt.test.gateway.mock.MockDiagram;
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
	
	/**
	 * Constructor. On instantiation, the dispatcher creates instances
	 * of all required handlers.
	 */
	public BLTTGatewayRpcDispatcher(GatewayContext cntx ) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = cntx;
		this.controller = BlockExecutionController.getInstance();
	}
	
	/**
	 * Create, but do not activate, a mock diagram.
	 * @return the Id of the diagram
	 */
	@Override
	public UUID createTestHarness(String blockClass) {
		SerializableDiagram origin = new SerializableDiagram();
		origin.setId(UUID.randomUUID());
		origin.setName("Mock:"+blockClass);
		MockDiagram mock = new MockDiagram(origin,null);  // No parent
		// Instantiate a block from the class
		ProcessBlock uut = BlockPropertiesHandler.getInstance().createInstance(blockClass, mock.getSelf(), UUID.randomUUID());
		if( uut==null) {
			uut = ProxyHandler.getInstance().createBlockInstance(blockClass, mock.getSelf(), UUID.randomUUID());
		}
		if( uut!=null ) {
			mock.addBlock(uut);
			controller.addTemporaryDiagram(mock);
		}
		else {
			log.warnf("%s.createTestHarness: Failed to create block of class %s",TAG,blockClass);
		}	
		return mock.getSelf();
	}
	/**
	 * Add an input block to the mock diagram. Connect it to the block-under-test's 
	 * input port of the specified name.
	 */
	@Override
	public void addMockInput(UUID harness, String tagPath, PropertyType dt, String port) {
		MockInputBlock input = new MockInputBlock(harness,path,dt,port);
		
	}
	@Override
	public void addMockOutput(UUID harness, String tagPath, PropertyType dt, String port) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void deleteTestHarness(UUID harness) {
		stopTestHarness(harness);
		controller.removeDiagram(harness);
		
	}
	@Override
	public QualifiedValue readValue(UUID harness, String port) {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public void setProperty(UUID harness, String propertyName, Object value) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void setValue(UUID harness, String port, QualifiedValue value) {
		// TODO Auto-generated method stub
		
	}
	/**
	 * Analyze connections in the diagram, then activate subscriptions.
	 */
	@Override
	public void startTestHarness(UUID harness) {
		// TODO Auto-generated method stub
		
	}
	/**
	 * Deactivate all subscriptions within the mock diagram.
	 */
	@Override
	public void stopTestHarness(UUID harness) {
		// TODO Auto-generated method stub
		
	}

	

}
