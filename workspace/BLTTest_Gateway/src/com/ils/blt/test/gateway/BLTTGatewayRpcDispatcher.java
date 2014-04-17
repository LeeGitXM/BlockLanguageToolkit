/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.test.gateway;

import java.util.UUID;

import com.ils.block.common.PropertyType;
import com.ils.blt.common.serializable.SerializableDiagram;
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
	
	/**
	 * Constructor. On instantiation, the dispatcher creates instances
	 * of all required handlers.
	 */
	public BLTTGatewayRpcDispatcher(GatewayContext cntx ) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = cntx;
	}
	
	@Override
	public UUID createTestHarness(String projectName, String blockClass) {
		SerializableDiagram origin = new SerializableDiagram();
		origin.setId(UUID.randomUUID());
		origin.setName("Mock:"+blockClass);
		MockDiagram mock = new MockDiagram(origin,null);  // No parent
		return null;
	}
	@Override
	public void addMockInput(UUID harness, PropertyType dt, String port) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void addMockOutput(UUID harness, PropertyType dt, String port) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void deleteTestHarness(UUID harness) {
		// TODO Auto-generated method stub
		
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
	@Override
	public void startTestHarness(UUID harness) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void stopTestHarness(UUID harness) {
		// TODO Auto-generated method stub
		
	}

	

}
