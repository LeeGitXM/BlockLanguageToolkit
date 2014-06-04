/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.test.gateway;

import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.BlockState;
import com.ils.block.common.PropertyType;
import com.ils.block.control.BlockPropertyChangeEvent;
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
 *  This handler provides is a common class for handling requests dealing with mock diagrams.
 *  The requests can be expected to arrive both through the scripting interface
 *  and the RPC dispatcher.  Handle those requests which are more than simple passthrus 
 *  to the BlockExecutionController
 *  
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class MockDiagramRequestHandler implements MockDiagramScriptingInterface  {
	private final static String TAG = "MockDiagramRequestHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private final BlockExecutionController controller;
	
	/**
	 * Initialize with a Gateway context.
	 */
	public MockDiagramRequestHandler(GatewayContext cntx) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.controller = BlockExecutionController.getInstance();
		this.context = cntx;
	}
	/**
	 * Create, but do not activate, a mock diagram.
	 * @param blockClass, the fully qualified class of the block under test
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
	public void addMockInput(UUID diagramId, String tagPath, String type,String port) {
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
	public void addMockOutput(UUID diagramId, String tagPath,String type, String port) {
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
		controller.removeTemporaryDiagram(diagram);
	}

	@Override
	public void forcePost(UUID diagramId, String port, String value) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			ProcessBlock uut = mock.getBlockUnderTest();
			uut.forcePost(port, value);
		}
	}
	/**
	 * Return the execution state of the block under test.
	 * @param diagram
	 * @return the state of the block under test.
	 */
	@Override
	public String getState(UUID diagramId) {
		BlockState state = BlockState.INITIALIZED;
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			ProcessBlock uut = mock.getBlockUnderTest();
			state = uut.getState();
		}
		return state.name();
	}

	@Override
	public boolean isLocked(UUID diagramId) {
		boolean result = false;
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			ProcessBlock uut = mock.getBlockUnderTest();
			result = uut.isLocked();
		}
		return result;
	}

	@Override
	public QualifiedValue readValue(UUID diagramId, String port) {
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
	public void reset(UUID diagramId) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			ProcessBlock uut = mock.getBlockUnderTest();
			uut.reset();
		}
	}

	@Override
	public void setLocked(UUID diagramId, Boolean flag) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			ProcessBlock uut = mock.getBlockUnderTest();
			uut.setLocked(flag.booleanValue());
		}
	}
	
	/**
	 * Set the property for a block through the change listener interface. 
	 * Do this synchronously.
	 * 
	 * @param diagramId
	 * @param propertyName
	 * @param value
	 */
	@Override
	public void setTestBlockProperty(UUID diagramId, String propertyName, String value) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null) {
			ProcessBlock uut = mock.getBlockUnderTest();
			BlockProperty property = uut.getProperty(propertyName);
			if( property!=null ) {
				BlockPropertyChangeEvent event = new BlockPropertyChangeEvent(
						uut.getBlockId().toString(),propertyName,
						new BasicQualifiedValue(property.getValue()),
						new BasicQualifiedValue(value));
				uut.propertyChange(event);
			}
			else {
				log.infof("%s.setTestBlockProperty: diagram %s, unable to find property %s ",TAG,diagramId.toString(),propertyName);
			}
		}
		else {
			log.infof("%s.setTestBlockProperty: unable to find diagram %s ",TAG,diagramId.toString());
		}
	}
	
	/**
	 * Start the execution engine, then start the test diagram. 
	 * @param diagramId
	 */
	public void startMockDiagram(UUID diagramId){
		log.infof("%s.startMockDiagram: %s ",TAG,diagramId.toString());
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			controller.start(context);
			mock.analyze();  // Analyze connections
			for(ProcessBlock block:mock.getProcessBlocks()) {
				for(BlockProperty prop:block.getProperties()) {
					controller.startSubscription(block, prop);
				}
			}
		}
	}
	
	/**
	 * Stop all property updates and input receipt by canceling all active
	 * subscriptions involving the diagramId. Stop the controller.
	 * @param diagramId unique Id
	 */
	public void stopMockDiagram(UUID diagramId) {
		log.infof("%s.stopMockDiagram: %s ",TAG,diagramId.toString());
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			for(ProcessBlock block:mock.getProcessBlocks()) {
				for(BlockProperty prop:block.getProperties()) {
					controller.removeSubscription(block, prop);
				}
			}
			controller.stop();
		}
	}
	
	/**
	 * Direct a MockInput block to transmit a value to the block-under-test. 
	 *  
	 * @param diagramId
	 * @param index of the connection into the named port. The index is zero-based.
	 * @param port
	 * @param value
	 * @param quality
	 */
	@Override
	public long writeValue(UUID diagramId,String port,Integer index,String value,String quality) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		long timestamp = -1;
		if( mock!=null ) {
			MockInputBlock block = mock.getInputForPort(port,index.intValue());
			if( block!=null ) {
				timestamp = block.writeValue(value,quality);
			}
		}
		return timestamp;
	}
}
