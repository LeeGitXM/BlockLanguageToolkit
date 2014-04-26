/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.test.gateway;

import java.util.UUID;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.control.BlockPropertyChangeEvent;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.test.gateway.mock.MockDiagram;
import com.ils.blt.test.gateway.mock.MockInputBlock;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 *  This handler provides is a common class for handling requests dealing with mock diagrams.
 *  The requests can be expected arrive both through the scripting interface
 *  and the RPC dispatcher.  Handle those requests which are more than simple passthrus 
 *  to the BlockExecutionController
 *  
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class MockDiagramRequestHandler   {
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
	 * Set the property for a block through the change listener interface. 
	 * Do this synchronously.
	 * 
	 * @param diagramId
	 * @param propertyName
	 * @param value
	 */
	public void setTestBlockProperty(UUID diagramId, String propertyName, String value) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null) {
			ProcessBlock uut = mock.getBlockUnderTest();
			BlockProperty property = uut.getProperty(propertyName);
			BlockPropertyChangeEvent event = new BlockPropertyChangeEvent(
					uut.getBlockId().toString(),propertyName,
					new BasicQualifiedValue(property.getValue()),
					new BasicQualifiedValue(value));
			uut.propertyChange(event);
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
	public void writeValue(UUID diagramId,String port,int index,String value,String quality) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			MockInputBlock block = mock.getInputForPort(port,index);
			if( block!=null ) {
				block.writeValue(value,quality);
			}
		}
	}
}
