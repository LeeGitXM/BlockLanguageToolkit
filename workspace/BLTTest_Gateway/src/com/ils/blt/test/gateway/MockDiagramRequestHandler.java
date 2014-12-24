/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.test.gateway;

import java.util.Date;
import java.util.UUID;

import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.gateway.ControllerRequestHandler;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.TagReader;
import com.ils.blt.gateway.engine.TagWriter;
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
	private final ControllerRequestHandler requestHandler;
	private final BlockExecutionController controller;
	private final TagReader tagReader;
	private final TagWriter tagWriter;
	
	/**
	 * Initialize with a Gateway context.
	 */
	public MockDiagramRequestHandler(GatewayContext cntx) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.controller = BlockExecutionController.getInstance();
		this.context = cntx;
		this.requestHandler = ControllerRequestHandler.getInstance();
		this.tagWriter = new TagWriter();
		this.tagReader = new TagReader();
		tagWriter.initialize(context);
		tagReader.initialize(context);
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
		ProcessBlock uut = requestHandler.createInstance(blockClass, mock.getSelf(), UUID.randomUUID());
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
			if( !type.equalsIgnoreCase("SIGNAL") )  {
				propertyType = PropertyType.valueOf(type.toUpperCase());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.addMockOutput: Unrecognized property type %s (%s)", TAG,type,iae.getLocalizedMessage());
		}
		MockOutputBlock output = new MockOutputBlock(diagramId,tagPath,propertyType,port);
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null) mock.addBlock(output);	
	}
	@Override
	public void clearOutput(UUID diagram,String port) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagram);
		if( mock!=null ) {
			MockOutputBlock block = mock.getOutputForPort(port);
			if( block!=null ) {
				block.clearValue();
			}
			else {
				log.warnf("%s.clearOutput: Unknown output port %s", TAG,port);
			}
		}
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
	 * Get the current value of the named property in the block-under-test.
	 * 
	 * @param diagramId
	 * @param propertyName
	 */
	@Override
	public Object getTestBlockPropertyValue(UUID diagramId,String propertyName){
		Object result = null;
		log.infof("%s.getTestBlockPropertyValue: %s %s",TAG,diagramId.toString(), propertyName);
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			ProcessBlock uut = mock.getBlockUnderTest();
			if( uut!=null ) {
				BlockProperty prop = uut.getProperty(propertyName);
				if( prop!=null) result = prop.getValue();
				else result = "ERROR: Property "+propertyName+" not found";
			}
			else{
				result = "ERROR: No block under test";
			}	
		}
		else{
			result = "ERROR: diagram not found";
		}
		log.infof("%s.getTestBlockPropertyValue: %s = %s", TAG,propertyName,result);
		return result;
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
	public QualifiedValue readTag(String tagPath) {
		return tagReader.readTag(tagPath);
	}
	
	@Override
	public QualifiedValue readValue(UUID diagramId, String port) {
		QualifiedValue qv = new BasicQualifiedValue("none");
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		MockOutputBlock block = null;
		if( mock!=null) block = mock.getOutputForPort(port);
		if( block!=null ) {
			qv = block.getValue();
			log.infof("%s.readValue: output block value %s", TAG,qv.toString());
		}
		else {
			log.warnf("%s.readValue: Unknown output port %s", TAG,port);
		}
		return qv;
	}

	@Override
	public void reset(UUID diagramId) {
		log.warnf("%s.reset: diagram %s", TAG,diagramId.toString());
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
						uut.getBlockId().toString(),propertyName,property.getValue(),value);
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
	 * Change binding parameters on a property. Note that this does NOT fire a change event.
	 * @param diagramId
	 * @param propertyName
	 * @param type BindingType, NONE, TAG_READ, TAG_READWRITE, TAG_MONITOR or TAG_WRITE
	 * @param binding
	 */
	@Override
	public void setTestBlockPropertyBinding(UUID diagramId,String propertyName, String type, String binding) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null) {
			ProcessBlock uut = mock.getBlockUnderTest();
			BlockProperty property = uut.getProperty(propertyName);
			if( property!=null ) {
				try {
					BindingType bt = BindingType.valueOf(type.toUpperCase());
					property.setBindingType(bt);
					property.setBinding(binding);
					property.setValue("");    // Notifies the change listeners (namely the block itself)
				}
				catch( IllegalArgumentException iae ) {
					log.infof("%s.setTestBlockPropertyBinding: diagram %s:%s, bad binding type (%s)",TAG,diagramId.toString(),propertyName,iae.getLocalizedMessage());
				}
			}
			else {
				log.infof("%s.setTestBlockPropertyBinding: diagram %s, unable to find property %s ",TAG,diagramId.toString(),propertyName);
			}
		}
		else {
			log.infof("%s.setTestBlockPropertyBinding: unable to find diagram %s ",TAG,diagramId.toString());
		}
	}
	
	/**
	 * Start the execution engine, then start the test diagram. 
	 * Note that we have not actually added the diagram to the controller.
	 * @param diagramId
	 */
	public void startMockDiagram(UUID diagramId){
		log.infof("%s.startMockDiagram: %s ",TAG,diagramId.toString());
		MockDiagram mock = (MockDiagram)(controller.getDiagram(diagramId));
		if( mock!=null ) {
			controller.start(context);
			mock.analyze();  // Analyze connections
			for(ProcessBlock block:mock.getProcessBlocks()) {
				block.start();
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
				block.stop();
				for(BlockProperty prop:block.getProperties()) {
					controller.removeSubscription(block, prop);
				}
			}
			controller.stop();
			controller.clearSubscriptions();
		}
	}
	@Override
	public void updateBlockAnchor(UUID diagramId,String port) {
		tagWriter.updateTag(projectId.longValue(),tagPath, qv);
	}
	@Override
	public void updateTag(Long projectId,String tagPath,QualifiedValue qv) {
		tagWriter.updateTag(projectId.longValue(),tagPath, qv);
	}
	/**
	 * Transmit a signal with the specified command to the block-under-test.
	 *   
	 * @param diagramId
	 * @param command
	 */
	@Override
	public long writeCommand(UUID diagramId,String command,String arg,String msg) {
		MockDiagram mock = (MockDiagram)controller.getDiagram(diagramId);
		if( mock!=null ) {
			ProcessBlock uut = mock.getBlockUnderTest();
			Signal sig= new Signal(command,arg,msg);
			SignalNotification snote = new SignalNotification(uut,sig);
			uut.acceptValue(snote);
		}
		return (new Date()).getTime();
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
		log.infof("%s.writeValue: %s:%s %s ",TAG,diagramId.toString(),port,value);
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
