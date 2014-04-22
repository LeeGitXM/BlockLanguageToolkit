/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.test.gateway.mock;

import java.util.UUID;

import com.ils.block.AbstractProcessBlock;
import com.ils.block.ProcessBlock;
import com.ils.block.common.AnchorDirection;
import com.ils.block.common.AnchorPrototype;
import com.ils.block.common.BindingType;
import com.ils.block.common.BlockConstants;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.PropertyType;
import com.ils.block.control.BlockPropertyChangeEvent;
import com.ils.block.control.OutgoingNotification;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;


/**
 * This block is strictly for use with a MockDiagram to provide
 * inputs that can be subscribed to tags.
 */
public class MockInputBlock extends AbstractProcessBlock implements ProcessBlock {
	private static String BLOCK_PROPERTY_INPUT = "Input";
	private final String portName;
	private final PropertyType propertyType;
	private final String tagPath;
	
	public MockInputBlock(UUID parent,String tag,PropertyType pt,String port) {
		super(BlockExecutionController.getInstance(),parent,UUID.randomUUID());
		this.portName = port;
		this.propertyType = pt;
		this.tagPath = tag;
		initialize();
	}
	
	public PropertyType getPropertyType() { return propertyType; }
	public String getPort() { return portName; }
	
	/**
	 * If the input property changes, then place the new value on the output.
	 * This is the normal path when a subscribed tag changes value.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		if( event.getPropertyName().equals(BLOCK_PROPERTY_INPUT)) {
			OutgoingNotification nvn = new OutgoingNotification(this,portName,event.getNewValue());
			controller.acceptCompletionNotification(nvn);
		}
	}
	
	/**
	 * Pass a value directly on the output. This callable directly.
	 */
	public void setValue(QualifiedValue qv) {
		OutgoingNotification nvn = new OutgoingNotification(this,portName,qv);
		controller.acceptCompletionNotification(nvn);
	}
	
	/**
	 * Add the tag property and link it to the value property.
	 */
	private void initialize() {
		setLabel("MockInput");
		BlockProperty value = new BlockProperty(BLOCK_PROPERTY_INPUT,null,propertyType,true);
		value.setBinding(tagPath);
		value.setBindingType(BindingType.TAG);
		properties.put(BLOCK_PROPERTY_INPUT, value);
		
		// Define a single output
		ConnectionType ctype = ConnectionType.connectionTypeForPropertyType(propertyType);
		AnchorPrototype output = new AnchorPrototype(portName,AnchorDirection.OUTGOING,ctype);
		anchors.add(output);
	}
}
