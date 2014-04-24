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
import com.ils.block.common.BlockProperty;
import com.ils.block.common.PropertyType;
import com.ils.block.control.IncomingNotification;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;


/**
 * This block is strictly for use with a MockDiagram to provide
 * a spot to collect and store output values.
 */
public class MockOutputBlock extends AbstractProcessBlock implements ProcessBlock {
	private static String BLOCK_PROPERTY_OUTPUT = "Output";
	private final String portName;
	private final PropertyType propertyType;
	private final String tagPath;
	private QualifiedValue value = null;
	
	public MockOutputBlock(UUID parent,String tag,PropertyType pt,String port) {
		super(BlockExecutionController.getInstance(),parent,UUID.randomUUID());
		this.portName = port;
		this.propertyType = pt;
		this.tagPath = tag;
		initialize();
	}
	
	public PropertyType getPropertyType() { return propertyType; }
	public String getPort() { return portName; }
	
	/**
	 * Add the tag property and link it to the value property.
	 */
	private void initialize() {
		setLabel("MockOutput");
		BlockProperty value = new BlockProperty(BLOCK_PROPERTY_OUTPUT,null,propertyType,true);
		value.setBinding(tagPath);
		value.setBindingType(BindingType.TAG);
		properties.put(BLOCK_PROPERTY_OUTPUT, value);
		
		// Define a single input. Accept any data type.
		ConnectionType ctype = ConnectionType.ANY;
		AnchorPrototype input = new AnchorPrototype(portName,AnchorDirection.INCOMING,ctype);
		anchors.add(input);
	}
	/**
	 * @return the latest value received by this block.
	 */
	public QualifiedValue getValue() { return value; }
	
	/**
	 * The block is notified that a new value has appeared on one of its input anchors.
	 * Save the value as a local variable.
	 * @param vcn notification of the new value.
	 */
	@Override
	public void setValue(IncomingNotification vcn) {
		super.setValue(vcn);
		QualifiedValue qv = null;
		Object val = vcn.getValue();
		if( val instanceof QualifiedValue ) {
			this.value = vcn.getValueAsQualifiedValue();
		}
	}
}
