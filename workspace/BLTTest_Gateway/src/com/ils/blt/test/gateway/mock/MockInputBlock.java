/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.test.gateway.mock;

import java.util.Date;
import java.util.UUID;

import com.ils.block.AbstractProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataQuality;


/**
 * This block is strictly for use with a MockDiagram to provide
 * inputs that can be subscribed to tags.
 */
public class MockInputBlock extends AbstractProcessBlock implements ProcessBlock {
	private final static String TAG = "MockInputBlock";
	private final String portName;
	private final PropertyType propertyType;
	
	public MockInputBlock(UUID parent,String tag,PropertyType pt,String port) {
		super(BlockExecutionController.getInstance(),parent,UUID.randomUUID());
		this.portName = port;
		this.propertyType = pt;
		initialize();
	}
	
	public PropertyType getPropertyType() { return propertyType; }
	public String getPort() { return portName; }
	
	
	/**
	 * Pass a value directly on the output. This callable directly. Convert 
	 * arguments to proper type for the outgoing connection.
	 * @param value as a string. The block will convert to the correct datatype.
	 * @param quality as a string. Good is 'good'.
	 * @return the timestamp ~msec
	 */
	public long writeValue(String value,String quality) {
		// Convert the string to a proper data type
		Object obj = value;
		Date now = new Date();
		if( propertyType.equals(PropertyType.BOOLEAN)) {
			obj = TruthValue.UNKNOWN;
			try {
				obj = TruthValue.valueOf(value.toUpperCase());
			}
			catch(IllegalArgumentException iae) {
				log.infof("%s.writeValue: Unknown truth value %s (%s)",TAG,value,iae.getLocalizedMessage());
			}
		}
		Quality q = DataQuality.OPC_BAD_DATA;
		if(  "good".equalsIgnoreCase(quality)) q = DataQuality.GOOD_DATA;
		QualifiedValue qv  = new BasicQualifiedValue(obj,q,now);
		OutgoingNotification nvn = new OutgoingNotification(this,portName,qv);
		controller.acceptCompletionNotification(nvn);
		return now.getTime();
	}
	
	/**
	 * Add the tag property and link it to the value property.
	 */
	private void initialize() {
		setName("MockInput");
	
		// Define a single output
		ConnectionType ctype = ConnectionType.connectionTypeForPropertyType(propertyType);
		AnchorPrototype output = new AnchorPrototype(portName,AnchorDirection.OUTGOING,ctype);
		anchors.add(output);
	}
}
