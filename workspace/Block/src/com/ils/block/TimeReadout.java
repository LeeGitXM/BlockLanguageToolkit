/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.text.SimpleDateFormat;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Present a digital readout of the last value that passed through.
 */
@ExecutableBlock
public class TimeReadout extends Readout implements ProcessBlock {
	private SimpleDateFormat customFormatter = new SimpleDateFormat(DEFAULT_FORMAT);
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public TimeReadout() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. There should be a custom property called format.
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public TimeReadout(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("TimeReadout");
		// Define the display format
		BlockProperty fmt = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FORMAT,DEFAULT_FORMAT,PropertyType.STRING,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_FORMAT, fmt);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input -- but allow multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(output);
	}
	
	/**
	 * Handle a change to the format. We deduce data type from the format.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_FORMAT)) {
			String format = event.getNewValue().toString();
			log.debugf("%s.propertyChange: New display format is (%s).",getName(),format);
			customFormatter = new SimpleDateFormat(format);
		}
		log.debugf("TIMEREADOUT: %s property change %s = %s",getName(),propertyName,event.getNewValue().toString());
	}
	
	/**
	 * A new value has appeared on the input. Post a notification, then pass it on.
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		QualifiedValue qv = incoming.getValue();
		if( qv!=null && qv.getValue()!=null ) {
			if( !isLocked()  ) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
				controller.acceptCompletionNotification(nvn);
				// Convert the value according to the data type specified by the format.
				try {
					String value = customFormatter.format(qv.getTimestamp());
					valueProperty.setValue(value);
					log.tracef("%s.acceptValue: port %s formatted value =  %s.",getName(),incoming.getConnection().getUpstreamPortName(),value);
					
					qv = new BasicQualifiedValue(value,qv.getQuality(),qv.getTimestamp()); 
					valueProperty.setValue(value);
					log.tracef("%s.acceptValue: port %s formatted value =  %s.",getName(),incoming.getConnection().getUpstreamPortName(),value);
					notifyOfStatus(qv);
				}
				catch(Exception ex) {
					log.warn(getName()+".acceptValue: error formatting timestamp",ex);  // Print stack trace
				} 
			}
		}
	}
	
	/**
	 * Define the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/time_readout.png");
		prototype.setPaletteLabel("TimeReadout");
		prototype.setTooltipText("Show time of most recent value. Use SimpleDateFormat to configure the output.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_MISC);
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.READOUT);
		view.setPreferredHeight(40);
		view.setPreferredWidth(150);    // 20 chars
		view.setCtypeEditable(true);
	}
}