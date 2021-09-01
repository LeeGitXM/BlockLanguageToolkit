/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.text.SimpleDateFormat;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

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
	 * @param parent resource Id identifying the parent of this block (a diagram)
	 * @param block universally unique Id for the block
	 */
	public TimeReadout(ExecutionController ec,ProjectResourceId parent,UUID block) {
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
		input.setIsMultiple(true);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(output);
		
		type = PropertyType.DATE;
	}
	
	/**
	 * Handle a change to the format. The format must be a legal SimpleDateFormat.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_FORMAT)) {
			String format = event.getNewValue().toString();
			log.debugf("%s.propertyChange: New display format is (%s).",getName(),format);
			customFormatter = new SimpleDateFormat(format);
			type = PropertyType.DATE;
		}
		log.debugf("TIMEREADOUT: %s property change %s = %s",getName(),propertyName,event.getNewValue().toString());
	}
	
	/**
	 * A new value has appeared on the input. Post a notification, then pass it on. If the value is, itself,
	 * a date, then display it
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		lastValue = incoming.getValue();
		if( lastValue!=null && lastValue.getValue()!=null ) {
			if( !isLocked()  ) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, lastValue);
				try {
					
					String value = "";
					// Convert the value according to the data type specified by the format.
					if( lastValue.getValue() instanceof java.util.Date ) {
						value = customFormatter.format((java.util.Date)(lastValue.getValue()));
					}
					else {
						value = customFormatter.format(lastValue.getTimestamp());
					}
			
					valueProperty.setValue(value);
					log.tracef("%s.acceptValue: port %s formatted value =  %s.",getName(),incoming.getConnection().getUpstreamPortName(),value);
					
					QualifiedValue qv = new BasicQualifiedValue(valueProperty.getValue(),lastValue.getQuality(),lastValue.getTimestamp()); 
//					log.tracef("%s.acceptValue: port %s formatted value =  %s.",getName(),incoming.getConnection().getUpstreamPortName(),value);

					controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
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
		prototype.setTooltipText("Show the time the most recent value. Use SimpleDateFormat to configure the output.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_MISC);
		
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.READOUT);
		view.setPreferredHeight(40);
		view.setPreferredWidth(150);    // 20 chars
		view.setCtypeEditable(true);
	}
}