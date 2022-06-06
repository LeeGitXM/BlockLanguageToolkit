/**
 *   (c) 2014-2018  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Date;
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
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * Present a digital readout of the time of last value that passed through.
 */
@ExecutableBlock
public class Readout extends AbstractProcessBlock implements ProcessBlock {
	private final static boolean DEBUG = false;
	private String format = "%4.4f";
	protected PropertyType type = PropertyType.DOUBLE;
	protected BlockProperty valueProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Readout() {
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
	public Readout(ExecutionController ec,ProjectResourceId parent,UUID block) {
		super(ec,parent,block);
		initialize();
		log.tracef("Initializing a readout named %s", getName());
	}
	/**
	 * On a reset, clear the display.
	 */
	@Override
	public void reset() {
		super.reset();
		valueProperty.setValue("");
		notifyOfStatus();
	}
	/**
	 * Clear the display on start of the block.
	 */
	@Override
	public void start() { 
		super.start();
		valueProperty.setValue("");
		notifyOfStatus();
	}
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Readout");
		// Define the display format
		BlockProperty fmt = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FORMAT,format,PropertyType.STRING,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_FORMAT, fmt);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input 
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(output);
	}
	/**
	 * Send status update notification for our last output value.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,valueProperty.getValue());
		notifyOfStatus(qv);
		
	}
	protected void notifyOfStatus(QualifiedValue qv) {
		if(DEBUG) log.infof("%s.notifyOfStatus %s %s =%s", getName(), getBlockId().toString(),BlockConstants.BLOCK_PROPERTY_VALUE,qv.getValue().toString());
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Handle a change to the format. We deduce data type from the format.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_FORMAT)) {
			format = event.getNewValue().toString();
			log.debugf("%s.propertyChange: New display format is (%s).",getName(),format);
			// Validate the format for a data type
			if( format.matches(".*%[0-9]*[.]?[0-9]*s.*") ) {
				type=PropertyType.STRING;
			}
			else if( format.matches(".*%[0-9]*d.*") ) {
				type=PropertyType.INTEGER;
			}
			else if( format.matches(".*%[0-9]*[.]?[0-9]*f.*") ) {
				type=PropertyType.DOUBLE;
			}
			// Sub-classes should override to a property type different from
			// STRING, INTEGER, DOUBLE
			else {
				type=PropertyType.STRING;
				format = "%s";
			}
		}
		if(DEBUG) log.infof("%s.propertyChange: %s = %s",getName(),propertyName,event.getNewValue().toString());
	}
	
	/**
	 * A new value has appeared on the input. Post a notification, then pass it on.
	 * Ignore types that are not explicitly handled by this class.
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		if( type.equals(PropertyType.BOOLEAN) || type.equals(PropertyType.DOUBLE) ||
				type.equals(PropertyType.INTEGER) || type.equals(PropertyType.STRING)     ) {
			lastValue = incoming.getValue();
			if( lastValue!=null && lastValue.getValue()!=null ) {
				if( !isLocked()  ) {		
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
					controller.acceptCompletionNotification(nvn);
					// Convert the value according to the data type specified by the format.
					String value = "";
					if( lastValue.getValue().toString().length()>0) {
						try {
							if( type==PropertyType.DOUBLE) {
								log.tracef("...formatting a double for %s...", getName());
								value = String.format(format, fcns.coerceToDouble(lastValue.getValue()));
							}
							else if( type==PropertyType.INTEGER) {
								log.tracef("...formatting an integer for %s...", getName());
								value = String.format(format, fcns.coerceToInteger(lastValue.getValue()));
							}
							else if(lastValue.getValue() instanceof Date) {
								log.tracef("...formatting a date for %s...", getName());
								value = dateFormatter.format(lastValue.getValue());
							}
							else {
								log.tracef("...formatting a value of an unknown type for %s...", getName());
								value = String.format(format,fcns.coerceToString(lastValue.getValue()));
							}
						}
						catch(Exception ex) {
							log.warn(getName()+".acceptValue: error formatting "+lastValue.getValue()+" with "+format+" as "+type.name(),ex);  // Print stack trace
						}
					}
					updateStateForNewValue(lastValue);
					QualifiedValue qv = new BasicQualifiedValue(value,lastValue.getQuality(),lastValue.getTimestamp()); 
					valueProperty.setValue(value);
					if(DEBUG) log.infof("%s.acceptValue: port %s formatted value =  %s.",getName(),incoming.getConnection().getUpstreamPortName(),value);
					notifyOfStatus(qv);
				}
			}
		}
	}
	

	
	/**
	 * Define the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/readout.png");
		prototype.setPaletteLabel("Readout");
		prototype.setTooltipText("Show current connection value. Sample formats: %s (string), %3.2f (float), %d (integer)");
		prototype.setTabName(BlockConstants.PALETTE_TAB_MISC);
		
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.READOUT);
		view.setPreferredHeight(40);
		view.setPreferredWidth(100);    // 15 chars
		view.setCtypeEditable(true);
	}
}