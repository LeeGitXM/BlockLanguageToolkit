/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.blt.common.UtilityFunctions;
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
import com.ils.common.annotation.ExecutableBlock;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Present a digital readout of the last value that passed through.
 */
@ExecutableBlock
public class Readout extends AbstractProcessBlock implements ProcessBlock {
	private final UtilityFunctions fncs;
	private String format = "%s";
	private PropertyType type = PropertyType.STRING;
	private BlockProperty valueProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Readout() {
		this.fncs = new UtilityFunctions();
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
	public Readout(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		this.fncs = new UtilityFunctions();
		initialize();
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
		
		// Define a single input -- but allow multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	/**
	 * Send status update notification for our last output value.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new BasicQualifiedValue(valueProperty.getValue());
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		log.debugf("%s.notifyOfStatus (%s)", getName(), qv.getValue().toString());
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
			else {
				log.warnf("%s.propertyChange: Did not recognize format (%s), using (%s).",getName(),format,"%s");
				type=PropertyType.STRING;
				format = "%s";
			}
		}
		log.debugf("READOUT: %s property change %s = %s",getName(),propertyName,event.getNewValue().toString());
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
				String value = "";
				try {
					if( type==PropertyType.DOUBLE) {
						value = String.format(format, fncs.coerceToDouble(qv.getValue()));
					}
					else if( type==PropertyType.INTEGER) {
						value = String.format(format, fncs.coerceToInteger(qv.getValue()));
					}
					else {
						value = String.format(format,fncs.coerceToString(qv.getValue()));
					}
				}
				catch(Exception ex) {
					log.warn(getName()+".acceptValue: error formatting "+qv.getValue()+" with "+format+" as "+type.name(),ex);  // Print stack trace
				}
				qv = new BasicQualifiedValue(value,qv.getQuality(),qv.getTimestamp()); 
				valueProperty.setValue(value);
				log.tracef("%s.acceptValue: port %s formatted value =  %s.",getName(),incoming.getConnection().getUpstreamPortName(),value);
				notifyOfStatus(qv);
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