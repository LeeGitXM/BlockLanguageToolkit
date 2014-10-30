/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Present a digital readout of the last value that passed through.
 */
@ExecutableBlock
public class Readout extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Readout";
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
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Readout");
		// Define the display format
		BlockProperty fmt = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FORMAT,format,PropertyType.STRING,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_FORMAT, fmt);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		properties.put(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input -- but allow multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}

	/**
	 * Handle a change to the format.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_FORMAT)) {
			format = event.getNewValue().toString();
			log.infof("%s.propertyChange: New display format is (%s).",TAG,format);
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
				log.warnf("%s.propertyChange: Did not recognize format (%s), using (%s).",TAG,format,"%s");
				type=PropertyType.STRING;
				format = "%s";
			}
		}
	}
	
	/**
	 * A new value has appeared on the input. Post a notification, then pass it on.
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		this.state = BlockState.ACTIVE;
		QualifiedValue qv = incoming.getValue();
		if( qv!=null && qv.getValue()!=null ) {
			valueProperty.setValue(qv.getValue());
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
					log.warn(TAG+".acceptValue: error formatting "+qv.getValue()+" with "+format+" as "+type.name(),ex);  // Print stack trace
				}
				qv = new BasicQualifiedValue(value,qv.getQuality(),qv.getTimestamp()); 
				log.debugf("%s.acceptValue: port %s formatted value =  %s.",TAG,incoming.getConnection().getUpstreamPortName(),value);
				controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE, qv);
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
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.READOUT);
		view.setPreferredHeight(40);
		view.setPreferredWidth(100);    // 15 chars
		view.setCtypeEditable(true);
	}
}