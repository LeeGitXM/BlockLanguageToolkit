/**
 *   (c) 2014-2020  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
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
 * Multiply the input by a constant.
 */
@ExecutableBlock
public class Gain extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "Gain";
	public static final String BLOCK_PROPERTY_GAIN = "Gain";
	private double gain = 1.0;
	private QualifiedValue lastInput = new BasicQualifiedValue(Double.NaN);
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Gain() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "Multiplier".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent resource Id identifying the parent of this block (a diagram)
	 * @param block universally unique Id for the block
	 */
	public Gain(ExecutionController ec,ProjectResourceId parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Accept notification that a value has arrived on an input
	 * @param vcn incoming notification
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		String port = vcn.getConnection().getDownstreamPortName();
		
		if( port.equalsIgnoreCase(BlockConstants.IN_PORT_NAME) ) {
			QualifiedValue qv = vcn.getValue();
			if( qv!=null && qv.getValue()!=null && qv.getQuality().isGood()) {
				lastInput = qv;
				evaluate();
			}
			else {
				if( qv!=null ) {
					lastInput = new BasicQualifiedValue(Double.NaN,qv.getQuality(),qv.getTimestamp());
					evaluate();
				}
			}	
		}
	}

	/**
	 * Place the result on the output.
	 */
	@Override
	public void evaluate() {
		if( !isLocked() ) {
			try {
				double dbl = fcns.coerceToDouble(lastInput.getValue().toString());
				if( !Double.isNaN(dbl))  {
					lastValue = new BasicQualifiedValue(dbl*gain,lastInput.getQuality(),lastInput.getTimestamp());
				}
				else {
					lastValue = new BasicQualifiedValue(Double.NaN,lastInput.getQuality(),lastInput.getTimestamp());
				}
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(lastValue);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}

	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(lastValue);
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Handle a change to the delay interval or buffer size
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if( propertyName.equals(BLOCK_PROPERTY_GAIN) ) {
			try {
				gain = Double.parseDouble(event.getNewValue().toString());
				evaluate();
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert gain value to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		BlockProperty constant = new BlockProperty(BLOCK_PROPERTY_GAIN,gain,PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_GAIN, constant);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setIsMultiple(false);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/gain.png");
		prototype.setPaletteLabel("Gain");
		prototype.setTooltipText("Multiply the incoming value by a specified constant");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ARITHMETIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedLabel("K");
		desc.setEmbeddedFontSize(28);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}