/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
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
 * Multiply the input by a constant.
 */
@ExecutableBlock
public class Gain extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "Gain";
	public static final String BLOCK_PROPERTY_GAIN = "Gain";
	private double gain = 1.0;
	
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
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Gain(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
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
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert gain value to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * A new value has appeared on an input anchor. Add it to the list and trigger the delay timer.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		if( !isLocked() ) {
			QualifiedValue qv = vcn.getValue();
			if( qv!=null && qv.getValue()!=null && qv.getQuality().isGood()) {
				try {
					Double dbl = Double.parseDouble(qv.getValue().toString());
					if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(BlockConstants.IN_PORT_NAME)) {
						double value = dbl.doubleValue();
						value = value*gain;
						qv = new BasicQualifiedValue(new Double(value),qv.getQuality(),qv.getTimestamp());
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
						controller.acceptCompletionNotification(nvn);
						notifyOfStatus(qv);
					}
					else {
						log.warnf("%s.acceptValue: Unexpected port designation (%s)",TAG,vcn.getConnection().getDownstreamPortName());
					}
				}
				catch(NumberFormatException nfe) {
					log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",TAG,nfe.getLocalizedMessage());
				}
			}
			else {
				if( qv!=null ) {
					qv = new BasicQualifiedValue(new Double(Double.NaN),qv.getQuality(),qv.getTimestamp());
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(qv);
				}
			}
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Gain");
		BlockProperty constant = new BlockProperty(BLOCK_PROPERTY_GAIN,new Double(gain),PropertyType.DOUBLE,true);
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
		desc.setEmbeddedFontSize(36);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}