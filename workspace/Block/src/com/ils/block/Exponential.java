/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import org.apache.commons.math3.analysis.function.Exp;

import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.annotation.ExecutableBlock;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class emits the e to the power of its input. There is no synchronization required.
 *  Input and output are data values.
 */
@ExecutableBlock
public class Exponential extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Exponential";
	protected double value = Double.NaN;
	private final Exp exp;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Exponential() {
		initialize();
		initializePrototype();
		exp = new Exp();
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Exponential(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		exp = new Exp();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("Exponential");

		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	

	/**
	 * A new value has appeared on our input. Manipulate it and send it on its way.
	 * Retain the timestamp.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		if( !isLocked() ) {
			QualifiedValue qv = vcn.getValue();
			if( qv!=null && qv.getValue()!=null && qv.getQuality().isGood()) {
				try {
					Double dbl = Double.parseDouble(qv.getValue().toString());
					if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(BlockConstants.IN_PORT_NAME)) {
						value = dbl.doubleValue();
						value = exp.value(value);
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
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/exp.png");
		prototype.setPaletteLabel("Exp");
		prototype.setTooltipText("Raise e to the input value and place result on the output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ARITHMETIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/ex.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}