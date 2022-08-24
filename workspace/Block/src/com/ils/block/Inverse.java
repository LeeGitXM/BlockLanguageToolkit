/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualityCode;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * This class emits the inverse of its input. There is no synchronization required.
 *  Input and output are data values.
 */
@ExecutableBlock
public class Inverse extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Inverse";
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Inverse() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent resource Id identifying the parent of this block (a diagram)
	 * @param block universally unique Id for the block
	 */
	public Inverse(ExecutionController ec,ProjectResourceId parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
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
			if( qv!=null && qv.getValue()!=null ) {
				try {
					Double dbl = Double.parseDouble(qv.getValue().toString());
					double value = dbl.doubleValue();
					if( value!=0.0) {
						value = 1/value;
						lastValue = new BasicQualifiedValue(value,qv.getQuality(),qv.getTimestamp());
						statusText = "";
					}
					else {
						statusText = "Value is equal to zero";
						qv = new BasicQualifiedValue(Double.POSITIVE_INFINITY,QualityCode.Bad,qv.getTimestamp());
					}
				}
				catch(NumberFormatException nfe) {
					log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",TAG,nfe.getLocalizedMessage());
					lastValue = new BasicQualifiedValue(Double.NaN,QualityCode.Bad,qv.getTimestamp());
				}
			}
			else {
				lastValue = new BasicQualifiedValue(Double.NaN,QualityCode.Bad,qv.getTimestamp());
			}
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
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
		prototype.setPaletteIconPath("Block/icons/palette/inverse.png");
		prototype.setPaletteLabel("Inverse");
		prototype.setTooltipText("Divide one by the value and place result on the output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ARITHMETIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/one_over_x.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}