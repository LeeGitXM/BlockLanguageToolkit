/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import org.apache.commons.math3.analysis.function.Log;

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
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class emits the natural logrithm of its input. There is no synchronization required.
 *  Input and output are data values.
 */
@ExecutableBlock
public class NaturalLog extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "NaturalLog";
	private final Log ln;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public NaturalLog() {
		initialize();
		initializePrototype();
		ln = new Log();
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public NaturalLog(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		ln = new Log();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("ln");

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
					double valu = dbl.doubleValue();
					if( valu>0.0) {
						valu = ln.value(valu);
						qv = new BasicQualifiedValue(new Double(valu),qv.getQuality(),qv.getTimestamp());
						statusText = "";
					}
					else {
						statusText = "Value is less than or equal to zero";
						lastValue = new BasicQualifiedValue(new Double(Double.POSITIVE_INFINITY),new BasicQuality("<= zero",Quality.Level.Bad),qv.getTimestamp());
					}
				}
				catch(NumberFormatException nfe) {
					log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",TAG,nfe.getLocalizedMessage());
					lastValue = new BasicQualifiedValue(new Double(Double.NaN),new BasicQuality(nfe.getLocalizedMessage(),Quality.Level.Bad),qv.getTimestamp());
				}
			}
			else {
				lastValue = new BasicQualifiedValue(new Double(Double.NaN),new BasicQuality("null value",Quality.Level.Bad),qv.getTimestamp());
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
		prototype.setPaletteIconPath("Block/icons/palette/ln.png");
		prototype.setPaletteLabel("Ln");
		prototype.setTooltipText("Take the natural logrithm of the input and place result on the output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ARITHMETIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("ln");
		desc.setEmbeddedFontSize(36);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}