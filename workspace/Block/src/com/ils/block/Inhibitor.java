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
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;

/**
 * On receipt of a trigger, this class inhibits further input from propagating.
 * Values that arrive during the inhibit period are discarded. Otherwise
 * values are propagated withput change.
 */
@ExecutableBlock
public class Inhibitor extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "Inhibitor";
	private double interval = 0.0;   // ~secs
	private long periodEndTime = 0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Inhibitor() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "filterConstant".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Inhibitor(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	
	/**
	 * A new value has appeared on an input anchor. If we are in an "inhibit" state, then 
	 * send this value to the bit-bucket.
	 * 
	 * Exponentially smooth values. The filter constant is the time-difference
     * between measurements divided by a time constant. The longer the time difference,
     * the more that we favor the current measurement. evaluation interval and time window
     * must have the same units.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		if( !isLocked() ) {
			String port = vcn.getConnection().getDownstreamPortName();
			if( port.equals(BlockConstants.IN_PORT_NAME)  ) {
				if( System.currentTimeMillis() > periodEndTime ) { 
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,vcn.getValue());
					controller.acceptCompletionNotification(nvn);
				}
				else {
					log.infof("%s.acceptValue: %s ignoring inhibited input ...",TAG,this.toString());
				}
			}
		}
		
	}
	
	/**
	 * We've received a transmitted signal. Deal with it, if appropriate.
	 * At a later time, we may implement pattern filtering or some other
	 * method to filter out unwanted messages. For now, if we recognize the command,
	 * then execute it.
	 * 
	 * @param sn signal notification.
	 */
	@Override
	public void acceptValue(SignalNotification sn) {
		Signal signal = sn.getSignal();
		if( signal.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_INHIBIT)) {
			periodEndTime = System.currentTimeMillis()+(long)(interval*1000);
			log.infof("%s.acceptValue: %s received inhibit command",TAG,this.toString());
		}
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Inhibitor");
		this.isReceiver = true;
		BlockProperty constant = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INHIBIT_INTERVAL,new Double(interval),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_INHIBIT_INTERVAL, constant);
		
		// Define a data input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * Handle a change to the interval value. Note that this does not currently
	 * change any inhibit in-progress.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		this.isReceiver = true;
		String propertyName = event.getPropertyName();
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_INHIBIT_INTERVAL) ) {
			try {
				interval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange Unable to convert interval value to an double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/inhibit.png");
		prototype.setPaletteLabel("Inhibitor");
		prototype.setTooltipText("Discard incoming values that arrive during an inhbit interval");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(80);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.CLAMP);
		desc.setReceiveEnabled(true);
	}
	
	@Override
	public void reset() {
		periodEndTime = 0;
	}
}