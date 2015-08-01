/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Date;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.annotation.ExecutableBlock;
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
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * On receipt of a trigger, this class inhibits further input from propagating.
 * Values that arrive during the inhibit period are discarded. Otherwise
 * values are propagated without change.
 */
@ExecutableBlock
public class Inhibitor extends AbstractProcessBlock implements ProcessBlock {
	private BlockProperty expirationProperty = null;
	private double interval = 0.0;   // ~secs
	
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
	
	@Override
	public void reset() {
		super.reset();
		expirationProperty.setValue(0L);
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
				QualifiedValue qv = vcn.getValue();
				long time = ((Long)expirationProperty.getValue()).longValue();
				if( qv.getQuality().isGood() &&  
						(time==0 || qv.getTimestamp().getTime()>=time) ) {
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(vcn.getValue());
				}
				else {
					log.infof("%s.acceptValue: %s ignoring inhibited or BAD input ...",getName(),this.toString());
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
			long now = System.currentTimeMillis();
			expirationProperty.setValue(new Long(now+(long)(interval*1000)));
			log.infof("%s.acceptValue: %s received inhibit command (delay %f secs on %s)",getName(),this.toString(),interval,timer.getName());
		}
	}
	private boolean inhibits() { 
		boolean inhibiting = false;
		long time = ((Long)expirationProperty.getValue()).longValue();
		if( time>0) {
			long now = System.currentTimeMillis();
			if( now < time ) inhibiting = true;
		}
		return inhibiting; 
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		
		attributes.put("Inhibiting", (inhibits()?"true":"false"));
		long time = ((Long)expirationProperty.getValue()).longValue();
		if( time>0 ) {
			Date expiration = new Date(time);
			attributes.put("Inhibit Expiration",formatter.format(expiration) );
		}
		return descriptor;
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Inhibitor");
		this.isReceiver = true;
		BlockProperty constant = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INHIBIT_INTERVAL,new Double(interval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INHIBIT_INTERVAL, constant);
		expirationProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_EXPIRATION_TIME,new Long(0L),PropertyType.DATE,true);
		expirationProperty.setBindingType(BindingType.ENGINE);   // Is not editable outside this class
		setProperty(BlockConstants.BLOCK_PROPERTY_EXPIRATION_TIME, expirationProperty);
		
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
				log.warnf("%s.propertyChange Unable to convert interval value to an double (%s)",getName(),nfe.getLocalizedMessage());
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
}