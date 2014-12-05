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
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Delay before passing the input onto the output.
 */
@ExecutableBlock
public class EdgeTrigger extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "EdgeTrigger";
	protected static String BLOCK_PROPERTY_INTERVAL = "HoldInterval";
	protected static String BLOCK_PROPERTY_TRIGGER  = "Trigger";

	private double holdInterval = 0.0;    // ~ secs (pulse)
	private TruthValue trigger = TruthValue.UNSET;
	protected QualifiedValue status = new BasicQualifiedValue(TruthValue.UNSET.name());
	private final Watchdog dog;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public EdgeTrigger() {
		dog = new Watchdog(TAG,this);
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom properties are "HoldInterval" and "Trigger".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public EdgeTrigger(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
	}
	
	@Override
	public void reset() {
		controller.removeWatchdog(dog);
	}
	/**
	 * Disconnect from the timer thread.
	 */
	@Override
	public void stop() {
		super.stop();
		controller.removeWatchdog(dog);
	}
	
	/**
	 * A new value has appeared on an input anchor. Send it on its way. Start timer to 
	 * propagate an inverse after the expiration time.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		String port = vcn.getConnection().getDownstreamPortName();
		if( port.equals(BlockConstants.IN_PORT_NAME) ) {

			QualifiedValue qv = vcn.getValue();
			// Ignore values arrriving during a hold period
			if( !dog.isActive() ) {
				if( !isLocked() ) {
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
					controller.acceptCompletionNotification(nvn);
				}

				if( trigger.equals(TruthValue.FALSE) || trigger.equals(TruthValue.TRUE) ) {
					TruthValue tv = vcn.getValueAsTruthValue();
					if( tv.equals(trigger)) {	
						dog.setSecondsDelay(holdInterval);
						controller.pet(dog);
					}
				}
			}
		}
	}
	
	/**
	 * The delay interval has expired. Propagate the inverse of the trigger.
	 * Set the timer for the next object, if any. 
	 */
	@Override
	public void evaluate() {
		log.infof("%s.evaluate trigger is (%s)",TAG,trigger.name());
		if( !isLocked() ) {
			if( trigger.equals(TruthValue.FALSE)) {
				status = new BasicQualifiedValue(TruthValue.TRUE.name());
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,status);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(status);
			}
			else if( trigger.equals(TruthValue.TRUE)) {
				status = new BasicQualifiedValue(TruthValue.FALSE.name());
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,status);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(status);
			}
		}	
	}
	
	/**
	 * Handle a change to the delay interval or buffer size
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if( propertyName.equals(BLOCK_PROPERTY_INTERVAL) ) {
			try {
				holdInterval = Double.parseDouble(event.getNewValue().toString());
				if( holdInterval<0 ) holdInterval = -holdInterval;
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert interval value to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BLOCK_PROPERTY_TRIGGER) ) {
			String val = event.getNewValue().toString();
			try {
				trigger = TruthValue.valueOf(val.toUpperCase());
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s: propertyChange Unable to convert %s to a truth value (%s)",TAG,val,iae.getLocalizedMessage());
			}
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(status);	
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("EdgeTrigger");
		BlockProperty constant = new BlockProperty(BLOCK_PROPERTY_INTERVAL,new Double(holdInterval),PropertyType.DOUBLE,true);
		properties.put(BLOCK_PROPERTY_INTERVAL, constant);
		BlockProperty trigProp = new BlockProperty(BLOCK_PROPERTY_TRIGGER,trigger.name(),PropertyType.STRING,true);
		properties.put(BLOCK_PROPERTY_TRIGGER, trigProp);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/edge_trigger.png");
		prototype.setPaletteLabel("EdgeTrigger");
		prototype.setTooltipText("Hold targeted value for a specified delay before reverting  (~secs)");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setEmbeddedIcon("Block/icons/embedded/edge_trigger.png");
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.SQUARE);
		view.setPreferredHeight(60);
		view.setPreferredWidth(60);
	}
	
}