/**
 *   (c) 2014-2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 *  When the block is reset, then the block emits a configured truth-value,
 *  waits for the interval to expire and emits the opposite.
 *
 */
@ExecutableBlock
public class TruthValuePulse extends AbstractProcessBlock implements ProcessBlock {
	private static String TAG = "TruthValuePulse";
	private final static String BLOCK_PROPERTY_PULSE_VALUE= "PulseValue";
	private static final double DEFAULT_INTERVAL = 60.;  // ~ seconds
	private double interval = DEFAULT_INTERVAL; // One minute pulse by default
	private TruthValue pulse = TruthValue.TRUE;
	private final Watchdog dog;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public TruthValuePulse() {
		initialize();
		initializePrototype();
		dog = new Watchdog(getName(),this);
	}
	
	/**

	 * Constructor.
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public TruthValuePulse(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
	}

	/**
	 * Do not call the base-class reset() as this sets outgoing
	 * connection states to UNKNOWN. This block allows only true/false
	 */
	@Override
	public void reset() {
		recordActivity(Activity.ACTIVITY_RESET,"");
		pulse();
	}

	/**
	 * Start any active monitoring or processing within the block.
	 * This default method does nothing. In general, a start does
	 * NOT reset state in a block that is already running.
	 */
	@Override
	public void start() {
		super.start();
		pulse();
	}
	/**
	 * Disconnect from the timer thread.
	 */
	@Override
	public void stop() {
		super.stop();
		timer.removeWatchdog(dog);
	}
	
	private void pulse() {
		setState(pulse);
		if( !isLocked()  ) {
			lastValue = new TestAwareQualifiedValue(timer,state);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
		}
		dog.setSecondsDelay(interval);
		timer.updateWatchdog(dog);
	}
	/**
	 * Define the interval and trigger properties and ports.
	 */
	private void initialize() {	
		setName("TruthValuePulse");
		delayStart = false;
		
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL,new Double(interval),PropertyType.TIME_MINUTES,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL, intervalProperty);
		BlockProperty pulseProperty = new BlockProperty(BLOCK_PROPERTY_PULSE_VALUE,pulse,PropertyType.BOOLEAN,true);
		setProperty(BLOCK_PROPERTY_PULSE_VALUE, pulseProperty);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	

	/**
	 * The interval has expired. Propagate the inverse of the PULSE value.
	 * NOTE: The diagram reset explicitly calls evaluate() on blocks that
	 *       are marked as "delayStart". 
	 */
	@Override
	public void evaluate() {
		if( !isLocked() ) {
			if( pulse.equals(TruthValue.TRUE)) {
				setState(TruthValue.FALSE);
			}
			else {
				setState(TruthValue.TRUE);
			}
			lastValue = new TestAwareQualifiedValue(timer,state);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
		}
	}
	
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		if( dog.isActive() ) {
			Map<String,String> attributes = descriptor.getAttributes();
			long now = System.nanoTime()/1000000;   // Work in milliseconds
			long waitTime = (long)(dog.getExpiration()-now);
			attributes.put("MSecsToStateChange",String.valueOf(waitTime));
		}
		return descriptor;
	}
	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.debugf("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_INTERVAL)) {
			try {
				interval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BLOCK_PROPERTY_PULSE_VALUE)) {
			try {
				TruthValue tv = TruthValue.valueOf(event.getNewValue().toString().toUpperCase());
				// Only allow TRUE/FALSE
				if( tv.equals(TruthValue.TRUE) || tv.equals(TruthValue.FALSE)) {
					pulse = tv;
				}
				else {
					log.warnf("%s.propertyChange: Trigger must TRUE or FALSE, was %s",TAG,event.getNewValue().toString());
				}
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s.propertyChange: Trigger must be a TruthValue (%s)",TAG,iae.getMessage());
			}
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}

	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(new TestAwareQualifiedValue(timer,state.name()));	
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}

	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/true_pulse.png");
		prototype.setPaletteLabel("TruthPulse");
		prototype.setTooltipText("On reset, emit a truth-value for a configured interval");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setEmbeddedIcon("Block/icons/embedded/signal_pulse.png");
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.SQUARE);
		view.setPreferredHeight(60);
		view.setPreferredWidth(60);
//		view.setReceiveEnabled(true);
	}
}