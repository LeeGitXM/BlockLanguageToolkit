/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
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
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataQuality;

/**
 *  When a truth-value matching the trigger arrives, then the block emits a TRUE,
 *  waits for the interval to expire and emits a FALSE.
 * @author chuckc
 *
 */
@ExecutableBlock
public class TruthValuePulse extends AbstractProcessBlock implements ProcessBlock {
	private static String TAG = "TruthValuePulse";
	private static final double DEFAULT_INTERVAL = 60.;  // ~ seconds
	private double interval = DEFAULT_INTERVAL; // One minute pulse by default
	private TruthValue trigger = TruthValue.TRUE;
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
	 * connection states to UNKNOWN.
	 */
	@Override
	public void reset() {
		if( state.equals(TruthValue.TRUE) || 
			state.equals(TruthValue.FALSE)   ) {
			state = TruthValue.FALSE;
			QualifiedValue qv = new TestAwareQualifiedValue(timer,state.name());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(qv);
		}
		timer.removeWatchdog(dog);
	}
	/**
	 * Disconnect from the timer thread.
	 */
	@Override
	public void stop() {
		super.stop();
		timer.removeWatchdog(dog);
	}
	
	/**
	 * Define the interval and trigger properties and ports.
	 */
	private void initialize() {	
		setName("TruthValuePulse");
		
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL,new Double(interval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL, intervalProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER, triggerProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	
	
	
	/**
	 * A new value has appeared on an input anchor. Send it on its way
	 * if TRUE or FALSE. Otherwise do nothing.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();
		
		if( qv.getQuality().isGood() && !isLocked() && qv.getValue().toString().equalsIgnoreCase(trigger.name()))  {
			QualifiedValue result = new BasicQualifiedValue(TruthValue.TRUE,qv.getQuality(),qv.getTimestamp());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			setState(TruthValue.TRUE);
			notifyOfStatus(result);
			dog.setSecondsDelay(interval);
			timer.updateWatchdog(dog);  // pet dog
		}
	}
	/**
	 * The interval has expired. Propagate a FALSE
	 */
	@Override
	public void evaluate() {
		log.debugf("%s.evaluate",getName());
		if( !isLocked() ) {
			setState(TruthValue.FALSE);
			QualifiedValue result = new TestAwareQualifiedValue(timer,state.name(),DataQuality.GOOD_DATA);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(result);
		}
	}
	
	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.infof("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_INTERVAL)) {
			try {
				interval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_TRIGGER)) {
			try {
				trigger = TruthValue.valueOf(event.getNewValue().toString().toUpperCase());
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
		prototype.setTooltipText("On signal, emit a truth-value for a configured interval");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setEmbeddedIcon("Block/icons/embedded/signal_pulse.png");
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.SQUARE);
		view.setPreferredHeight(60);
		view.setPreferredWidth(60);
	}
}