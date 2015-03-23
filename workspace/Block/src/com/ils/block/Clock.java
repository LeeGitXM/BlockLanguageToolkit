/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 *   Code based on sample code at: 
 *        http://www.codeproject.com/Articles/36459/PID-process-control-a-Cruise-Control-example
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
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Emit a configured signal on a configured interval
 */
@ExecutableBlock
public class Clock extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Clock";
	private double interval = 60;  // ~secs
	private String command = BlockConstants.COMMAND_START;
	private final Watchdog dog;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Clock() {
		dog = new Watchdog(TAG,this);
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom properties are limit, standardDeviation
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Clock(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Clock");
		this.isReceiver = true;
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL,new Double(interval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL, intervalProperty);
		
		BlockProperty commandProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_COMMAND,command,PropertyType.STRING,false);
		setProperty(BlockConstants.BLOCK_PROPERTY_COMMAND, commandProperty);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	/**
	 * On reset report a false.
	 */
	@Override
	public void reset() {
		if( !isLocked() ) {
			OutgoingNotification sig = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,new BasicQualifiedValue(TruthValue.FALSE.name()));
			controller.acceptCompletionNotification(sig);
		}
	}
	// Initially set the value FALSE
	@Override
	public void start() {
		if( !running ) {
			log.infof("%s.start: emit FALSE",TAG);
			if( !isLocked() ) {
				OutgoingNotification sig = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,new BasicQualifiedValue(TruthValue.FALSE.name()));
				controller.acceptCompletionNotification(sig);
			}
		}
		super.start();
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
	 * We've received a transmitted signal. If it is appropriate 
	 * based on our configured filters, forward the signal on to our output.
	 * @param sn 
	 */
	public void acceptValue(SignalNotification sn) {
		Signal signal = sn.getSignal();
		log.infof("%s.acceptValue: signal = %s",TAG,signal.getCommand());
		if( signal.getCommand() != null && signal.getCommand().equalsIgnoreCase(command) && interval > 0) {
			dog.setSecondsDelay(interval);
			timer.updateWatchdog(dog);  // pet dog
		}
	}
	
	/**
	 * The interval has expired. Turn true. Do nothing else unless we are reset.
	 */
	@Override
	public synchronized void evaluate() {
		log.infof("%s.evaluate ... %f secs",TAG,interval);
		
		if( !isLocked() ) {
			QualifiedValue qv = new BasicQualifiedValue(TruthValue.TRUE.name());
			OutgoingNotification sig = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
			controller.acceptCompletionNotification(sig);
			notifyOfStatus(qv);
		}
		timer.removeWatchdog(dog);
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
	 * Note, an interval change resets the timeput period.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.infof("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_INTERVAL)) {
			try {
				interval = Double.parseDouble(event.getNewValue().toString());
				if( dog.isActive() && interval>0.0 ) {
					dog.setSecondsDelay(interval);
					timer.updateWatchdog(dog);  // pet dog
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}
	
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/clock.png");
		prototype.setPaletteLabel("Clock");
		prototype.setTooltipText("Send a signal to connected blocks");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/alarm_clock.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setStyle(BlockStyle.SQUARE);
		desc.setReceiveEnabled(true);
	}
}