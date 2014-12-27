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
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Emit a "reset" signal on a configured interval
 */
@ExecutableBlock
public class Reset extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Reset";
	private final static double MIN_RESET_INTERVAL = 0.05;  // Do not spin faster than this.
	private Signal signal = new Signal(BlockConstants.COMMAND_RESET,"","");
	private double interval = Double.MAX_VALUE;             // ~secs (essentially never)
	private final Watchdog dog;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Reset() {
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
	public Reset(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Reset");

		BlockProperty commandProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_COMMAND,signal.getCommand(),PropertyType.STRING,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_COMMAND, commandProperty);
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL,new Double(interval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL, intervalProperty);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.SIGNAL);
		output.setHint(PlacementHint.R);  // Got wierd behavior if Top
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		this.state = BlockState.ACTIVE;
		dog.setSecondsDelay(interval);
		if( interval>MIN_RESET_INTERVAL) controller.pet(dog);
	}
	@Override
	public void start() {
		if( !running ) {
			log.debugf("%s.start",TAG);
			this.state = BlockState.ACTIVE;
			dog.setSecondsDelay(interval);
			if( interval>MIN_RESET_INTERVAL) controller.pet(dog);
		}
		super.start();
	}
	@Override
	public void stop() {
		this.state = BlockState.INITIALIZED;
		controller.removeWatchdog(dog);
		log.debugf("%s.stop",TAG);
	}

	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.infof("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_COMMAND)) {
			signal = new Signal(event.getNewValue().toString(),"","");
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_INTERVAL)) {
			try {
				interval = Double.parseDouble(event.getNewValue().toString());
				if( interval>MIN_RESET_INTERVAL) {
					dog.setSecondsDelay(interval);
					controller.pet(dog);
				}
				else {
					controller.removeWatchdog(dog);
				}
				
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}
	
	/**
	 * The interval has expired. Reset interval, then compute output.
	 * Do not compute anything until all parameters have been set.
	 */
	@Override
	public synchronized void evaluate() {
		log.infof("%s.evaluate ... %f secs",TAG,interval);
		if( !isLocked() ) {
			QualifiedValue result = new BasicQualifiedValue(signal);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(result);
		}
		if( interval>MIN_RESET_INTERVAL ) {
			dog.setSecondsDelay(interval);
			controller.pet(dog);
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
		prototype.setPaletteIconPath("Block/icons/palette/reset.png");
		prototype.setPaletteLabel("Reset");
		prototype.setTooltipText("Send a \"reset\" signal to connected blocks");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/reset.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setStyle(BlockStyle.SQUARE);
		desc.setReceiveEnabled(true);
	}
}