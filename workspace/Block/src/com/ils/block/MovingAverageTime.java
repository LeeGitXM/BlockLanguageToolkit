/**
 *   (c) 2014-1015  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
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
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class computes an average of the input over a time interval.
 */
@ExecutableBlock
public class MovingAverageTime extends AbstractProcessBlock implements ProcessBlock {
	private final LinkedList<QualifiedValue> buffer;
	private boolean clearOnReset = false;
	private QualifiedValue currentValue = null;
	private double scanInterval = 15.0;    // ~secs
	private double timeWindow = 60;        // ~ secs
	private final Watchdog dog;
	private BlockProperty valueProperty = null;
	private final UtilityFunctions fncs = new UtilityFunctions();
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public MovingAverageTime() {
		initialize();
		buffer = new LinkedList<>();
		dog = new Watchdog(getName(),this);
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom properties are limit, standardDeviation
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public MovingAverageTime(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
		buffer = new LinkedList<>();
		
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("MovingAverageTime");
		
		BlockProperty resetProperty =  new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,Boolean.FALSE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, resetProperty);
		BlockProperty windowProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW,new Double(timeWindow),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW, windowProperty);
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL,new Double(scanInterval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL, intervalProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,new Double(Double.NaN),PropertyType.DOUBLE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input.
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);

		// Define the main output.
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		timer.removeWatchdog(dog);    // Stop evaluation
		state = TruthValue.UNSET;
		if( clearOnReset ) {
			buffer.clear();
			currentValue = null;
		}
	}

	@Override
	public void start() {
		super.start();
	}
	@Override
	public void stop() {
		timer.removeWatchdog(dog);
	}
	
	/**
	 * A new value has arrived. Simply set the current value.
	 * (We poll the current value on an interval).
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		QualifiedValue qv = incoming.getValue();
		Quality qual = qv.getQuality();
		if( qual.isGood() && qv.getValue()!=null ) {
			try {
				currentValue = qv;
				log.tracef("%s.acceptValue: %s (%3.1f)",getName(),qv.getValue().toString(),currentValue);
				if( !dog.isActive() && scanInterval>0.0 ) {
					dog.setSecondsDelay(scanInterval);
					timer.updateWatchdog(dog);  // pet dog
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue exception converting incoming %s to double (%s)",getName(),qv.getValue().toString(),nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.acceptValue received a GOOD value, but null",getName());
		}
	}
	
	/**
	 * The interval timer has expired. Evaluate the buffer.
	 */
	@Override
	public void evaluate() {
		if( currentValue==null) return;
		// Evaluate the buffer and report
		// Add the currentValue to the queue
		buffer.addLast(currentValue);
		int maxPoints = (int)((timeWindow+0.99*scanInterval)/scanInterval);
		while(buffer.size() > maxPoints ) {
			buffer.removeFirst();
		}
		log.tracef("%s(%d).evaluate %d of %d points",getName(),hashCode(),buffer.size(),maxPoints);
		if( buffer.size() >= maxPoints) {
			double result = computeAverage();
			log.tracef("%s(%d).evaluate avg=%f",getName(),hashCode(),result);
			if( !isLocked() ) {
				// Give it a new timestamp
				QualifiedValue outval = new BasicQualifiedValue(result);
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(outval);
			}
			// Even if locked, we update the current state
			valueProperty.setValue(result);
			controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,new BasicQualifiedValue(result));
		}

		dog.setSecondsDelay(scanInterval);
		timer.updateWatchdog(dog);  // pet dog
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new BasicQualifiedValue(valueProperty.getValue());
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.infof("%s(%d).propertyChange: %s = %s",getName(),hashCode(),propertyName,event.getNewValue().toString());
		if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET)) {
			try {
				clearOnReset = Boolean.parseBoolean(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert clear flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}	
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL)) {
			try {
				double oldInterval = scanInterval;
				scanInterval = Double.parseDouble(event.getNewValue().toString());
				if( scanInterval < 0.1 ) scanInterval = 0.1;   // Don't allow to go too fast
				if( dog.isActive() && scanInterval < oldInterval ) {
					dog.setSecondsDelay(scanInterval);
					timer.updateWatchdog(dog);  // pet dog
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW)) {
			try {
				timeWindow = Double.parseDouble(event.getNewValue().toString());
				if( timeWindow<=0.0) timeWindow = scanInterval;
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}	
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		log.infof("%s(%d).getInternalStatus: buffer size = %d",getName(),hashCode(),buffer.size());
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Average", String.valueOf(currentValue));
		List<Map<String,String>> descBuffer = descriptor.getBuffer();
		Iterator<QualifiedValue> walker = buffer.iterator();
		while( walker.hasNext() ) {
			Map<String,String> qvMap = new HashMap<>();
			QualifiedValue qv = walker.next();
			qvMap.put("Value", qv.getValue().toString());
			qvMap.put("Quality", qv.getQuality().toString());
			qvMap.put("Timestamp", qv.getTimestamp().toString());
			descBuffer.add(qvMap);
		}

		return descriptor;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/moving_average.png");
		prototype.setPaletteLabel("TimeAve");
		prototype.setTooltipText("Compute the moving average of the input and place results on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/xbart.png");
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	/**
	 * Compute the average, presumably because of a scan interval timeout.
	 */
	private double computeAverage() {
		double result = 0.0;
		double sum = 0.0;
		int count = 0;
		
		for( QualifiedValue qv:buffer) {
			if( qv==null ) continue;  // Shouldn't happen
			double val = fncs.coerceToDouble(qv.getValue());
			sum = sum + val;
			count++;
		}
		
		if( count>0 ) result = sum/count;
		return result;	
	}
}