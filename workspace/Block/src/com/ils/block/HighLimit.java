/**
 *   (c) 2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
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
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class is a no-op. It simply passes its input onto the output.
 */
@ExecutableBlock
public class HighLimit extends AbstractProcessBlock implements ProcessBlock {
	private double limit   = 0.;
	// Keep map of values by originating block id
	protected final Map<String,QualifiedValue> qualifiedValueMap;
	private final Watchdog dog;
	double currentValue = Double.NaN;
	private BlockProperty valueProperty = null;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public HighLimit() {
		qualifiedValueMap = new HashMap<>();
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
	public HighLimit(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		qualifiedValueMap = new HashMap<>();
		initialize();
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("HighLimit");
		
		BlockProperty bp = new BlockProperty(BlockConstants.BLOCK_PROPERTY_LIMIT,limit,PropertyType.DOUBLE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_LIMIT, bp);

		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,synchInterval,PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,Double.NaN,PropertyType.DOUBLE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);

		// We allow multiple connections on the input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setIsMultiple(false);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		timer.removeWatchdog(dog);
	}
	/**
	 * Initialize the qualified value map.
	 */
	@Override
	public void start() {
		super.start();
		reconcileQualifiedValueMap(BlockConstants.IN_PORT_NAME,qualifiedValueMap,Double.NaN);
		log.debugf("%s.start: initialized %d inputs",getName(),qualifiedValueMap.size());
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
	 * Notify the block that a new value has appeared on one of its input anchors.
	 * For now we simply record the change in the map and start the watchdog. 
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		QualifiedValue qv = incoming.getValue();
		if( qv!=null && qv.getValue()!=null ) {
			String key = incoming.getConnection().getSource().toString();
			try {
				Double dbl = Double.parseDouble(qv.getValue().toString());
				qv = new BasicQualifiedValue(dbl,qv.getQuality(),qv.getTimestamp());
				dog.setSecondsDelay(synchInterval);
				log.tracef("%s.acceptValue got %s for %s", getName(),dbl.toString(),key);
				timer.updateWatchdog(dog);  // pet dog
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",getName(),nfe.getLocalizedMessage());
				qv = new BasicQualifiedValue(Double.NaN,new BasicQuality(nfe.getLocalizedMessage(),Quality.Level.Bad),qv.getTimestamp());
			}
			qualifiedValueMap.put(key, qv);
		}
		else {
			log.warnf("%s.acceptValue: received null value",getName());
		}
	}
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 * if it has changed.
	 */
	@Override
	public void evaluate() {
		log.tracef("%s.evaluate",getName());
		if( !isLocked() ) {
			lastValue = getMaxValue();
			if( lastValue!=null ) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				valueProperty.setValue(lastValue.getValue());
				notifyOfStatus(lastValue);
			}
		}
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("CurrentMaximum", valueProperty.getValue().toString());
		for(String key:qualifiedValueMap.keySet()) {
			QualifiedValue qv = (QualifiedValue)qualifiedValueMap.get(key);
			if( qv!=null && qv.getValue()!=null) {
				attributes.put(key, String.valueOf(qv.getValue()));
			}
			else {
				attributes.put(key,"NULL"); 
			}
		}
		return descriptor;
	}

	/**
	 * Handle a change to the limit or coalescing interval.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_LIMIT)) {
			try {
				limit = Double.parseDouble(event.getNewValue().toString());
				evaluate();
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert limit to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL)) {
			try {
				Double interval = Double.parseDouble(event.getNewValue().toString());
				if( interval > 0.0 ) synchInterval = interval;
				else {
					log.warnf("%s: propertyChange Synch interval must be positive",getName());
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert synch interval to an double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv) {
		updateStateForNewValue(qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * On a save, make sure that our map of connections is proper. 
	 * We are only concerned with the in port as it allows multiple connections
	 */
	@Override
	public void validateConnections() {
		reconcileQualifiedValueMap(BlockConstants.IN_PORT_NAME,qualifiedValueMap,Double.NaN);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/maxlimit.png");
		prototype.setPaletteLabel("HighLimit");
		prototype.setTooltipText("Determine the maximim value among inputs subject to an entered maximum");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/max.png");
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_BLUE_GRAY);
	}
	
	/**
	 * Compute the maximum, presumably because of a new input.
	 */
	private QualifiedValue getMaxValue() {
		Collection<QualifiedValue> values = qualifiedValueMap.values();
		double max = -Double.MAX_VALUE;
		QualifiedValue result = new BasicQualifiedValue(max);
		
		for(QualifiedValue qv:values) {
			if(qv.getQuality().isGood() && qv.getValue()!=null && !qv.getValue().toString().isEmpty() && !qv.getValue().equals(Double.NaN)) {
				double val = fcns.coerceToDouble(qv.getValue().toString());
				if(val>max ) {
					max = val;
					if( val>limit ) {
						result = new BasicQualifiedValue(new Double(limit));
					}
					else {
						result = qv;
					}
					
				}
			}
			else {
				return new BasicQualifiedValue(Double.NaN,new BasicQuality("Bad input",Quality.Level.Bad),qv.getTimestamp());
			}
		}
		return result;	
	}
}