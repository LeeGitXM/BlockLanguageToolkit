/**
 *   (c) 2014-2019  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Collection;
import java.util.HashMap;
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
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class emits the "product" of its inputs. Synchronizing
 * is available. Inputs and outputs are data values.
 */
@ExecutableBlock
public class Product extends AbstractProcessBlock implements ProcessBlock {
	// Keep map of values by originating block id
	protected final Map<String,QualifiedValue> valueMap;
	private final Watchdog dog;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Product() {
		initialize();
		initializePrototype();
		dog = new Watchdog(getName(),this);
		valueMap = new HashMap<>();
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Product(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
		valueMap = new HashMap<>();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("Product");
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
		// Define a single input -- but allow multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setIsMultiple(true);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}

	/**
	 * Initialize the qualified value map.
	 */
	@Override
	public void start() {
		super.start();
		reconcileQualifiedValueMap(BlockConstants.IN_PORT_NAME,valueMap,Double.NaN);
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
				log.tracef("%s.acceptValue got %s for %s",getName(),dbl.toString(),key);
				timer.updateWatchdog(dog);  // pet dog
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",getName(),nfe.getLocalizedMessage());
				qv = new BasicQualifiedValue(Double.NaN,new BasicQuality(nfe.getLocalizedMessage(),Quality.Level.Bad),qv.getTimestamp());
			}
			valueMap.put(key, qv);
			recordActivity(Activity.ACTIVITY_RECEIVE,key,qv.getValue().toString());
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
		//log.infof("%s.evaluate ...", getName());
		if( !isLocked() && !valueMap.isEmpty()) {
			double value = getAggregateResult();
			log.debugf("%s.evaluate ... value = %3.2f", getName(),value);
			lastValue = new TestAwareQualifiedValue(timer,new Double(value),getAggregateQuality());
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
		Map<String,String> attributes = descriptor.getAttributes();
		for(String key:valueMap.keySet()) {
			QualifiedValue qv = (QualifiedValue)valueMap.get(key);
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
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(lastValue);
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Handle a change to the coalescing interval.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL)) {
			try {
				synchInterval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert synch interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * On a save, make sure that our map of connections is proper. 
	 * We only care about the in port which allows multiple connections.
	 */
	@Override
	public void validateConnections() {
		reconcileQualifiedValueMap(BlockConstants.IN_PORT_NAME,valueMap,Double.NaN);
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/product.png");
		prototype.setPaletteLabel("Product");
		prototype.setTooltipText("Multiply incoming values and place result changes on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ARITHMETIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/pi.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY); 
	}
	
	/**
	 * Compute the overall product, presumably because of a new input.
	 * The datatype of the QualifiedValue is guaranteed to be a Double.
	 */
	private double getAggregateResult() {
		Collection<QualifiedValue> values = valueMap.values();
		double result = Double.NaN;
		if(!values.isEmpty()) {
			result = 1.;
			for(QualifiedValue qv:values) {
				if( qv.getQuality().isGood() && qv.getValue()!=null && !qv.getValue().equals(Double.NaN) ) {
					log.tracef("%s.aggregating ... value = %sf",getName(),qv.getValue().toString());
					result = result*fcns.coerceToDouble(qv.getValue());
				}
				else {
					return Double.NaN;
				}
			}
		}
		return result;	
	}
	/**
	 * Compute the overall product, presumably because of a new input.
	 *  valueMap is guaranteed to be non-empty.
	 */
	private Quality getAggregateQuality() {
		Collection<QualifiedValue> values = valueMap.values();
		Quality result = null;
		for(QualifiedValue qv:values) {
			result = qv.getQuality();
			if( !result.isGood() ) break;
		}
		return result;	
	}
}