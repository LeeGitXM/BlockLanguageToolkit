/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
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
	private final String TAG = "Product";
	// Keep map of values by originating block id
	protected final Map<String,QualifiedValue> valueMap;
	private final Watchdog dog;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Product() {
		dog = new Watchdog(TAG,this);
		valueMap = new HashMap<String,QualifiedValue>();
		initialize();
		initializePrototype();
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
		dog = new Watchdog(TAG,this);
		valueMap = new HashMap<String,QualifiedValue>();
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("Product");
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
		// Define a single input -- but allow multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
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
				log.warnf("%s: propertyChange Unable to convert synch interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
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
			String key = String.format("%s:%s",incoming.getConnection().getSource().toString(),
					   incoming.getConnection().getUpstreamPortName());
			try {
				Double dbl = Double.parseDouble(qv.getValue().toString());
				qv = new BasicQualifiedValue(dbl,qv.getQuality(),qv.getTimestamp());
				dog.setSecondsDelay(synchInterval);
				log.tracef("%s.acceptValue got %s for %s", TAG,dbl.toString(),key);
				timer.updateWatchdog(dog);  // pet dog
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",TAG,nfe.getLocalizedMessage());
				qv = new BasicQualifiedValue(Double.NaN,new BasicQuality(nfe.getLocalizedMessage(),Quality.Level.Bad),qv.getTimestamp());
			}
			valueMap.put(key, qv);
		}
		else {
			log.warnf("%s.acceptValue: received null value",TAG);
		}
	}
	
	
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 * if it has changed.
	 */
	@Override
	public void evaluate() {
		if( !isLocked() && !valueMap.isEmpty()) {
			double value = getAggregateResult();
			QualifiedValue result = new BasicQualifiedValue(new Double(value),getAggregateQuality());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(result);
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
				if( qv.getQuality().isGood() ) {
					result = result*((Double)qv.getValue()).doubleValue();
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