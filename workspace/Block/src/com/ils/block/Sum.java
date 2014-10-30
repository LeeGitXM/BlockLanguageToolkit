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
import com.ils.blt.common.block.BlockState;
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
 * This class sums its inputs. The configured interval is a
 * synchronization time.
 */
@ExecutableBlock
public class Sum extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Sum";
	// Keep map of values by originating block id
	protected final Map<String,QualifiedValue> valueMap;
	private final Watchdog dog;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Sum() {
		dog = new Watchdog(TAG,this);
		valueMap = new HashMap<String,QualifiedValue>();
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "SyncInterval".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Sum(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		valueMap = new HashMap<String,QualifiedValue>();
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Sum");
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
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
		valueMap.clear();
	}

	/**
	 * Notify the block that a new value has appeared on one of its input anchors.
	 * We record the value and start the watchdog timer.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		this.state = BlockState.ACTIVE;
		
		QualifiedValue qv = incoming.getValue();
		if( qv!=null && qv.getValue()!=null ) {
			String key = String.format("%s:%s",incoming.getConnection().getSource().toString(),
					                           incoming.getConnection().getUpstreamPortName());
			try {
				Double dbl = Double.parseDouble(qv.getValue().toString());
				qv = new BasicQualifiedValue(dbl,qv.getQuality(),qv.getTimestamp());
				dog.setSecondsDelay(synchInterval);
				log.tracef("%s.acceptValue got %s for %s", TAG,dbl.toString(),key);
				controller.pet(dog);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",TAG,nfe.getLocalizedMessage());
				qv = new BasicQualifiedValue(Double.NaN,new BasicQuality(nfe.getLocalizedMessage(),Quality.Level.Bad),qv.getTimestamp());
			}
			valueMap.put(key, qv);
		}
	}
	
	/**
	 * The coalescing time has expired. Place the sum of all inputs on the output.
	 */
	@Override
	public void evaluate() {
		if( !isLocked() && !valueMap.isEmpty()) {
			double value = getAggregateResult();
			QualifiedValue result = new BasicQualifiedValue(new Double(value),getAggregateQuality());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
		}
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
	 * Define the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/sum.png");
		prototype.setPaletteLabel("Sum");
		prototype.setTooltipText("Sum incoming values and place on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ARITHMETIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/sigma.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	
	/**
	 * Compute the overall sum, presumably because of a new input.
	 * The datatype of the QualifiedValue is guaranteed to be a Double.
	 */
	private double getAggregateResult() {
		Collection<QualifiedValue> values = valueMap.values();
		double result = Double.NaN;
		result = 0.;
		for(QualifiedValue qv:values) {
			if( qv.getQuality().isGood() ) {
				result = result+((Double)qv.getValue()).doubleValue();
			}
			else {
				return Double.NaN;
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