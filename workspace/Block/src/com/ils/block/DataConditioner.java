/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Use an auxiliary truth-value input to override quality data on the main input.
 * For this block, "state" applies to the quality input.
 */
@ExecutableBlock
public class DataConditioner extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "DataConditioner";
	private static final String VALUE_PORT_NAME = "value";
	private static final String QUALITY_PORT_NAME = "quality";
	private static final String STATUS_PORT_NAME = "status";
	private final Watchdog dog;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	private BlockProperty valueProperty = null;
	private TruthValue lastQuality = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public DataConditioner() {
		dog = new Watchdog(TAG,this);
		initialize();
		initializePrototype();
		state = TruthValue.UNKNOWN;
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public DataConditioner(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
		state = TruthValue.UNKNOWN;
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("DataConditioner");
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a two inputs -- one for the data, one for the quality
		AnchorPrototype input = new AnchorPrototype(VALUE_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("V");
		input.setHint(PlacementHint.LT);
		input.setIsMultiple(false);
		anchors.add(input);
		input = new AnchorPrototype(QUALITY_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setAnnotation("Q");
		input.setHint(PlacementHint.LB);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define two outputs
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("D");
		anchors.add(output);
		AnchorPrototype sig = new AnchorPrototype(STATUS_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		sig.setAnnotation("S");
		anchors.add(sig);
	}
	
	@Override
	public void reset() {
		super.reset();
		lastQuality = null;
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
	 * Note: we are not calling the super as it sets lastValue for either port
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		String blockId = vcn.getConnection().getSource().toString();
		QualifiedValue qv = vcn.getValue();
		String port = vcn.getConnection().getDownstreamPortName();
		recordActivity(Activity.ACTIVITY_RECEIVE,port,qv.getValue().toString());

		if( port.equalsIgnoreCase(VALUE_PORT_NAME)) {
			lastValue = qv;
			//log.infof("%s.acceptValue got VALUE =  %s", TAG,qv.getValue().toString());
		}
		else if (port.equalsIgnoreCase(QUALITY_PORT_NAME)) {
			lastQuality = qualifiedValueAsTruthValue(qv);

			if( qv.getQuality().isGood()) {
				// Update the timestamp of the data value
				if( lastValue!=null ) {
					lastValue = new TestAwareQualifiedValue(timer,lastValue.getValue(),lastValue.getQuality());
				}
			}
		}
		else {
			log.warnf("%s.acceptValue: Unexpected port designation (%s)",TAG,vcn.getConnection().getDownstreamPortName());
		}
		dog.setSecondsDelay(synchInterval);
		timer.updateWatchdog(dog);  // pet dog
		log.debugf("%s.acceptValue got %s for %s", TAG,qv.getValue().toString(),blockId);
	}
	/**
	 * The coalescing time has expired. Place the value on the output, but only if 
	 * quality indicators are good. The quality port indicates the quality where 
	 * a TRUE implied bad quality.
	 */
	@Override
	public void evaluate() {
		if( lastValue != null && lastQuality!=null &&  !locked  ) {
			state = TruthValue.FALSE;
			if( lastQuality.equals(TruthValue.TRUE))   state = TruthValue.TRUE;
			else if(!lastValue.getQuality().isGood())  state = TruthValue.TRUE;

		    // If the state is not TRUE, then propagate the output.	
			if( !state.equals(TruthValue.TRUE) ) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				//log.tracef("%s.evaluate: propagating %s %s",getName(),value.getValue().toString(),value.getQuality().getName());
			}

			QualifiedValue qv = new TestAwareQualifiedValue(timer,state);
			OutgoingNotification nvn = new OutgoingNotification(this,STATUS_PORT_NAME,qv);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus();
		}
	}
	/**
	 * Send status update notification for our last latest state. We only update the value notification
	 * if the quality is good.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,state);
		controller.sendConnectionNotification(getBlockId().toString(), STATUS_PORT_NAME, qv);
		if( lastValue!=null) {
			valueProperty.setValue(lastValue.getValue());
			controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,lastValue);
			if( !state.equals(TruthValue.TRUE)) {
				controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, lastValue);
			}
		}
	}
	
	
	/**
	 * We have a custom version as there are two ports. lastQuality is the last
	 * time we received input on the quality port. The state reflects the AND
	 * of the quality port and the value.
	 */
	@Override
	public void propagate() {
		super.propagate();
		
		if( lastQuality!=null && lastValue!=null ) {
			TruthValue temp = TruthValue.FALSE;
			if(lastQuality.equals(TruthValue.TRUE) )  temp = TruthValue.TRUE;
			else if(!lastValue.getQuality().isGood()) temp = TruthValue.TRUE;
			QualifiedValue qv = new BasicQualifiedValue(temp);
			OutgoingNotification nvn = new OutgoingNotification(this,STATUS_PORT_NAME,qv);
			controller.acceptCompletionNotification(nvn);
		}
	}
	
	/**
	 * @return a block-specific description of internal status. Add quality to the default list
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Quality", state.name());
		if(lastValue==null || lastValue.getValue()==null )
			attributes.put("Value", "None");
		else
			attributes.put("Value", lastValue.getValue().toString());
		return descriptor;
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/data_conditioner.png");
		prototype.setPaletteLabel("Conditioner");
		prototype.setTooltipText("Apply additional quality constraints to the input.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("Data\nConditioner");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setEmbeddedFontSize(12);
		desc.setPreferredHeight(80);
		desc.setPreferredWidth(100);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY); 
	}
}