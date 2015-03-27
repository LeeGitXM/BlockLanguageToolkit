/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

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
import com.ils.blt.common.block.PlacementHint;
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
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * Use an auxiliary text input to override quality data on the main input.
 */
@ExecutableBlock
public class DataConditioner extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "DataConditioner";
	private static final String VALUE_PORT_NAME = "value";
	private static final String QUALITY_PORT_NAME = "quality";
	private static final String OUT_PORT_NAME = "out";
	private static final String STATUS_PORT_NAME = "status";
	private final QualifiedValue BAD_VALUE = new BasicQualifiedValue(0.0,new BasicQuality("BAD",Quality.Level.Bad));
	private final Watchdog dog;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	private TruthValue qualityInput = TruthValue.UNKNOWN;
	private QualifiedValue value = null;
	private TruthValue truthValue = TruthValue.UNSET;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public DataConditioner() {
		dog = new Watchdog(TAG,this);
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
	public DataConditioner(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("DataConditioner");
		truthValue = TruthValue.UNSET;
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
		// Define a two inputs -- one for the data, one for the quality
		AnchorPrototype input = new AnchorPrototype(VALUE_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("V");
		anchors.add(input);
		input = new AnchorPrototype(QUALITY_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setAnnotation("Q");
		input.setHint(PlacementHint.L);
		anchors.add(input);

		// Define two outputs
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
		AnchorPrototype sig = new AnchorPrototype(STATUS_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(sig);
	}
	
	@Override
	public void reset() {
		super.reset();
		qualityInput = TruthValue.UNKNOWN;
		value = null;
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
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		String blockId = vcn.getConnection().getSource().toString();
		QualifiedValue qv = vcn.getValue();

		if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(VALUE_PORT_NAME)) {
			value = qv;
			//log.infof("%s.acceptValue got VALUE =  %s", TAG,qv.getValue().toString());
		}
		else if (vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(QUALITY_PORT_NAME)) {
			if( qv.getQuality().isGood()) {
				qualityInput = qualifiedValueAsTruthValue(qv);
				//log.infof("%s.acceptValue got QUALITY =  %s", TAG,qv.getValue().toString());
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
		if( value != null && !locked  ) {
			truthValue = qualityInput;
			if( !truthValue.equals(TruthValue.TRUE) && !value.getQuality().isGood()) truthValue = TruthValue.TRUE;
			log.tracef("%s.evaluate value %s(%s)", getName(),value.getValue().toString(),truthValue.name());
			
			if( !truthValue.equals(TruthValue.TRUE) ) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,value);
				controller.acceptCompletionNotification(nvn);
				//log.tracef("%s.evaluate: propagating %s %s",getName(),value.getValue().toString(),value.getQuality().getName());
			}
			else {
				value = BAD_VALUE;
			}
			QualifiedValue result = new BasicQualifiedValue(truthValue);
			OutgoingNotification nvn = new OutgoingNotification(this,STATUS_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(value,result);
		}
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new BasicQualifiedValue(truthValue);
		notifyOfStatus(value,qv);
		
	}
	private void notifyOfStatus(QualifiedValue val,QualifiedValue tv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, val);
		controller.sendConnectionNotification(getBlockId().toString(), STATUS_PORT_NAME, tv);
	}
	/**
	 * @return a block-specific description of internal status. Add quality to the default list
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Quality", qualityInput.name());
		attributes.put("Result", truthValue.name());
		return descriptor;
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/data_conditioner.png");
		prototype.setPaletteLabel("Conditioner");
		prototype.setTooltipText("Apply additional quality constraints to the input.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_MISC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("Data\nConditioner");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setEmbeddedFontSize(20);
		desc.setPreferredHeight(80);
		desc.setPreferredWidth(100);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY); 
	}
}