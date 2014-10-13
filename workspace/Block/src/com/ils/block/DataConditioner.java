/**
 *   (c) 2014  ILS Automation. All rights reserved. 
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
import com.ils.blt.common.control.BlockPropertyChangeEvent;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.control.IncomingNotification;
import com.ils.blt.common.control.OutgoingNotification;
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
	protected static String DATA_PORT_NAME = "value";
	protected static String QUALITY_PORT_NAME = "quality";
	protected static String OUT_PORT_NAME = "out";
	protected static String STATUS_PORT_NAME = "status";
	private final Watchdog dog;
	private double synchInterval = 0.0; // default
	protected String quality = "good";
	private QualifiedValue value = null;
	
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
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
		// Define a two inputs -- one for the divisor, one for the dividend
		AnchorPrototype input = new AnchorPrototype(DATA_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("V");
		anchors.add(input);
		input = new AnchorPrototype(QUALITY_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TEXT);
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
		quality = "good";
		value = null;
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
		if( qv!=null && qv.getValue()!=null && qv.getQuality().isGood()) {
			if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(DATA_PORT_NAME)) {
				value = qv;
			}
			else if (vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(QUALITY_PORT_NAME)) {
				quality = qv.getValue().toString();
			}
			else {
				log.warnf("%s.acceptValue: Unexpected port designation (%s)",TAG,vcn.getConnection().getDownstreamPortName());
			}
			dog.setSecondsDelay(synchInterval);
			controller.pet(dog);
			log.infof("%s.acceptValue got %s for %s", TAG,qv.getValue().toString(),blockId);
		}
	}
	
	
	/**
	 * The coalescing time has expired. Place the value on the output, possibly
	 * modifying its quality..
	 */
	@Override
	public void evaluate() {
		if( value != null ) {
			OutgoingNotification nvn = null;
			if( quality.equalsIgnoreCase("good") ||
				!value.getQuality().isGood() ) {
				nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,value);
			}
			else {
				BasicQuality q = new BasicQuality(quality,Quality.Level.Bad);
				QualifiedValue qv = new BasicQualifiedValue(value.getValue(),q,value.getTimestamp());
				nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
			}
			
			
			controller.acceptCompletionNotification(nvn);
		}
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