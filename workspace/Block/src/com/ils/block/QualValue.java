/**
 *   (c) 2015  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
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
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataQuality;

/**
 * Construct a qualified value from separate inputs for value, quality and time.
 * NOTE: Avoid the confusion of two classes named "QualifiedValue" 
 */
@ExecutableBlock
public class QualValue extends AbstractProcessBlock implements ProcessBlock {
	private final static String QUALITY_PORT = "quality";
	private final static String TIME_PORT    = "time";
	private final static String VALUE_PORT   = "value";

	private final Watchdog dog;
	private QualifiedValue value = null;
	private Quality   quality = DataQuality.GOOD_DATA;
	private Date      timestamp    = null;
	private SimpleDateFormat customFormatter = new SimpleDateFormat(DEFAULT_FORMAT);
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public QualValue() {
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
	public QualValue(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("QualifiedValue");
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		BlockProperty formatProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FORMAT,DEFAULT_FORMAT,PropertyType.STRING,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_FORMAT, formatProperty);
		
		// Define an input for each of the three components
		AnchorPrototype qual = new AnchorPrototype(QUALITY_PORT,AnchorDirection.INCOMING,ConnectionType.TEXT);
		qual.setAnnotation("Q");
		anchors.add(qual);
		AnchorPrototype tim = new AnchorPrototype(TIME_PORT,AnchorDirection.INCOMING,ConnectionType.ANY);
		tim.setAnnotation("T");
		anchors.add(tim);
		AnchorPrototype input = new AnchorPrototype(VALUE_PORT,AnchorDirection.INCOMING,ConnectionType.ANY);
		input.setAnnotation("V");
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		timestamp = null;
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
	 * For now we simply record the change in the map and start the watchdog. Only
	 * start the watchdog on receipt of a value. 
	 * 
	 * @param incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		String port = incoming.getConnection().getDownstreamPortName();
		QualifiedValue qv = incoming.getValue();
		if( port.equals(VALUE_PORT)  ) {
			value = qv;
			if( synchInterval>0 ) {
				dog.setSecondsDelay(synchInterval);
				timer.updateWatchdog(dog);  // pet dog
			}
		}
		else if( port.equals(QUALITY_PORT)  ) {
			if( qv.getValue().toString().equalsIgnoreCase("good")) quality = DataQuality.GOOD_DATA;
			else quality = new BasicQuality(qv.getValue().toString(),Quality.Level.Bad);
		}
		else if( port.equals(TIME_PORT)  ) {
			if( qv.getValue() instanceof Date ) {
				timestamp = (Date)qv.getValue();
				log.tracef("%s.acceptValue: Received date as (%s)",getName(),customFormatter.format(timestamp));
			}
			else {
				try {
					timestamp = customFormatter.parse(qv.getValue().toString());
					log.tracef("%s.acceptValue: time as string (%s)",getName(),customFormatter.format(timestamp));
				}
				catch(ParseException pe) {
					log.errorf("%s.acceptValue: Exception formatting time as %s (%s)",getName(),customFormatter.toString(),pe.getLocalizedMessage());
				} 
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
		if( value!=null ) attributes.put("Value", value.getValue().toString());
		attributes.put("Quality", quality.toString());
		if(timestamp!=null) attributes.put("Timestamp",dateFormatter.format(timestamp));
		return descriptor;
	}
	/**
	 * The coalescing time has expired. Place the composite value on the output.
	 * Evaluation is triggered by a new entry on any of the inputs.
	 */
	@Override
	public void evaluate() {
		if( value==null ) return;   // Shouldn't happen
		if( !isLocked() ) {
			Quality q = quality;
			if( q.isGood() && value.getQuality()!=null ) q = value.getQuality();
			Date ts = value.getTimestamp();
			if(timestamp!=null ) ts = timestamp;
			QualifiedValue result = new BasicQualifiedValue(value.getValue(),q,ts);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(result);
			value = result;
			log.tracef("%s.evaluate: %s %s %s",getName(),value.getValue().toString(),
					value.getQuality().getName(),customFormatter.format(value.getTimestamp()));
		}
	}

	/**
	 * Handle a change to the coalescing interval.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_FORMAT)) {
			customFormatter = new SimpleDateFormat(event.getNewValue().toString());
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL)) {
			try {
				synchInterval = Double.parseDouble(event.getNewValue().toString());
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
	public void notifyOfStatus() {
		notifyOfStatus(value);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		if( qv!=null && qv.getValue()!=null ) {
			controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
			controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
		}
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/qualified_value.png");
		prototype.setPaletteLabel("QualValue");
		prototype.setTooltipText("Create a qualified value from separate value, quality and timestamp inputs");
		prototype.setTabName(BlockConstants.PALETTE_TAB_MISC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("QV");
		desc.setEmbeddedFontSize(18);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}