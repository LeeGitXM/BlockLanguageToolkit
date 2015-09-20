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
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
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
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataQuality;

/**
 * Construct a qualified value from separate inputs for value and time.
 * The quality component comes from the "value" input. The time may be
 * either a date-time or String in SimpleDateFormat. 
 */
@ExecutableBlock
public class LabData extends Input implements ProcessBlock {
	private final static String BLOCK_PROPERTY_TIME_PATH = "TimeTagPath";
	private final static String BLOCK_PROPERTY_VALUE_PATH = "ValueTagPath";
	private BlockProperty timePathProperty = null;
	private BlockProperty valuePathProperty = null;
	
	private Quality quality = null;
	private final Watchdog dog;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public LabData() {
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
	public LabData(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	@Override
	protected void initialize() {	
		setName("LabData");
		// This property causes the engine to start a subscription.
		timePathProperty = new BlockProperty(BLOCK_PROPERTY_TIME_PATH,"",PropertyType.OBJECT,true);
		timePathProperty.setBinding("");
		timePathProperty.setBindingType(BindingType.TAG_READ);
		setProperty(BLOCK_PROPERTY_TIME_PATH, timePathProperty);


		valuePathProperty = new BlockProperty(BLOCK_PROPERTY_VALUE_PATH,"",PropertyType.OBJECT,true);
		valuePathProperty.setBinding("");
		valuePathProperty.setBindingType(BindingType.TAG_READ);
		setProperty(BLOCK_PROPERTY_VALUE_PATH, valuePathProperty);
		
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.OBJECT,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		BlockProperty formatProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FORMAT,DEFAULT_FORMAT,PropertyType.STRING,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_FORMAT, formatProperty);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(output);
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
	 * The block is notified that a new value has appeared on one of the pseudo 
	 * ports - either the value or time path property. Wait for the synchronization
	 * interval to emit the value.
	 * @param vcn notification of the new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		if( synchInterval>0 ) {
			dog.setSecondsDelay(synchInterval);
			timer.updateWatchdog(dog);  // pet dog
		}
	}

	/**
	 * The coalescing time has expired. Place the composite value on the output.
	 * Evaluation is triggered by a new entry either of the input tags. The values
	 * are in the tag path properties.
	 */
	@Override
	public void evaluate() {
		if( qv == null || valuePathProperty.getValue()==null || timePathProperty.getValue()==null ) return; // Shouldn't happen
		if( !isLocked() ) {
			Date timestamp = null;
			if( timePathProperty.getValue() instanceof Date ) {
				timestamp = (Date)timePathProperty.getValue();
				
			}
			else {
				try {
					timestamp = dateFormatter.parse(timePathProperty.getValue().toString());
				}
				catch(ParseException pe) {
					log.errorf("%s.acceptValue: Exception formatting time as %s (%s)",getName(),dateFormatter.toString(),pe.getLocalizedMessage());
				} 
			}
			log.infof("%s.evaluate: Using date as (%s)",getName(),dateFormatter.format(timestamp));
			QualifiedValue result = new BasicQualifiedValue(qv.getValue(),qv.getQuality(),timestamp);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(result);
			qv = result;
			log.infof("%s.evaluate: %s %s %s",getName(),qv.getValue().toString(),
					qv.getQuality().getName(),dateFormatter.format(qv.getTimestamp()));
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
			dateFormatter = new SimpleDateFormat(event.getNewValue().toString());
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
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/lab_data.png");
		prototype.setPaletteLabel("LabData");
		prototype.setTooltipText("Transmit a qualified value from separate value and timestamp tags");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("Lab Data");
		desc.setEmbeddedFontSize(16);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.ARROW);
		desc.setPreferredHeight(50);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_ROSE);
	}
	
	/**
	 * Check the block configuration for missing input tag.
	 * @return a validation summary. Null if everything checks out.
	 */
	@Override
	public String validate() {
		String generic = super.validate();
		StringBuffer summary = new StringBuffer();
		if( generic!=null ) summary.append(generic);
		
		String binding = timePathProperty.getBinding();
		if( binding==null || binding.length()==0 ) {
			summary.append(String.format("%s: binding is not configured\t",timePathProperty.getName()));
		}
		binding = valuePathProperty.getBinding();
		if( binding==null || binding.length()==0 ) {
			summary.append(String.format("%s: binding is not configured\t",valuePathProperty.getName()));
		}
		
		if( summary.length()==0 ) return null;
		else return summary.toString();
	}
}