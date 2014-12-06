/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
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
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Propagate a set (or bounded) value on a configured interval.
 */
@ExecutableBlock
public class DataPump extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "DataPump";
	private final Watchdog dog;
	private Double interval = Double.NaN;      // No interval by Default
	private Object value = "";
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public DataPump() {
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
	public DataPump(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
	}

	
	@Override
	public void reset() {
		super.reset();
		controller.removeWatchdog(dog);    // Stop current cycle.
		if( !Double.isNaN(interval) && interval>0.0 ) {
			dog.setSecondsDelay(interval);
			controller.pet(dog);
		}
	}
	/**
	 * Disconnect from the timer thread.
	 */
	@Override
	public void stop() {
		super.stop();
		controller.removeWatchdog(dog);
	}
	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.infof("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_VALUE)) {
			
			value = event.getNewValue();
			if( !dog.isActive() && !Double.isNaN(interval) && interval>0.0) {
				dog.setSecondsDelay(interval);
				controller.pet(dog);
			}
			// If the interval is zero, we propagate the value immediately. Coerce to match output connection type
			else {
				value = coerceToMatchOutput(value);
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,new BasicQualifiedValue(value));
				controller.acceptCompletionNotification(nvn);
			}
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_INTERVAL)) {
			try {
				interval = Double.parseDouble(event.getNewValue().toString());
				// Start the pump
				if( interval > 0.0 ) {
					dog.setSecondsDelay(interval);
					controller.pet(dog);
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}
	
	/**
	 * The interval has expired. Reset interval, then emit the value on the output
	 */
	@Override
	public synchronized void evaluate() {
		if( Double.isNaN(interval) || interval<=0.0 ) return;   // Stops watchdog
		dog.setSecondsDelay(interval);
		controller.pet(dog);
		
		// Coerce the value to match the output
		if( !anchors.isEmpty()) {   // There should be exactly one, get its type.
			AnchorPrototype ap = anchors.get(0);
			log.infof("%s.evaluate: proto type = %s",TAG,ap.getConnectionType());
		}
		value = coerceToMatchOutput(value);
		QualifiedValue qv = new BasicQualifiedValue(value);
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus(qv);
	}
	
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		if( !anchors.isEmpty()) {   // There should be exactly one, get its type.
			AnchorPrototype ap = anchors.get(0);
			attributes.put("ConnectionType", ap.getConnectionType().name());
		}
		return descriptor;
	}
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("DataPump");
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL,new Double(interval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL, intervalProperty);
		BlockProperty valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,value,PropertyType.OBJECT,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
		
		// Start the pump
		if( !Double.isNaN(interval) && interval > 0.0) {
			dog.setSecondsDelay(interval);
			controller.pet(dog);
		}
	}
	/**
	 * Send status update notification for our last output value.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new BasicQualifiedValue(value);
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/timed_source.png");
		prototype.setPaletteLabel("DataPump");
		prototype.setTooltipText("Write the incoming value to a pre-configured tag");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.ARROW);
		desc.setEmbeddedIcon("Block/icons/embedded/alarm_clock.png");
		desc.setPreferredHeight(45);
		desc.setPreferredWidth(60);
		desc.setBackground(new Color(125,240,230).getRGB());   // Dark Green
		desc.setCtypeEditable(true);
	}
	
	private Object coerceToMatchOutput(Object val) {
		// Coerce the value to match the output
		if( !anchors.isEmpty() && val!=null && val.toString().length()>0 ) { 
			AnchorPrototype ap = anchors.get(0);  // There should be exactly one anchor, get its type.
			log.infof("%s.evaluate: %s type = %s",TAG,(val==null?"null":val.toString()),ap.getConnectionType());
			if( ConnectionType.DATA.equals(ap.getConnectionType()))  {
				val = new Double(fcns.coerceToDouble(val));
			}
			else if( ConnectionType.TRUTHVALUE.equals(ap.getConnectionType())) {
				boolean flag = fcns.coerceToBoolean(val);
				if( flag ) val = TruthValue.TRUE;
				else val = TruthValue.FALSE;
			}
			else val = val.toString();
		}
		return val;
	}
}