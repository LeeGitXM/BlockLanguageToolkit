/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
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
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;

/**
 * Propagate a set (or bounded) value on a configured interval.
 */
@ExecutableBlock
public class DataPump extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "DataPump";
	private final Watchdog dog;
	private Double interval = 60.;
	private Double value = Double.NaN;
	
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
			try {
				value = Double.parseDouble(event.getNewValue().toString());
				if( !dog.isActive() && !Double.isNaN(interval) ) {
					dog.setSecondsDelay(interval);
					controller.pet(dog);
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert kd value to a float (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_INTERVAL)) {
			try {
				interval = Double.parseDouble(event.getNewValue().toString());
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
		if( Double.isNaN(value) || Double.isNaN(interval)) return;   // Stops watchdog
		dog.setSecondsDelay(interval);
		controller.pet(dog);
		
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,new BasicQualifiedValue(value));
		controller.acceptCompletionNotification(nvn);
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("DataPump");
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL,new Double(interval),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_INTERVAL, intervalProperty);
		BlockProperty valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,new Double(value),PropertyType.DOUBLE,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
		
		// Start the pump
		if( !Double.isNaN(value) && !Double.isNaN(interval)) {
			dog.setSecondsDelay(interval);
			controller.pet(dog);
		}
		
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
}