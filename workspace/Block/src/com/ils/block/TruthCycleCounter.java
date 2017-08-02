/**
 *   (c) 2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
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
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is a no-op. It simply passes its input onto the output.
 */
@ExecutableBlock
public class TruthCycleCounter extends AbstractProcessBlock implements ProcessBlock {
	private int initialValue = 0;
	private TruthValue trigger = TruthValue.TRUE; 
	private BlockProperty valueProperty = null;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public TruthCycleCounter() {
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
	public TruthCycleCounter(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * On a reset, propagate our initial value
	 * 
	 * NOTE: This has no effect on Python blocks. They must do this
	 * for themselves.
	 */
	@Override
	public void reset() {
		this.state = TruthValue.UNSET;
		this.lastValue = new TestAwareQualifiedValue(timer,new Integer(initialValue));
		recordActivity(Activity.ACTIVITY_RESET,"propagates initial value");
		valueProperty.setValue(new Integer(initialValue));
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus();
	}
	
	/**
	 * Initially transmit our initial value.
	 */
	@Override
	public void start() {
		super.start();
		if( valueProperty!=null  ) {
			lastValue = new TestAwareQualifiedValue(timer,new Integer(initialValue));
			log.debugf("%s.start: %s (%s)",getName(),lastValue.getValue().toString(),lastValue.getQuality().getName());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus();
		}
	}

	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("TruthCycleCounter");
		state = TruthValue.UNSET;
		delayStart = true;    // We transmit our initial value
		
		BlockProperty initialValueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INITIAL_VALUE,new Integer(initialValue),PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INITIAL_VALUE, initialValueProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.TRUTHVALUE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER, triggerProperty);
		// The value is the count-down shown in the UI
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,new Integer(initialValue),PropertyType.INTEGER,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setHint(PlacementHint.L);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setHint(PlacementHint.R);
		anchors.add(output);
	}
	

	/**
	 * A new value has appeared on our input.  Pass it on.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		String port = vcn.getConnection().getDownstreamPortName();
		QualifiedValue qv = vcn.getValue();
		if( qv!=null && qv.getValue()!=null ) {
			String key = vcn.getConnection().getSource().toString();
			recordActivity(Activity.ACTIVITY_RECEIVE,port,qv.getValue().toString(),key);
			if(!isLocked() ) {
				TruthValue incoming = qualifiedValueAsTruthValue(vcn.getValue());
				if( !this.state.equals(incoming) ) {
					this.state = incoming;
					if( this.state.equals(trigger) ) {
						int count = ((Integer)(valueProperty.getValue())).intValue();
						valueProperty.setValue(new Integer(count+1));
						//log.infof("%s.acceptValue: %s", getName(),qv.getValue().toString());
						lastValue = new TestAwareQualifiedValue(timer,new Integer(count+1));
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
						controller.acceptCompletionNotification(nvn);
						notifyOfStatus();
					}
				}
			}
		}
	}
	
	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.debugf("%s.propertyChange: Received %s = %s",getName(),propertyName,event.getNewValue().toString());
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_TRIGGER)) {
			trigger = TruthValue.valueOf(event.getNewValue().toString().toUpperCase());	
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_INITIAL_VALUE)) {
			try {
				initialValue = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert initial value to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",getName(),propertyName);
		}
	}


	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/PMIDigitalDisplay32.png");
		prototype.setPaletteLabel("TruthCounter");
		prototype.setTooltipText("Count the number of times that the trigger value has arrived");
		prototype.setTabName(BlockConstants.PALETTE_TAB_LOGIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setPreferredHeight(40);
		desc.setPreferredWidth(80);
		desc.setStyle(BlockStyle.READOUT);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_BLUE_GRAY);
	}
}