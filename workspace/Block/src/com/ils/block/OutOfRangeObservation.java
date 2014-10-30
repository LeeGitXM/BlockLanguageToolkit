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
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class compares input against a target range with deadband. If the value is not with a deadband
 * zone around the target, a TRUE value is emitted.
 */
@ExecutableBlock
public class OutOfRangeObservation extends AbstractProcessBlock implements ProcessBlock {
	private final static String TAG = "OutOfRangeObservation";
	private final static String BLOCK_PROPERTY_LOWER_DEADBAND = "LowerDeadband";
	private final static String BLOCK_PROPERTY_LOWER_LIMIT = "LowerLimit";
	private final static String BLOCK_PROPERTY_UPPER_DEADBAND = "UpperDeadband";
	private final static String BLOCK_PROPERTY_UPPER_LIMIT = "UpperLimit";
	private TruthValue truthValue = TruthValue.UNSET;
	private double lowerdeadband   = 0.;
	private double lowerlimit   = 0.;
	private double upperdeadband   = 0.;
	private double upperlimit   = 0.;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public OutOfRangeObservation() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "limit".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public OutOfRangeObservation(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	@Override
	public void reset() {
		super.reset();
		truthValue = TruthValue.UNSET;
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("OutOfRangeObservation");
		BlockProperty bp = new BlockProperty(BLOCK_PROPERTY_LOWER_LIMIT,new Double(lowerlimit),PropertyType.DOUBLE,true);
		properties.put(BLOCK_PROPERTY_LOWER_LIMIT, bp);
		bp = new BlockProperty(BLOCK_PROPERTY_UPPER_LIMIT,new Double(upperlimit),PropertyType.DOUBLE,true);
		properties.put(BLOCK_PROPERTY_UPPER_LIMIT, bp);
		bp = new BlockProperty(BLOCK_PROPERTY_LOWER_DEADBAND,new Double(lowerdeadband),PropertyType.DOUBLE,true);
		properties.put(BLOCK_PROPERTY_LOWER_DEADBAND, bp);
		bp = new BlockProperty(BLOCK_PROPERTY_UPPER_DEADBAND,new Double(upperdeadband),PropertyType.DOUBLE,true);
		properties.put(BLOCK_PROPERTY_UPPER_DEADBAND, bp);

		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	/**
	 * Accept notification that a value has arrived on an input
	 * @param vcn incoming notification
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		
		QualifiedValue qv = vcn.getValue();
		String val = qv.getValue().toString();
		try {
			double dbl = Double.parseDouble(val);
			TruthValue newValue = truthValue;
			if( dbl< upperlimit - upperdeadband && dbl>lowerlimit+lowerdeadband  ) newValue = TruthValue.FALSE;
			if( dbl> upperlimit || dbl<lowerlimit ) newValue = TruthValue.TRUE;
			if( !qv.getQuality().isGood()) newValue = TruthValue.UNKNOWN;
			if( !newValue.equals(truthValue)) {
				truthValue = newValue;
				if( !isLocked() ) {
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,new BasicQualifiedValue(truthValue,qv.getQuality(),qv.getTimestamp()));
					controller.acceptCompletionNotification(nvn);
				}
			}
		}
		catch(NumberFormatException nfe) {
			log.warnf("%s: setValue Unable to convert incoming value (%s) to a double (%s)",TAG,val,nfe.getLocalizedMessage());
		}

	}
	
	/**
	 * Handle a limit or limit type change.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BLOCK_PROPERTY_LOWER_DEADBAND)) {
			try {
				lowerdeadband = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert lower deadband to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equals(BLOCK_PROPERTY_UPPER_DEADBAND)) {
			try {
				upperdeadband = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert upper deadband to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equals(BLOCK_PROPERTY_LOWER_LIMIT)) {
			try {
				lowerlimit = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert lower limit to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equals(BLOCK_PROPERTY_UPPER_LIMIT)) {
			try {
				upperlimit = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert upper limit to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		
	}
	
	
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/out_of_range.png");
		prototype.setPaletteLabel("OutOfRange");
		prototype.setTooltipText("Test incoming value against configured high and low limits and deadbands");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/out_of_range_limit.png");
		desc.setEmbeddedFontSize(56);
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}