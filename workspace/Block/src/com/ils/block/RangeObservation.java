/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.List;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class compares input against a target range with deadband. If the value is not with a deadband
 * zone around the target, a TRUE value is emitted.
 */
@ExecutableBlock
public class RangeObservation extends AbstractProcessBlock implements ProcessBlock {
	private final static String TAG = "RangeObservation";
	private final static String BLOCK_PROPERTY_LOWER_DEADBAND = "LowerDeadband";
	private final static String BLOCK_PROPERTY_LOWER_LIMIT = "LowerLimit";
	private final static String BLOCK_PROPERTY_UPPER_DEADBAND = "UpperDeadband";
	private final static String BLOCK_PROPERTY_UPPER_LIMIT = "UpperLimit";
	private double lowerdeadband   = 0.;
	private double lowerlimit   = 0.;
	private double upperdeadband   = 0.;
	private double upperlimit   = 0.;
	private QualifiedValue observation = null;    // Most recent value
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public RangeObservation() {
		initialize();
		initializePrototype();
		observation = null;
	}
	
	/**
	 * Constructor. Custom property is "limit".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public RangeObservation(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	@Override
	public void reset() {
		super.reset();
		state = TruthValue.UNKNOWN;
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("RangeObservation");
		BlockProperty bp = new BlockProperty(BLOCK_PROPERTY_LOWER_LIMIT,new Double(lowerlimit),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_LOWER_LIMIT, bp);
		bp = new BlockProperty(BLOCK_PROPERTY_UPPER_LIMIT,new Double(upperlimit),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_UPPER_LIMIT, bp);
		bp = new BlockProperty(BLOCK_PROPERTY_LOWER_DEADBAND,new Double(lowerdeadband),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_LOWER_DEADBAND, bp);
		bp = new BlockProperty(BLOCK_PROPERTY_UPPER_DEADBAND,new Double(upperdeadband),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_UPPER_DEADBAND, bp);

		
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
		
		observation = vcn.getValue();
		String val = observation.getValue().toString();
		try {
			double dbl = Double.parseDouble(val);
			TruthValue newValue = state;
			if( dbl< upperlimit - upperdeadband && dbl>lowerlimit+lowerdeadband  ) newValue = TruthValue.TRUE;
			if( dbl> upperlimit|| dbl<lowerlimit ) newValue = TruthValue.FALSE;
			if( !observation.getQuality().isGood()) newValue = TruthValue.UNKNOWN;
			if( !newValue.equals(state)) {
				state = newValue;
				if( !isLocked() ) {
					QualifiedValue nqv = new BasicQualifiedValue(state,observation.getQuality(),observation.getTimestamp());
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,nqv);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(nqv);
				}
			}
		}
		catch(NumberFormatException nfe) {
			log.warnf("%s: setValue Unable to convert incoming value (%s) to a double (%s)",TAG,val,nfe.getLocalizedMessage());
		}

	}
	/**
	 * The explanation for this block just reports the observation status
	 * 
	 * @return an explanation for the current state of the block.
	 */
	@Override
	public String getExplanation(DiagnosticDiagram parent,List<UUID> members) {
		String explanation = "";
		if( observation!=null && state.equals(TruthValue.TRUE) ) {
			explanation = String.format("At %s, %s is not within limits (%3.2f,%3.2f)",getName(),
														observation.getValue().toString(),lowerlimit,upperlimit);
		}
		else if( observation!=null && state.equals(TruthValue.FALSE)) {
			explanation = String.format("At %s, %s is within limits (%3.2f,%3.2f)",getName(),
														observation.getValue().toString(),lowerlimit,upperlimit);
		}
		return explanation;
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
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,state);
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/range_observation.png");
		prototype.setPaletteLabel("Range");
		prototype.setTooltipText("Test incoming value against configured high and low limits and deadbands");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/range_limit.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}