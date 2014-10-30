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
 * This class compares input against an upper limit with deadband.
 */
@ExecutableBlock
public class HighLimitObservation extends AbstractProcessBlock implements ProcessBlock {
	private final static String TAG = "HighLimitObservation";
	private TruthValue truthValue = TruthValue.UNSET;
	private double deadband   = 0.;
	private double limit   = 0.;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public HighLimitObservation() {
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
	public HighLimitObservation(ExecutionController ec,UUID parent,UUID block) {
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
		setName("HighLimitObservation");
		BlockProperty bp = new BlockProperty(BlockConstants.BLOCK_PROPERTY_LIMIT,new Double(limit),PropertyType.DOUBLE,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_LIMIT, bp);
		bp = new BlockProperty(BlockConstants.BLOCK_PROPERTY_DEADBAND,new Double(deadband),PropertyType.DOUBLE,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_DEADBAND, bp);

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
		String port = vcn.getConnection().getDownstreamPortName();
		
		if( port.equalsIgnoreCase(BlockConstants.IN_PORT_NAME) ) {
			QualifiedValue qv = vcn.getValue();
			String val = qv.getValue().toString();
			try {
				double dbl = Double.parseDouble(val);
				TruthValue newValue = truthValue;
				if( dbl > limit   ) newValue = TruthValue.TRUE;
				if( dbl < limit - deadband ) newValue = TruthValue.FALSE;
				if( !qv.getQuality().isGood()) newValue = TruthValue.UNKNOWN;
				if( !newValue.equals(truthValue)) {
					truthValue = newValue;
					if( !isLocked() ) {
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,
								new BasicQualifiedValue(truthValue,qv.getQuality(),qv.getTimestamp()));
						controller.acceptCompletionNotification(nvn);
					}
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: setValue Unable to convert incoming value (%s) to a double (%s)",TAG,val,nfe.getLocalizedMessage());
			}
		}
	}
	
	/**
	 * Handle a limit or deadband change.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_DEADBAND)) {
			try {
				deadband = Double.parseDouble(event.getNewValue().toString());
				if( deadband<0.0) deadband = -deadband;
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert deadband to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_LIMIT)) {
			try {
				limit = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert limit to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		
	}
	/**
	 *  When unlocking, set the remembered state as "UNSET". This will allow
	 *  the next value to generate output, no matter what.
	 */
	@Override
	public void setLocked(boolean flag) {
		if(this.locked && !flag ) {
			truthValue = TruthValue.UNSET;
		}
		this.locked = flag;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/high_limit_observation.png");
		prototype.setPaletteLabel("HighLimit");
		prototype.setTooltipText("Test incoming value against a configured high limit and deadband");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/upper_limit.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}