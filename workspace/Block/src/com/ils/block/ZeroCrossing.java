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
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class checks consecutive inputs for a change in sign.
 */
@ExecutableBlock
public class ZeroCrossing extends AbstractProcessBlock implements ProcessBlock {
	private final static String TAG = "ZeroCrossing";
	private TruthValue truthValue = TruthValue.UNSET;
	private double lastValue = Double.NaN;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public ZeroCrossing() {
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
	public ZeroCrossing(ExecutionController ec,UUID parent,UUID block) {
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
		setName("ZeroCrossing");

		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	
	/**
	 * Accept notification that a value has arrived on an input.
	 * Conclude TRUE if the value has crossed zero.
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
				TruthValue result = TruthValue.UNKNOWN;
				if( qv.getQuality().isGood() )  {
					if (Double.isNaN(lastValue) ) {
						result = TruthValue.FALSE;
					}
					else if( dbl * lastValue >= 0.0 ) {
						result = TruthValue.FALSE;
					}
					else {
						result = TruthValue.TRUE;
					}
					
					if( dbl!= 0.0 ) lastValue = dbl;
				}
				
				
				if( !result.equals(truthValue)) {
					truthValue = result;
					if( !isLocked() ) {
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,
								new BasicQualifiedValue(truthValue,qv.getQuality(),qv.getTimestamp()));
						controller.acceptCompletionNotification(nvn);
					}
				}
				if (dbl != 0){
					lastValue = dbl;
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: setValue Unable to convert incoming value (%s) to a double (%s)",TAG,val,nfe.getLocalizedMessage());
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
		prototype.setPaletteIconPath("Block/icons/palette/zero_crossing.png");
		prototype.setPaletteLabel("ZeroCrossing");
		prototype.setTooltipText("Test incoming value for a sign change");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/zero_crossing.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}