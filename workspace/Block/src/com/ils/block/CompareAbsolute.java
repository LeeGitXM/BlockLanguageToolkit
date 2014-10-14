/**
r *   (c) 2014  ILS Automation. All rights reserved. 
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
import com.ils.blt.common.control.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class emits "true" if the absolute value of the "x" input is greater than or equal to
 * the absolute value of the "y". This block supports synchronization. Inputs and outputs 
 * are data values.
 */
@ExecutableBlock
public class CompareAbsolute extends Compare implements ProcessBlock {
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public CompareAbsolute() {
		super();
		initialize();
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public CompareAbsolute(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	
	/**
	 * The super-class initialize method is run first.
	 */
	private void initialize() {	
		setName("CompareAbsolute");
		
		anchors.clear();  // Re-define the anchors here
		// Define a two inputs -- one for the divisor, one for the dividend
		AnchorPrototype input = new AnchorPrototype(X_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("|x|");
		anchors.add(input);
		input = new AnchorPrototype(Y_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("|y|");
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	

	
	
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 * if it has changed.   )
	 */
	@Override
	public void evaluate() {
		if( !isLocked() ) {
			TruthValue tv = TruthValue.UNKNOWN;
			QualifiedValue result = null;
			if( x==null ) {
				result = new BasicQualifiedValue(tv,new BasicQuality("'x' is unset",Quality.Level.Bad));
			}
			else if( y==null ) {
				result = new BasicQualifiedValue(tv,new BasicQuality("'y' is unset",Quality.Level.Bad));
			}
			else if( !x.getQuality().isGood()) {
				result = new BasicQualifiedValue(tv,x.getQuality());
			}
			else if( !y.getQuality().isGood()) {
				result = new BasicQualifiedValue(tv,y.getQuality());
			}
			double xx = Double.NaN;
			double yy = Double.NaN;
			if( result == null ) {
				try {
					xx = Double.parseDouble(x.getValue().toString());
					try {
						yy = Double.parseDouble(y.getValue().toString());
					}
					catch(NumberFormatException nfe) {
						result = new BasicQualifiedValue(TruthValue.UNKNOWN,new BasicQuality("'y' is not a valid double",Quality.Level.Bad));
					}
				}
				catch(NumberFormatException nfe) {
					result = new BasicQualifiedValue(TruthValue.UNKNOWN,new BasicQuality("'x' is not a valid double",Quality.Level.Bad));
				}
			}
			
			if( result==null ) {     // Success!
				if( x.getQuality().isGood() && y.getQuality().isGood() ) {
					tv = TruthValue.FALSE;
					if( xx<0.0) xx = -xx;
					if( yy<0.0) yy = -yy;
					if( xx > yy+offset) tv = TruthValue.TRUE;
					result = new BasicQualifiedValue(tv);
				}
				else {
					Quality q = x.getQuality();
					if( q.isGood()) q = y.getQuality();
					result = new BasicQualifiedValue(tv,q);
					log.infof("%s.evaluate: UNKNOWN x=%s, y=%s",getName(),x.toString(),y.toString());
				}
				
			}
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
		}
	}
	
	/**
	 * Override the super-class method
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/compare_absolute.png");
		prototype.setPaletteLabel("CompareAbs");
		prototype.setTooltipText("Compare the absolute value of two inputs. Report true if the first is greater than or equal to the second.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ANALYSIS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/greater_equal.png");
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}

}