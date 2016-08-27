/**
r *   (c) 2014-2016  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class emits "true" if the value of the "x" input is greater than or equal to
 * the value of the "y". This block supports synchronization. Inputs and outputs 
 * are data values.
 */
@ExecutableBlock
public class CompareDeadband extends Compare implements ProcessBlock {
	private final String TAG = "CompareDeadband";
	private double deadband = 0.0;
	private TruthValue truthValue = TruthValue.UNSET;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public CompareDeadband() {
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
	public CompareDeadband(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	@Override
	public void reset() {
		super.reset();
		truthValue = TruthValue.UNSET;
	}
	
	/**
	 * The super-class method has already been run. Its anchors work for us as-is.
	 */
	private void initialize() {	
		setName("CompareDeadband");
		// Define the deadband
		BlockProperty oprop = new BlockProperty(BlockConstants.BLOCK_PROPERTY_DEADBAND,new Double(deadband),PropertyType.DOUBLE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_DEADBAND, oprop);
	}
	
	/**
	 * Augment the superclass method to include the deadband interval
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_DEADBAND)) {
			try {
				deadband = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert deadband to an double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}
	
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 * if it has changed.
	 */
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 */
	@Override
	public void evaluate() {
		log.tracef("%s.evaluate: %s,  x=%s, y=%s",getName(),truthValue.name(),(x==null?"null":x.toString()),(y==null?"null":y.toString()));
		state = TruthValue.UNKNOWN;
		QualifiedValue result = null;
		if( x==null ) {
			result = new TestAwareQualifiedValue(timer,state,new BasicQuality("'x' is unset",Quality.Level.Bad));
		}
		else if( y==null ) {
			result = new TestAwareQualifiedValue(timer,state,new BasicQuality("'y' is unset",Quality.Level.Bad));
		}
		else if( !x.getQuality().isGood()) {
			result = new TestAwareQualifiedValue(timer,state,x.getQuality());
		}
		else if( !y.getQuality().isGood()) {
			result = new TestAwareQualifiedValue(timer,state,y.getQuality());
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
					result = new TestAwareQualifiedValue(timer,TruthValue.UNKNOWN,new BasicQuality("'y' is not a valid double",Quality.Level.Bad));
				}
			}
			catch(NumberFormatException nfe) {
				result = new TestAwareQualifiedValue(timer,TruthValue.UNKNOWN,new BasicQuality("'x' is not a valid double",Quality.Level.Bad));
			}
		}

		if( result==null ) {     // Success!
			if( x.getQuality().isGood() && y.getQuality().isGood() ) {
				double db = deadband;
				if( db<0) db = -db;   // Abs value
				TruthValue newValue = TruthValue.UNKNOWN;
				if( xx>=yy+offset ) newValue = TruthValue.TRUE;
				else if( xx<yy+offset-db) newValue = TruthValue.FALSE;
				if( !newValue.equals(truthValue)) {
					truthValue = newValue;
					lastValue = new TestAwareQualifiedValue(timer,truthValue);
				}
				else {
					// No change, do nothing
					log.debugf("%s.evaluate: NO CHANGE (%s)",getName(),newValue.name());
					return;
				}
			}
			else {
				Quality q = x.getQuality();
				if( q.isGood()) q = y.getQuality();
				lastValue = new TestAwareQualifiedValue(timer,state,q);
			}

		}
		
		if( !isLocked() ) {
			log.debugf("%s.evaluate: wrote %s",getName(),result.getValue().toString());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);	
			notifyOfStatus(lastValue);
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,truthValue);
		notifyOfStatus(qv);
	}

	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
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
	 * Override the super-class method
	 */
	@Override
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/compare_deadband.png");
		prototype.setPaletteLabel("CompareDb");
		prototype.setTooltipText("Compare two inputs considering a deadband zone below the second.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ANALYSIS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/greater_equal_deadband.png");
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}