/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Date;
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
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class emits "true" if the "x" input is greater than or equal to the "y". This block
 * supports synchronization. Inputs and outputs are data values.
 */
@ExecutableBlock
public class Compare extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Compare";
	protected QualifiedValue currentValue = null;
	
	protected static String X_PORT_NAME = "x";
	protected static String Y_PORT_NAME = "y";
	protected static String OUT_PORT_NAME = "out";
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	// Keep map of values by originating block id
	protected QualifiedValue x = null;
	protected QualifiedValue y = null;
	protected final Watchdog dog;
	protected double offset = 0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Compare() {
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
	public Compare(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
	}
	
	/**
	 * Offset is ...
	 */
	private void initialize() {		
		setName("Compare");
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty oprop = new BlockProperty(BlockConstants.BLOCK_PROPERTY_OFFSET,new Double(offset),PropertyType.DOUBLE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_OFFSET, oprop);
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
		// Define two inputs 
		AnchorPrototype input = new AnchorPrototype(X_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		input.setAnnotation("x");
		anchors.add(input);
		input = new AnchorPrototype(Y_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		input.setAnnotation("y");
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	@Override
	public void reset() {
		super.reset();
		state = TruthValue.UNKNOWN;
	}
	
	

	/**
	 * We are notified that a new value has appeared on one of our input anchors
	 * For now we simply record the change in the map and start the watchdog.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();
		if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(X_PORT_NAME)) {
			if( qv!=null && qv.getValue()!=null ) {
				x = qv;
			}
			else {
				x = null;
			}
		}
		else if (vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(Y_PORT_NAME)) {
			if( qv!=null && qv.getValue()!=null && qv.getQuality().isGood()) {
				y = qv;
			}
			else {
				y = null;
			}
		}
		dog.setSecondsDelay(synchInterval);
		timer.updateWatchdog(dog);  // pet dog
	}
	
	
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 */
	@Override
	public void evaluate() {
		if( !isLocked() ) {
			state = TruthValue.UNKNOWN;
			currentValue = null;
			if( x==null ) {
				currentValue = new TestAwareQualifiedValue(timer,state,new BasicQuality("'x' is unset",Quality.Level.Bad));
			}
			else if( y==null ) {
				currentValue = new TestAwareQualifiedValue(timer,state,new BasicQuality("'y' is unset",Quality.Level.Bad));
			}
			else if( !x.getQuality().isGood()) {
				currentValue = new TestAwareQualifiedValue(timer,state,x.getQuality());
			}
			else if( !y.getQuality().isGood()) {
				currentValue = new TestAwareQualifiedValue(timer,state,y.getQuality());
			}
			double xx = Double.NaN;
			double yy = Double.NaN;
			if( currentValue == null ) {
				try {
					// Handle dates
					if( x.getValue() instanceof Date || y.getValue() instanceof Date ) {
						if( !(x.getValue() instanceof Date) || !(y.getValue() instanceof Date)  ) {
							currentValue = new TestAwareQualifiedValue(timer,TruthValue.UNKNOWN,new BasicQuality("If one input is a Date, then both must be",Quality.Level.Bad));
						}
						else {
							xx = ((Date)(x.getValue())).getTime();
							yy = ((Date)(y.getValue())).getTime();
						}
					}
					else {
						xx = Double.parseDouble(x.getValue().toString());
						try {
							yy = Double.parseDouble(y.getValue().toString());
						}
						catch(NumberFormatException nfe) {
							currentValue = new TestAwareQualifiedValue(timer,TruthValue.UNKNOWN,new BasicQuality("'y' is not a valid double",Quality.Level.Bad));
						}
					}
				}
				catch(NumberFormatException nfe) {
					currentValue = new TestAwareQualifiedValue(timer,TruthValue.UNKNOWN,new BasicQuality("'x' is not a valid double",Quality.Level.Bad));
				}
			}
			
			if( currentValue==null ) {     // Success!
				if( x.getQuality().isGood() && y.getQuality().isGood() ) {
					state = TruthValue.FALSE;
					if( xx > yy+offset)state = TruthValue.TRUE;
					currentValue = new TestAwareQualifiedValue(timer,state);
				}
				else {
					Quality q = x.getQuality();
					if( q.isGood()) q = y.getQuality();
					currentValue = new TestAwareQualifiedValue(timer,state,q);
					log.infof("%s.evaluate: UNKNOWN x=%s, y=%s",getName(),x.toString(),y.toString());
				}
				
			}
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,currentValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(currentValue);
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(currentValue);
	}

	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * Handle a change to the coalescing interval.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_OFFSET)) {
			try {
				offset = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert offset to an double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL)) {
			try {
				synchInterval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert synch interval to an double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/compare.png");
		prototype.setPaletteLabel("Compare");
		prototype.setTooltipText("Compare the value of two inputs. Report true if the first is greater than or equal to the second.");
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