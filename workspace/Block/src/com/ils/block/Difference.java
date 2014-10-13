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
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.BlockPropertyChangeEvent;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.control.IncomingNotification;
import com.ils.blt.common.control.OutgoingNotification;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class emits the difference of its inputs. Synchronizing
 * is available. Inputs and outputs are data values.
 */
@ExecutableBlock
public class Difference extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Difference";
	protected static String A_PORT_NAME = "a";
	protected static String B_PORT_NAME = "b";
	private final Watchdog dog;
	private double synchInterval = 0.0; // default
	private QualifiedValue a = null;
	private QualifiedValue b = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Difference() {
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
	public Difference(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("Difference");
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
		// Define a two inputs -- one for the divisor, one for the dividend
		AnchorPrototype input = new AnchorPrototype(A_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("a");
		anchors.add(input);
		input = new AnchorPrototype(B_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("b");
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		a = null;
		b = null;
	}
	
	
	/**
	 * We are notified that a new value has appeared on one of our input anchors.
	 * For now we simply record the change in the map and start the watchdog.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		QualifiedValue qv = vcn.getValue();
		if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(A_PORT_NAME)) {
			if( qv!=null && qv.getValue()!=null ) {
				a = qv;
			}
			else {
				a = null;
			}
		}
		else if (vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(B_PORT_NAME)) {
			if( qv!=null && qv.getValue()!=null && qv.getQuality().isGood()) {
				b = qv;
			}
			else {
				b = null;
			}
		}
		dog.setSecondsDelay(synchInterval);
		controller.pet(dog);
	}
	
	
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 * if it has changed.
	 */
	@Override
	public void evaluate() {
		if( !isLocked() ) {
			QualifiedValue result = null;
			if( a==null ) {
				result = new BasicQualifiedValue(new Double(Double.NaN),new BasicQuality("'a' is unset",Quality.Level.Bad));
			}
			else if( b==null ) {
				result = new BasicQualifiedValue(new Double(Double.NaN),new BasicQuality("'b' is unset",Quality.Level.Bad));
			}
			else if( !a.getQuality().isGood()) {
				result = new BasicQualifiedValue(new Double(Double.NaN),a.getQuality());
			}
			else if( !b.getQuality().isGood()) {
				result = new BasicQualifiedValue(new Double(Double.NaN),b.getQuality());
			}
			double aa = Double.NaN;
			double bb = Double.NaN;
			if( result == null ) {
				try {
					aa = Double.parseDouble(a.getValue().toString());
					try {
						bb = Double.parseDouble(b.getValue().toString());
					}
					catch(NumberFormatException nfe) {
						result = new BasicQualifiedValue(new Double(Double.NaN),new BasicQuality("'b' is not a valid double",Quality.Level.Bad));
					}
				}
				catch(NumberFormatException nfe) {
					result = new BasicQualifiedValue(new Double(Double.NaN),new BasicQuality("'a' is not a valid double",Quality.Level.Bad));
				}
			}
			
			if( result==null ) {     // Success!
				
				result = new BasicQualifiedValue(new Double(aa-bb));
			}
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
		}
	}
	
	/**
	 * Handle a change to the coalescing interval.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL)) {
			try {
				synchInterval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert synch interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}

	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/difference.png");
		prototype.setPaletteLabel("Difference");
		prototype.setTooltipText("Subtract b from a and place result on the output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ARITHMETIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/minus.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY); 
	}
	
}