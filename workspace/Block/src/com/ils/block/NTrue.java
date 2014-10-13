/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.BlockPropertyChangeEvent;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.control.IncomingNotification;
import com.ils.blt.common.control.OutgoingNotification;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class tests if the number of true inputs is greater than a set value. Synchronizing
 * is available. Inputs and outputs are truth-values.
 */
@ExecutableBlock
public class NTrue extends AbstractProcessBlock implements ProcessBlock {
	private final static String TAG = "NTrue";
	private final static String BLOCK_PROPERTY_N_VALUE  = "NTrue";
	// Keep map of values by originating block id
	protected final Map<String,QualifiedValue> qualifiedValueMap;
	private final Watchdog dog;
	private double synchInterval = 0.0; // No synchronization by default
	protected TruthValue truthValue;
	protected Quality.Level truthQuality;
	protected int nTrue = 0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public NTrue() {
		dog = new Watchdog(TAG,this);
		qualifiedValueMap = new HashMap<String,QualifiedValue>();
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
	public NTrue(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		qualifiedValueMap = new HashMap<String,QualifiedValue>();
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("NTrue");
		truthValue = TruthValue.UNSET;
		truthQuality = Quality.Level.Good;
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		BlockProperty Nvalue = new BlockProperty(BLOCK_PROPERTY_N_VALUE, new Integer(nTrue),PropertyType.INTEGER, true);
		properties.put(BLOCK_PROPERTY_N_VALUE, Nvalue);
		BlockProperty valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,TruthValue.UNKNOWN,PropertyType.TRUTHVALUE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		properties.put(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input -- but allow multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		qualifiedValueMap.clear();
		truthValue = TruthValue.UNSET;
		truthQuality = Quality.Level.Good;
	}
	
	
	/**
	 * Notify the block that a new value has appeared on one of its input anchors.
	 * For now we simply record the change in the map and start the watchdog. 
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		String blockId = vcn.getConnection().getSource().toString();
		QualifiedValue qv = vcn.getValue();
		qualifiedValueMap.put(blockId, qv);
		dog.setSecondsDelay(synchInterval);
		controller.pet(dog);
	}
	
	
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 * if it has changed or its quality has changed.
	 */
	@Override
	public void evaluate() {
		if( !isLocked() ) {
			TruthValue newState = getAggregateState();
			log.infof("%s.evaluate: new = %s, old =%s",TAG,newState.name(),truthValue.name());
			Quality q = getAggregateQuality();
			if(newState!=truthValue || q.getLevel() != truthQuality) {
				truthValue = newState;
				truthQuality = q.getLevel();
				QualifiedValue result = new BasicQualifiedValue(truthValue.name(),q);
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
				controller.acceptCompletionNotification(nvn);
				controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,result);
			}
		}
	}
	
	/**
	 * Handle a change to the coalescing interval.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BLOCK_PROPERTY_N_VALUE)) {
			try {
				nTrue = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert nTrue to an integer (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL)) {
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
		prototype.setPaletteIconPath("Block/icons/palette/nTrue.png");
		prototype.setPaletteLabel("nTrue");
		prototype.setTooltipText("Determine if there are more true incoming values than a pre-determined constant.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_LOGIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("N");
		desc.setEmbeddedFontSize(18);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.LOGIC_NTRUE);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	/**
	 * Compute the overall quality.
	 */
	private Quality getAggregateQuality() {
		Collection<QualifiedValue> values = qualifiedValueMap.values();
		Quality q = null; 
		for(QualifiedValue qv:values) {
			if( q == null || !qv.getQuality().isGood() ) q = qv.getQuality();
		}
		return q;	
	}
	
	/**
	 * Compute the overall state, presumably because of a new input.
	 * This is an "nTrue"
	 */
	private TruthValue getAggregateState() {
		Collection<QualifiedValue> values = qualifiedValueMap.values();
		TruthValue result = TruthValue.UNSET;
		int trues = 0;
		
		for(QualifiedValue qv:values) {
			TruthValue tv = qualifiedValueAsTruthValue(qv);
			if( tv==TruthValue.TRUE ) {
				trues ++;
			}
			else if(tv.equals(TruthValue.UNKNOWN) || !qv.getQuality().isGood() ) {
				result = TruthValue.UNKNOWN;
			}
		}
		if (trues >= nTrue) result = TruthValue.TRUE;
		else if(result.equals(TruthValue.UNSET)) result = TruthValue.FALSE;
		return result;	
	}
}