/**
 *   (c) 2014-2019  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
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
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataQuality;

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
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	protected int nTrue = 0;
	private BlockProperty valueProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public NTrue() {
		dog = new Watchdog(TAG,this);
		qualifiedValueMap = new HashMap<>();
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
		qualifiedValueMap = new HashMap<>();
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("NTrue");
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		BlockProperty Nvalue = new BlockProperty(BLOCK_PROPERTY_N_VALUE, new Integer(nTrue),PropertyType.INTEGER, true);
		setProperty(BLOCK_PROPERTY_N_VALUE, Nvalue);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,TruthValue.UNSET,PropertyType.TRUTHVALUE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input -- but allow multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setIsMultiple(true);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	
	/**
	 * Notify the block that a new value has appeared on one of its input anchors.
	 * For now we simply record the change in the map and start the watchdog. 
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		String key = String.format("%s:%s",incoming.getConnection().getSource().toString(),
                                           incoming.getConnection().getUpstreamPortName());
		QualifiedValue qv = incoming.getValue();
		qualifiedValueMap.put(key, qv);
		dog.setSecondsDelay(synchInterval);
		timer.updateWatchdog(dog);  // pet dog
	}
	
	/**
	 * The explanation for this block is simply the current count of current 
	 * TRUE inputs.
	 * 
	 * @return an explanation for the current state of the block.
	 *         By default it is the concatenated explanations of all 
	 *         upstream blocks with the same state.
	 *         If this is a block that has no relevant state, return
	 *         an empty string.
	 */
	@Override
	public String getExplanation(DiagnosticDiagram parent,List<UUID> members) {
		String explanation = "";
		if( state.equals(TruthValue.TRUE) || state.equals(TruthValue.FALSE)) {
			Collection<QualifiedValue> values = qualifiedValueMap.values();
			int trues = 0;
			for(QualifiedValue qv:values) {
				if(qv.getQuality().isGood()) {
					TruthValue tv = qualifiedValueAsTruthValue(qv);
					if( tv.equals(TruthValue.TRUE )) {
						trues ++;
					}
				}
			}
			explanation = String.format("At %s, %s inputs are true (%d needed)",getName(),trues,nTrue);
		}
		return explanation;
	}
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 * if it has changed or its quality has changed.
	 */
	@Override
	public void evaluate() {
		if( !isLocked() ) {
			TruthValue newState = getAggregateState();
			log.tracef("%s.evaluate: new = %s, old =%s",TAG,newState.name(),state.name());
			if(!newState.equals(state)) {
				setState(newState); 
				lastValue = new TestAwareQualifiedValue(timer,state.name(),
			            (state.equals(TruthValue.UNKNOWN)?getAggregateQuality():DataQuality.GOOD_DATA));
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				valueProperty.setValue(state);
				notifyOfStatus(lastValue);
			}
		}
	}
	
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Value", state.name());
		return descriptor;
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
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,state);
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}

	@Override
	public void setState(TruthValue newState) { 
		super.setState(newState);
		valueProperty.setValue(newState);
	}
	/**
	 * On a save, make sure that our map of connections is proper. 
	 * We are only concerned with the in port that allows multiple connections.
	 */
	@Override
	public void validateConnections() {
		reconcileQualifiedValueMap(BlockConstants.IN_PORT_NAME,qualifiedValueMap,TruthValue.UNKNOWN);
		evaluate();
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
		desc.setEmbeddedFontSize(14);
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
		int falses = 0;
		
		pruneInitialConnections(qualifiedValueMap);

//		Collection<String> keys = qualifiedValueMap.keySet();
//		for(String str:keys ) {
//			log.errorf("%s.evaluate EREIAM JH - Key spinner: %s",TAG,str);
//		}
		
		for(QualifiedValue qv:values) {
			if(qv.getQuality().isGood()) {
				TruthValue tv = qualifiedValueAsTruthValue(qv);
//				log.errorf("%s.evaluate EREIAM JH - Truth spinner: %s",TAG,tv.name());
				if( tv.equals(TruthValue.TRUE) ) {
					trues ++;
				} else {
					if(tv.equals(TruthValue.FALSE)) {
						falses++;
					}
				}
			}
		}
		int inputs = values.size();
		int unknowns = inputs - (trues + falses);
		if (trues >= nTrue) {
			result = TruthValue.TRUE;
		} else {
			if((nTrue <= trues + unknowns) && (trues > 0 || unknowns == inputs)) {
				result = TruthValue.UNKNOWN;
			} else {
				result = TruthValue.FALSE;
			}
		}
//		log.errorf("%s.evaluate EREIAM JH - T=%d,F=%d,U=%d of %d, need %d => %s",TAG,trues,falses,unknowns,values.size(),nTrue,result.name());
		
		log.debugf("%s.evaluate T=%d,F=%d of %d, need %d => %s",TAG,trues,falses,values.size(),nTrue,result.name());
		return result;	
	}
}