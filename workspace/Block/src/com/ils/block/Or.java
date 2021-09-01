/**
 *   (c) 2013-2019  ILS Automation. All rights reserved. 
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
import com.ils.blt.common.block.Activity;
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
import com.inductiveautomation.ignition.common.model.values.QualityCode;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * This class emits the "or" of its inputs. Synchronizing
 * is available. Inputs and outputs are truth-values.
 */
@ExecutableBlock
public class Or extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Or";
	// Keep map of values by originating block id
	protected final Map<String,QualifiedValue> qualifiedValueMap;
	private final Watchdog dog;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	private BlockProperty valueProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Or() {
		dog = new Watchdog(TAG,this);
		initialize();
		initializePrototype();
		qualifiedValueMap = new HashMap<>();
	}
	
	/**
	 * Constructor.
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent resource Id identifying the parent of this block (a diagram)
	 * @param block universally unique Id for the block
	 */
	public Or(ExecutionController ec,ProjectResourceId parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
		qualifiedValueMap = new HashMap<>();
	}
	
	private void initialize() {	
		setName("Or");	
		state = TruthValue.UNSET;
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,synchInterval,PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
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
	 * Initialize the qualified value map.
	 */
	@Override
	public void start() {
		super.start();
		reconcileQualifiedValueMap(BlockConstants.IN_PORT_NAME,qualifiedValueMap,TruthValue.UNSET);
	}
	/**
	 * Disconnect from the timer thread.
	 */
	@Override
	public void stop() {
		super.stop();
		timer.removeWatchdog(dog);
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
		String key = incoming.getConnection().getSource().toString();
		QualifiedValue qv = incoming.getValue();
		qualifiedValueMap.put(key, qv);
		dog.setSecondsDelay(synchInterval);
		timer.updateWatchdog(dog);  // pet dog
		recordActivity(Activity.ACTIVITY_RECEIVE,key,qv.getValue().toString());
	}
	
	/**
	 * On a TRUE or FALSE, concatenate the upstream reasons.
	 * 
	 * @return an explanation for the current state of the block.
	 */
	@Override
	public String getExplanation(DiagnosticDiagram parent,List<UUID> members) {
		String explanation = "";
		members.add(getBlockId());
		StringBuffer sb = new StringBuffer("(");
		int count = 0;
		if( state.equals(TruthValue.TRUE) ) {
			List<ProcessBlock>predecessors = parent.getUpstreamBlocks(this);
			for( ProcessBlock predecessor:predecessors ) {
				if( members.contains(predecessor.getBlockId())) {
					explanation = explanation + "-- truncated (circular reasoning)";
				}
				else if( predecessor.getState().equals(TruthValue.TRUE)) {
					count ++;
					if(sb.length()>1) sb.append(" or ");
					sb.append(predecessor.getExplanation(parent,members));
				}
			}
		}
		else if( state.equals(TruthValue.FALSE) ) {
			List<ProcessBlock>predecessors = parent.getUpstreamBlocks(this);
			for( ProcessBlock predecessor:predecessors ) {
				if( members.contains(predecessor.getBlockId())) {
					explanation = explanation + "-- truncated (circular reasoning)";
				}
				else if( predecessor.getState().equals(TruthValue.FALSE)) {
					count++;
					if(sb.length()>1) sb.append(" and ");
					sb.append(predecessor.getExplanation(parent,members));
				}
			}
		}
		if(count==1) explanation = sb.substring(1);    // Drop parenthesis
		else if(count>1) {
			sb.append(")");
			explanation = sb.toString();
		}
		return explanation;
	}
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 * if it has changed.
	 */
	@Override
	public void evaluate() {
		if( !isLocked() ) {
			TruthValue newState = getAggregateState();
			log.debugf("%s.evaluate: new = %s, old =%s",TAG,newState.name(),state.name());
			if(!newState.equals(state)) {
				setState(newState);  // Sets last value as side effect
				lastValue = new TestAwareQualifiedValue(timer,state.name(),
                        (state.equals(TruthValue.UNKNOWN)?getAggregateQuality():QualityCode.Good));
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
		for(String key:qualifiedValueMap.keySet()) {
			QualifiedValue qv = (QualifiedValue)qualifiedValueMap.get(key);
			if( qv!=null && qv.getValue()!=null) {
				attributes.put(key, String.valueOf(qv.getValue()));
			}
			else {
				attributes.put(key,"NULL"); 
			}
		}
		return descriptor;
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
				log.warnf("%s.propertyChange Unable to convert synch interval to a double (%s)",TAG,nfe.getLocalizedMessage());
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
	 * We are only concerned with the in port as it allows multiple connections
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
		prototype.setPaletteIconPath("Block/icons/palette/or.png");
		prototype.setPaletteLabel("Or");
		prototype.setTooltipText("Perform a logical 'or' of the inputs and place on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_LOGIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("OR");
		desc.setEmbeddedFontSize(14);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.LOGIC_OR);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	
	/**
	 * Compute the overall state, presumably because of a new input.
	 * This is an "or".
	 */
	private synchronized TruthValue getAggregateState() {
		Collection<QualifiedValue> values = qualifiedValueMap.values();
		
		boolean allUnset = true;
		boolean allFalse  = true;
		for(QualifiedValue qv:values) {
			TruthValue ts = qualifiedValueAsTruthValue(qv);
			if(qv.getQuality().isGood() && !ts.equals(TruthValue.UNSET) ) {
				allUnset = false;
				// Once we get a valid TRUE, it's all over
				if( ts.equals(TruthValue.TRUE) ) {
					return ts;
				}
				else if(!ts.equals(TruthValue.FALSE)) {
					allFalse = false;
				}	
			}
		}
		TruthValue result = TruthValue.UNKNOWN;
		if( allUnset ) result = TruthValue.UNSET;
		else if(allFalse) result = TruthValue.FALSE;
		return result;	
	}
	/**
	 * * Compute the overall quality.
	 * NOTE: This is only valid if the current state is UNKNOWN.
	 */
	private QualityCode getAggregateQuality() {
		Collection<QualifiedValue> values = qualifiedValueMap.values();
		QualityCode q = QualityCode.Good; 
		for(QualifiedValue qv:values) {
			if( !qv.getQuality().isGood() ) return qv.getQuality();
		}
		return q;	
	}
	
}