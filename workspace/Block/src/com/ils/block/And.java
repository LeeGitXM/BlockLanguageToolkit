/**
 *   (c) 2014-2016  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Collection;
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
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataQuality;

/**
 * This class emits the "and" of its inputs. Synchronizing
 * is available. Inputs and outputs are truth-values.
 */
@ExecutableBlock
public class And extends AbstractProcessBlock implements ProcessBlock {
	// Keep map of values by originating block id
	protected Map<String,QualifiedValue> qualifiedValueMap = null;
	private final Watchdog dog;
	private BlockProperty valueProperty = null;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public And() {
		initialize();
		initializePrototype();
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public And(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("And");
		state = TruthValue.UNSET;
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,TruthValue.UNKNOWN,PropertyType.TRUTHVALUE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input -- but allow multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
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
		qualifiedValueMap = initializeQualifiedValueMap(BlockConstants.IN_PORT_NAME);
		log.debugf("%s.start: initialized %d inputs",getName(),qualifiedValueMap.size());
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
		log.debugf("%s.acceptValue %s from %s",getName(),qv.getValue().toString(),key);
		qualifiedValueMap.put(key, qv);
		dog.setSecondsDelay(synchInterval);
		timer.updateWatchdog(dog);  // pet dog
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
					if(sb.length()>1) sb.append(" and ");
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
					if(sb.length()>1) sb.append(" or ");
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
		log.debugf("%s.evaluate",getName());
		if( !isLocked() ) {
			TruthValue newState = getAggregateState();
			log.debugf("%s.evaluate new: %s, old: %s",getName(),newState.name(),state.name());
			if(newState!=state) {
				state = newState;
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
	 * Handle a change to the coalescing interval.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL)) {
			try {
				synchInterval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert synch interval to a double (%s)",getName(),nfe.getLocalizedMessage());
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
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/and.png");
		prototype.setPaletteLabel("And");
		prototype.setTooltipText("And incoming values and place state changes on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_LOGIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("AND");
		desc.setEmbeddedFontSize(14);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.LOGIC_AND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	/**
	 * Compute the overall quality.
	 * NOTE: This is only valid if the current state is UNKNOWN.
	 */
	private Quality getAggregateQuality() {
		Collection<QualifiedValue> values = qualifiedValueMap.values();
		Quality q = DataQuality.GOOD_DATA; 
		for(QualifiedValue qv:values) {
			if( !qv.getQuality().isGood() ) return qv.getQuality();
		}
		return q;	
	}
	
	/**
	 * Compute the overall state, presumably because of a new input.
	 * This is an "and"
	 */
	private TruthValue getAggregateState() {
		Collection<QualifiedValue> values = qualifiedValueMap.values();
		TruthValue result = TruthValue.UNSET;
		
		for(QualifiedValue qv:values) {
			if(!qv.getQuality().isGood() ) {
				if(!result.equals(TruthValue.FALSE) ) result = TruthValue.UNKNOWN;
				continue;
			}
			// Once we get a valid FALSE, it's all over
			TruthValue ts = qualifiedValueAsTruthValue(qv);
			if( ts==TruthValue.FALSE ) {
				result = ts;
				break;
			}
			else if( ts.equals(TruthValue.TRUE) ) {
				if( !result.equals(TruthValue.UNKNOWN))  result = TruthValue.TRUE;
				continue;
			}
			else  {
				result = TruthValue.UNKNOWN;
				continue;
			}
		}
		if(result.equals(TruthValue.UNSET)) result = TruthValue.UNKNOWN;
		return result;	
	}
}