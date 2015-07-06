/**
 *   (c) 2013-2015  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.AbstractBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.BlockPropertyChangeListener;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.watchdog.WatchdogObserver;
import com.ils.common.watchdog.WatchdogTimer;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;


/**
 * This abstract class is the base of all blocks. It cannot in itself
 * be instantiated. 
 *  
 * The subclasses depend on the "ExecutableBlock" class annotation
 * as the signal to group a particular subclass into the list of 
 * available executable block types.
 */
public abstract class AbstractProcessBlock extends AbstractBlock implements ProcessBlock, BlockPropertyChangeListener, WatchdogObserver {
	// These are the qualified values sent on a reset
	private final static QualifiedValue UNKNOWN_TRUTH_VALUE = new BasicQualifiedValue(TruthValue.UNKNOWN);
	private final static QualifiedValue NAN_DATA_VALUE = new BasicQualifiedValue(new Double(Double.NaN));
	private final static QualifiedValue EMPTY_STRING_VALUE = new BasicQualifiedValue("");
	private GeneralPurposeDataContainer auxiliaryData = null;
	protected String statusText;
	protected boolean delayStart = false;
	protected boolean locked     = false;
	protected boolean isReceiver = false;
	protected boolean isTransmitter = false;
	protected boolean running = false;
	protected TruthValue state = TruthValue.UNSET;
	protected WatchdogTimer timer = null;

	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 *              It does not correspond to a functioning block.
	 */
	public AbstractProcessBlock() {
		initializePrototype();
		initialize();
	}
	
	/**
	 * Constructor: Use this version to create a block that correlates to a block in the diagram.
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block. The id may be null for blocks that are "unattached"
	 * @param block universally unique Id for the block
	 */
	public AbstractProcessBlock(ExecutionController ec, UUID parent, UUID block) {
		super(ec,parent,block);
	}
	
	/**
	 * Create an initial list of properties. There are none for the base class.
	 * We also add a stub for signals. Every block has this connection, but,
	 * by default, it is hidden.
	 */
	private void initialize() {
		this.state = TruthValue.UNSET;
		AnchorPrototype sig = new AnchorPrototype(BlockConstants.SIGNAL_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.SIGNAL);
		sig.setHidden(true);
		anchors.add(sig);
	}
	
	/**
	 * Fill a prototype object with defaults - as much as is reasonable.
	 */
	private void initializePrototype() {
		prototype.getBlockDescriptor().setReceiveEnabled(isReceiver);
		prototype.getBlockDescriptor().setTransmitEnabled(isTransmitter);
	}
	
	/**
	 * Place a value on the named output port without disrupting
	 * the current state of the block. Coerce the value based on the
	 * connection type.
	 */
	@Override
	public void forcePost(String port,String sval) {
		for( AnchorPrototype ap:anchors) {
			if( ap.getName().equalsIgnoreCase(port)) {
				ConnectionType ct = ap.getConnectionType();
				Object value = sval;
				try {
					if( ct.equals(ConnectionType.TRUTHVALUE) ) value = TruthValue.valueOf(sval.toUpperCase());
					else if(ct.equals(ConnectionType.DATA)   ) value = Double.parseDouble(sval);
					else if(ct.equals(ConnectionType.SIGNAL) ) value = new Signal(sval,"","");
				}
				catch( NumberFormatException nfe) {
					log.warnf("%s.forcePost: Unable to coerce %s to %s (%s)",getName(),sval,ct.name(),nfe.getLocalizedMessage());
				}
				catch( IllegalArgumentException iae) {
					log.warnf("%s.forcePost: Unable to coerce %s to %s (%s)",getName(),sval,ct.name(),iae.getLocalizedMessage());
				}

				OutgoingNotification nvn = new OutgoingNotification(this,port,new BasicQualifiedValue(value));
				controller.acceptCompletionNotification(nvn);
			}
		}
	}
	@Override
	public boolean delayBlockStart() { return this.delayStart; }
	@Override
	public List<AnchorPrototype>getAnchors() { return anchors; }
	public GeneralPurposeDataContainer getAuxiliaryData() {return auxiliaryData;}
	public void setAuxiliaryData(GeneralPurposeDataContainer auxiliaryData) {this.auxiliaryData = auxiliaryData;}
	@Override
	public PalettePrototype getBlockPrototype() {return prototype; }
	@Override
	public TruthValue getState() {return state;}
	@Override
	public void setState(TruthValue state) { if(state!=null) this.state = state; }

	@Override
	public String getStatusText() {return statusText;}
	@Override
	public void setStatusText(String statusText) {this.statusText = statusText;}
	/**
	 * @param t the new timer. Timer start/stop is managed by the controller.
	 */
	@Override
	public void setTimer(WatchdogTimer t) { 
		this.timer = t; 
	}
	
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = new SerializableBlockStateDescriptor();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Name", getName());
		attributes.put("UUID", getBlockId().toString());
		attributes.put("State", getState().toString());
		return descriptor;
	}
	/**
	 * @return all properties. The returned array is a copy of the internal.
	 * Thus although the attributes of an individual property can be modified,
	 * the makeup of the set cannot.
	 * @return properties an array of the properties of the block.
	 */
	public BlockProperty[] getProperties() {
		 Collection<BlockProperty> propertyList = propertyMap.values();
		 BlockProperty[] results = new BlockProperty[propertyList.size()];
		 int index=0;
		 for(BlockProperty bp:propertyList ) {
			 results[index]=bp;
			 index++;
		 }
		 return results;
	}
	
	/**
	 * @return a list of the attribute names required by this class.
	 */
	@Override
	public Set<String> getPropertyNames() {
		return propertyMap.keySet();
	}
	
	protected void setProperty(String nam,BlockProperty prop) { propertyMap.put(nam, prop); }
	
	@Override
	public void setAnchors(List<AnchorPrototype> protos) {
		if( protos.size()>0 ) {
			this.anchors = protos; 
			BlockDescriptor blockDescriptor = prototype.getBlockDescriptor();
			blockDescriptor.setAnchors(anchors);
		}
	}
	@Override
	public boolean isLocked() {return locked;}
	/**
	 * When unlocking we set the state to UNSET to force
	 * an output next evaluation.
	 */
	@Override
	public void setLocked(boolean locked) {
		if(this.locked && !locked) this.state = TruthValue.UNSET;
		this.locked = locked;
		
	}
	@Override
	public boolean isReceiver() { return isReceiver; }
	@Override
	public boolean isTransmitter() { return isTransmitter; }
	
	/**
	 * The default method sets the state to INITIALIZED.
	 * It also sends notifications to block outputs setting them to empty or
	 * unknown. NOTE: This has no effect on Python blocks. They must do this
	 * for themselves.
	 */
	@Override
	public void reset() {
		this.state = TruthValue.UNSET;
		if( controller!=null ) {
			// Send notifications on all outputs to indicate empty connections.
			// For truth-values, actually propagate UNKNOWN.
			for(AnchorPrototype ap:getAnchors()) {
				if( ap.getAnchorDirection().equals(AnchorDirection.OUTGOING) ) {
					if( ap.getConnectionType().equals(ConnectionType.TRUTHVALUE)) {
						controller.sendConnectionNotification(getBlockId().toString(), ap.getName(),UNKNOWN_TRUTH_VALUE);
						OutgoingNotification nvn = new OutgoingNotification(this,ap.getName(),UNKNOWN_TRUTH_VALUE);
						controller.acceptCompletionNotification(nvn);
					}
					else if( ap.getConnectionType().equals(ConnectionType.DATA)) {
						controller.sendConnectionNotification(getBlockId().toString(), ap.getName(),NAN_DATA_VALUE);
					}
					else {
						controller.sendConnectionNotification(getBlockId().toString(), ap.getName(),EMPTY_STRING_VALUE);
					}
				}
			}
		}
	}
	/**
	 * Accept a new value for a block property. In general this does not trigger
	 * block evaluation. Use the property change listener interface to do so.
	 * 
	 * @param name of the property to update
	 * @param value new value of the property
	 */
	@Override
	public void setProperty(String name,Object value) {
		BlockProperty prop = getProperty(name);
		if( prop!=null && value!=null ) {
			prop.setValue(value);
		}
	}
	/**
	 * The block is notified that a new value has appeared on one of its input anchors.
	 * The base implementation simply logs the value.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn notification of the new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		checkIncomingValue(vcn );
		if( log.isDebugEnabled()) {
			// An input from a TAG_READ bound property does not have a source
			if( vcn.getConnection()!=null ) {
				QualifiedValue qv = vcn.getValue();
				if( qv!=null && qv.getValue()!=null ) {
					log.debugf("%s.acceptValue: %s (%s) port: %s",getName(),
							qv.getValue().toString(),
							qv.getQuality().getName(),
							vcn.getConnection().getDownstreamPortName());

				}
			}
		}
	}
	/**
	 * The block is notified that signal has been sent to it.
	 * The base implementation handles to universal commands:
	 *     reset
	 *     lock/unlock
	 *     evaluate
	 * 
	 * @param sn notification of a signal.
	 */
	@Override
	public void acceptValue(SignalNotification sn) {
		Signal sig = sn.getSignal();
		if( sig.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_EVALUATE) ) {
			evaluate();
		}
		else if( sig.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_LOCK) ) {
			setLocked(true);
		}
		else if( sig.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_RESET) ) {
			reset();
		}
		else if( sig.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_UNLOCK) ) {
			setLocked(false);
		}
	}
	/**
	 * Send status update notifications for any properties
	 * or output connections known to the designer. This
	 * basic implementation reports all values bound to ENGINE.
	 * 
	 * It is expected that most blocks will implement this in
	 * a more efficient way.
	 */
	@Override
	public void notifyOfStatus() {
		for( BlockProperty bp:getProperties()) {
			if( bp.getBindingType().equals(BindingType.ENGINE) ) {
				QualifiedValue qv = new BasicQualifiedValue(bp.getValue());
				controller.sendPropertyNotification(getBlockId().toString(),bp.getName(), qv);
			}
		}
	}
	/**
	 * Start any active monitoring or processing within the block.
	 * This default method does nothing. In general, a start does
	 * NOT reset state in a block that is already running.
	 */
	@Override
	public void start() { this.running = true;}
	/**
	 * Terminate any active operations within the block.
	 * This default method does nothing.
	 */
	@Override
	public void stop() {
		this.running = false;
		state = TruthValue.UNSET;
	}
	
	/**
	 * In the case where the block has specified a coalescing time, this method
	 * will be called by the engine after receipt of input once the coalescing 
	 * "quiet" time has passed without further input.
	 * 
	 * The default implementation is appropriate for blocks that trigger calculation
	 * on every update of the inputs. It does nothing.
	 */
	public void evaluate() {}
	
	// =================================  Convenience Methods   ================================
	protected TruthValue qualifiedValueAsTruthValue(QualifiedValue qv) {
		TruthValue result = TruthValue.UNSET;
		Object value = qv.getValue();
		if( value instanceof TruthValue ) {
			result = (TruthValue) value;
		}
		else if(value instanceof Boolean) {
			if( ((Boolean)value).booleanValue() ) result = TruthValue.TRUE;
			else result = TruthValue.FALSE;
		}
		else if(value instanceof String) {
			try {
				result = TruthValue.valueOf(value.toString().toUpperCase());
			}
			catch( IllegalArgumentException iae) {
				log.warnf("%s.qualifiedValueAsTruthValue: Exception converting %s (%s)", getName(),value.toString(),iae.getLocalizedMessage());
			}
		}
		return result;
	}
	// Assuming single output, force the data to match,
	protected Object coerceToMatchOutput(String port,Object val) {
		// Coerce the value to match the output
		if( !anchors.isEmpty() && val!=null && val.toString().length()>0 ) { 
			for(AnchorPrototype ap: anchors) {
				if(ap.getName().equals(port)) {
					log.infof("%s.coerceToMatchOutput: %s %s type = %s",getName(),ap.getName(),(val==null?"null":val.toString()),ap.getConnectionType());
					if( ConnectionType.DATA.equals(ap.getConnectionType()))  {
						val = new Double(fcns.coerceToDouble(val));
					}
					else if( ConnectionType.TRUTHVALUE.equals(ap.getConnectionType())) {
						boolean flag = fcns.coerceToBoolean(val);
						if( flag ) val = TruthValue.TRUE;
						else val = TruthValue.FALSE;
					}
					else val = val.toString();
					break;
				}
			}
		}
		return val;
	}
	// ================================= PropertyChangeListener ================================
	/**
	 * One of the block properties has changed. This default implementation simply updates
	 * the block property with the new value and logs the result. The data type is guaranteed
	 * to be QualifiedValue.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		String propertyName = event.getPropertyName();
		Object newValue = event.getNewValue();
		setProperty(propertyName,event.getNewValue());

		if( log.isTraceEnabled() ) {
			Object oldValue = event.getOldValue();
			log.debugf("%s.propertyChange: %s from %s to %s",this.getName(),propertyName,
					(oldValue==null?"null":oldValue.toString()),newValue.toString());
		}
	}
	
	/**
	 * Validate the incoming value. This does not execute unless the logger
	 * is enabled for debugging.
	 */
	private void checkIncomingValue(IncomingNotification vcn ) {
		/*
		if( log.isDebugEnabled() ) {
			if(getName()==null)          log.warnf("AbstractProcessBlock.validate: getName is null");
			
			if(vcn.getValue()==null)      log.warnf("AbstractProcessBlock.validate: %s getValue is null",getName());
			if(vcn.getConnection()==null) log.warnf("AbstractProcessBlock.validate: %s getConnection is null",getName());
			else {
				if(vcn.getConnection().getSource()==null) log.warnf("AbstractProcessBlock.validate: %s getSource is null",getName());
				if(vcn.getConnection().getUpstreamPortName()==null) log.warnf("AbstractProcessBlock.validate: %s getUpPort is null",getName());
			}
		}
		*/
	}
	/**
	 * Convert the block into a portable, serializable description.
	 * The basic descriptor holds common attributes of the block.
	 * @return the descriptor
	 */
	@Override
	public SerializableBlockStateDescriptor toDescriptor() {
		SerializableBlockStateDescriptor descriptor = new SerializableBlockStateDescriptor();
		descriptor.setName(getName());
		descriptor.setIdString(getBlockId().toString());
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put(BLTProperties.BLOCK_ATTRIBUTE_CLASS,getClassName());
		return descriptor;
	}
	
	/**
	 * Check the block configuration for missing or conflicting
	 * information.
	 * @return a validation summary. Null if everything checks out.
	 */
	@Override
	public String validate() {
		StringBuffer summary = new StringBuffer();
		for(BlockProperty property:propertyMap.values()) {
			BindingType bindingType = property.getBindingType();
			if( bindingType.equals(BindingType.TAG_MONITOR) ||
				bindingType.equals(BindingType.TAG_READ)    ||
				bindingType.equals(BindingType.TAG_WRITE)   ||
				bindingType.equals(BindingType.TAG_READWRITE) ) {
				
				String tagPath = property.getBinding();
				if( !controller.validateTag(getParentId(),tagPath) ) {
					summary.append(String.format("%s: configured tag (%s) dos not exist\t",property.getName(),tagPath));
				}
			}
		}
		if( summary.length()==0 ) return null;
		else return summary.toString();
	}

}