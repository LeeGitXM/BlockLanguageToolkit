/**
 *   (c) 2013-2015  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
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
import com.ils.common.FixedSizeQueue;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.WatchdogObserver;
import com.ils.common.watchdog.WatchdogTimer;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * This abstract class is the base of all blocks. It cannot in itself
 * be instantiated. 
 *  
 * The subclasses depend on the "ExecutableBlock" class annotation
 * as the signal to group a particular subclass into the list of 
 * available executable block types.
 */
public abstract class AbstractProcessBlock implements ProcessBlock, BlockPropertyChangeListener, WatchdogObserver {
	protected final static String DEFAULT_FORMAT = "yyyy/MM/dd hh:mm:ss";
	protected final static SimpleDateFormat dateFormatter = new SimpleDateFormat(DEFAULT_FORMAT);
	protected final FixedSizeQueue<Activity> activities;
	protected ExecutionController controller = null;
	private UUID blockId;
	private UUID parentId;
	private long projectId = -1;    // This is the global project
	private GeneralPurposeDataContainer auxiliaryData = null;
	private String name = ".";
	protected String statusText;
	protected PalettePrototype prototype = null;
	protected boolean delayStart = false;
	protected boolean locked     = false;
	protected boolean isReceiver = false;
	protected boolean isTransmitter = false;
	protected boolean running = false;
	protected TruthValue state = TruthValue.UNSET;
	protected WatchdogTimer timer = null;

	protected LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	/** Properties are a dictionary of attributes keyed by property name */
	protected final Map<String,BlockProperty> propertyMap;
	/** Describe ports/stubs where connections join the block */
	protected List<AnchorPrototype> anchors;
	protected final UtilityFunctions fcns = new UtilityFunctions();

	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 *              In this usage, it does not correspond to a functioning block. This constructor
	 *              is also used when deserializing. Properties are restored after this initialization.
	 */
	public AbstractProcessBlock() {
		propertyMap = new HashMap<>();
		anchors = new ArrayList<AnchorPrototype>();
		activities = new FixedSizeQueue<Activity>(0);
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
		this();
		this.controller = ec;
		this.blockId = block;
		this.parentId = parent;
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
		
		// Define a property that holds the size of the activity buffer. This applies to all blocks.
		BlockProperty bufferSize = new BlockProperty(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE,new Integer(activities.size()),PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE, bufferSize);
	}
	
	/**
	 * Fill a prototype object with defaults - as much as is reasonable.
	 */
	private void initializePrototype() {
		prototype = new PalettePrototype();
		BlockDescriptor blockDescriptor = prototype.getBlockDescriptor();
		blockDescriptor.setAnchors(anchors);
		blockDescriptor.setReceiveEnabled(isReceiver);
		blockDescriptor.setTransmitEnabled(isTransmitter);
		
		// Currently this refers to a path in /images of the BLT_Designer source area.
		prototype.setPaletteIconPath("unknown.png");
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

				OutgoingNotification nvn = new OutgoingNotification(this,port,new TestAwareQualifiedValue(timer,value));
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
	public String getClassName() {return this.getClass().getCanonicalName();}
	@Override
	public String getName() {return name;}
	@Override
	public long getProjectId() {return projectId;}
	@Override
	public void setProjectId(long projectId) {this.projectId = projectId;}
	@Override
	public TruthValue getState() {return state;}
	@Override
	public void setState(TruthValue state) { 
		if(state!=null) {
			this.state = state; 
			recordActivity(Activity.ACTIVITY_STATE,state.name());
		}
	}
	@Override
	public void setName(String lbl) {this.name = lbl;}
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
	 * @param name the property (attribute) name.
	 * @return a particular property given its name.
	 */
	@Override
	public BlockProperty getProperty(String nam) {
		return propertyMap.get(nam);
	}
	
	@Override
	public UUID getParentId() { return parentId; }
	@Override
	public UUID getBlockId() { return blockId; }
	
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
		if( activities.size()>0 ) {
			List<Activity> buffer = descriptor.getActivities();
			for( Activity act:activities) {
				buffer.add(act.clone());
			}
		}

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
	 * Add a time-stamped entry to the block's activity log.
	 * The log is viewable as part of the internal status.
	 * @param desc description of the activity being recorded. 
	 *        Presumably this comes from a controlled vocabulary
	 * @param value a new value associated with the activity, if any.
	 */
	public void recordActivity(String desc,String value) {
		if( activities.getBufferSize()>0) {
			if(value==null) value="";
			Activity activity = new Activity(desc,value);
			activities.add(activity);
		}
	}
	/**
	 * The default method sets the state to INITIALIZED.
	 * It also sends notifications to block outputs setting them to empty or
	 * unknown. NOTE: This has no effect on Python blocks. They must do this
	 * for themselves.
	 */
	@Override
	public void reset() {
		this.state = TruthValue.UNSET;
		recordActivity(Activity.ACTIVITY_RESET,"");
		if( controller!=null ) {
			// Send notifications on all outputs to indicate empty connections.
			// For truth-values, actually propagate UNKNOWN.
			for(AnchorPrototype ap:getAnchors()) {
				if( ap.getAnchorDirection().equals(AnchorDirection.OUTGOING) ) {
					if( ap.getConnectionType().equals(ConnectionType.TRUTHVALUE) ) {
						QualifiedValue UNKNOWN_TRUTH_VALUE = new TestAwareQualifiedValue(timer,TruthValue.UNKNOWN);
						controller.sendConnectionNotification(getBlockId().toString(), ap.getName(),UNKNOWN_TRUTH_VALUE);
						OutgoingNotification nvn = new OutgoingNotification(this,ap.getName(),UNKNOWN_TRUTH_VALUE);
						controller.acceptCompletionNotification(nvn);
					}
					else if( ap.getConnectionType().equals(ConnectionType.DATA)) {
						QualifiedValue NAN_DATA_VALUE = new TestAwareQualifiedValue(timer,new Double(Double.NaN));
						controller.sendConnectionNotification(getBlockId().toString(), ap.getName(),NAN_DATA_VALUE);
					}
					else {
						QualifiedValue EMPTY_STRING_VALUE = new TestAwareQualifiedValue(timer,"");
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
				QualifiedValue qvalue = vcn.getValue();
				if( qvalue!=null && qvalue.getValue()!=null ) {
					log.debugf("%s.acceptValue: %s (%s) port: %s",getName(),
							qvalue.getValue().toString(),
							qvalue.getQuality().getName(),
							vcn.getConnection().getDownstreamPortName());

				}
			}
		}
	}
	/**
	 * The block is notified that signal has been sent to it.
	 * The base implementation handles the universal commands:
	 *     reset
	 *     lock/unlock
	 *     evaluate
	 *     configure
	 * 
	 * @param sn notification of a signal.
	 */
	@Override
	public void acceptValue(SignalNotification sn) {
		Signal sig = sn.getSignal();
		if( sig.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_CONFIGURE) ) {
			String propertyName = sig.getArgument();
			BlockProperty bp = getProperty(propertyName);
			if( bp!=null ) {
				// Simulate the signal payload coming in as a value change. We don't really know the source
				BlockPropertyChangeEvent event = new BlockPropertyChangeEvent(getBlockId().toString(), propertyName, bp.getValue(), sig.getPayload());
				propertyChange(event);
				QualifiedValue qv = new TestAwareQualifiedValue(timer,bp.getValue());  // Value now event payload.
				controller.sendPropertyNotification(getBlockId().toString(),bp.getName(), qv);
			}
		}
		else if( sig.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_EVALUATE) ) {
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
				QualifiedValue qv = new TestAwareQualifiedValue(timer,bp.getValue());
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
	public void start() { 
		this.running = true;
		recordActivity(Activity.ACTIVITY_START,"");
	}
	/**
	 * Terminate any active operations within the block.
	 * This default method does nothing.
	 */
	@Override
	public void stop() {
		this.running = false;
		state = TruthValue.UNSET;
		recordActivity(Activity.ACTIVITY_STOP,"");
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
					log.debugf("%s.coerceToMatchOutput: %s %s type = %s",getName(),ap.getName(),(val==null?"null":val.toString()),ap.getConnectionType());
					if( ConnectionType.DATA.equals(ap.getConnectionType()))  {
						val = new Double(fcns.coerceToDouble(val));
					}
					else if( ConnectionType.TRUTHVALUE.equals(ap.getConnectionType())) {
						boolean flag = fcns.coerceToBoolean(val);
						if( flag ) val = TruthValue.TRUE;
						else val = TruthValue.FALSE;
					}
					else if( ConnectionType.TEXT.equals(ap.getConnectionType())) {
						val = val.toString();
					}
					// For type ANY, just leave alone
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
		// Handle the one property that is global
		if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE)) {
			try {
				int bufferSize = Integer.parseInt(event.getNewValue().toString());
				activities.setBufferSize(bufferSize);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert buffer size to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
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
		SerializableBlockStateDescriptor descriptor = getInternalStatus();
		descriptor.setClassName(getClassName());
		descriptor.setName(getName());
		descriptor.setIdString(getBlockId().toString());
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put(BLTProperties.BLOCK_ATTRIBUTE_CLASS,getClassName());
		return descriptor;
	}
	
	/**
	 * @param tagpath
	 * @return true if any property of the block is bound to
	 *         the supplied tagpath. The comparison does not
	 *         consider the provider portion of the path.
	 */
	@Override
	public boolean usesTag(String tagpath) {
		int pos = tagpath.indexOf("]");
		if(pos>0) tagpath = tagpath.substring(pos+1);
		for(BlockProperty property:propertyMap.values()) {
			String binding = property.getBinding();
			if( binding.endsWith(tagpath)) return true;
		}
		return false;
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
				String reason = controller.validateTag(getParentId(),tagPath);
				if( reason!=null ) {
					summary.append(String.format("%s: tag (%s) %s\t",property.getName(),tagPath,reason));
				}
				else if( !bindingType.equals(BindingType.TAG_WRITE) && !controller.hasActiveSubscription(this, property,tagPath)) {
					summary.append(String.format("%s: has no subscription for tag %s\t",property.getName(),tagPath));
				}
			}
		}
		if( summary.length()==0 ) return null;
		else return summary.toString();
	}
	
	/**
	 * Convert a value received on an input connection 
	 * into a string. Used for debugging purposes.
	 * @param val
	 * @return
	 */
	private String valueToString(Object val) {
		String result = "";
		if( val==null ) {
			result = "NULL";
		}
		else if( val instanceof QualifiedValue ) {
			Object value = ((QualifiedValue)val).getValue();
			if( value==null ) result = "NULL";
			else              result = value.toString();
		}
		else if(val instanceof Signal ) {
			result = String.format("%s:%s",((Signal)val).getCommand(),((Signal)val).getArgument());
		}
		else {
			result = val.toString();
		}
		return result;
	}
	
	// So that class is comparable
	// Same blockId is sufficient to prove equality
	@Override
	public boolean equals(Object arg) {
		boolean result = false;
		if( arg instanceof AbstractProcessBlock) {
			AbstractProcessBlock that = (AbstractProcessBlock)arg;
			if( this.getBlockId().equals(that.getBlockId()) ) {
				result = true;
			}
		}
		return result;
	}
	@Override
	public int hashCode() {
		return this.getBlockId().hashCode();
	}
	
	/**
	 * Identify the block as a string. Make this as user-friendly as possible.
	 */
	@Override
	public String toString() { return getName(); }

}