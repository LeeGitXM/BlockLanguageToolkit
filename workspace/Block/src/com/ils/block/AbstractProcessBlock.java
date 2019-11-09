/**
 *   (c) 2013-2019  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
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
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
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
	protected final static int DEFAULT_ACTIVITY_BUFFER_SIZE = 10; 
	protected final static String DEFAULT_FORMAT = "yyyy/MM/dd HH:mm:ss";
	protected final static SimpleDateFormat dateFormatter = new SimpleDateFormat(DEFAULT_FORMAT);
	protected final FixedSizeQueue<Activity> activities;
	protected ExecutionController controller = null;
	private UUID blockId;
	private UUID parentId;
	private long projectId = -1;    // This is the global project
	private GeneralPurposeDataContainer auxiliaryData = new GeneralPurposeDataContainer();
	private String name = ".";
	protected QualifiedValue lastValue = null;  // Most recently propagated value.
	protected String statusText;
	protected PalettePrototype prototype = null;
	protected boolean delayStart = false;
	protected boolean locked     = false;
//	private boolean isReceiver = false;
//	private boolean isTransmitter = false;
	protected boolean running = false;
	protected TruthValue state = TruthValue.UNSET;
	protected Date stateChangeTimestamp = null;
	protected WatchdogTimer timer = null;

	protected LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	/** Properties are a dictionary of attributes keyed by property name */
	protected final Map<String,BlockProperty> propertyMap;
	/** PropertyBlocks is a dictionary of block properties displayed in the workspace */
	/** It's here so that they can be efficiently notified of changes, and managed by the parent block (this) */
//	protected final Map<String,ProcessBlock> displayedPropertyBlocks;
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
//		displayedPropertyBlocks = new HashMap<>();
		anchors = new ArrayList<AnchorPrototype>();
		activities = new FixedSizeQueue<Activity>(DEFAULT_ACTIVITY_BUFFER_SIZE);
		lastValue = null;
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
		BlockProperty bufferSize = new BlockProperty(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE,new Integer(activities.getBufferSize()),PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE, bufferSize);
	}
	

	/**
	 * Fill a prototype object with defaults - as much as is reasonable.
	 */
	private void initializePrototype() {
		prototype = new PalettePrototype();
		BlockDescriptor blockDescriptor = prototype.getBlockDescriptor();
		blockDescriptor.setAnchors(anchors);
//		blockDescriptor.setReceiveEnabled(isReceiver());
//		blockDescriptor.setTransmitEnabled(isTransmitter());
		
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
	public List<AnchorPrototype>getAnchors() { 
//		log.error("EREIAM jh - abstract get anchors");
		return anchors; 
		}
	public GeneralPurposeDataContainer getAuxiliaryData() {return auxiliaryData;}
	public void setAuxiliaryData(GeneralPurposeDataContainer auxiliaryData) {this.auxiliaryData = auxiliaryData;}
	@Override
	public PalettePrototype getBlockPrototype() {return prototype; }
	@Override
	public String getClassName() {return this.getClass().getCanonicalName();}
	/**
	 * Blocks that have a logical output are responsible for 
	 * compiling a string that describes the reason for either 
	 * a TRUE or FALSE result. If the block has logical inputs
	 * then the explanation accounts for upstream explanations.
	 * 
	 * @return an explanation for the current state of the block.
	 *         By default it is the concatenated explanations of all 
	 *         upstream blocks with the same state.
	 *         If this is a block that has no relevant state, return
	 *         an empty string.
	 */
	@Override
	public String getExplanation(DiagnosticDiagram diagram,List<UUID> members) {
		String explanation = "";
		members.add(blockId);
		TruthValue blockState = getState();
		if( blockState.equals(TruthValue.TRUE) || blockState.equals(TruthValue.FALSE)) {
			List<ProcessBlock>predecessors = diagram.getUpstreamBlocks(this);
			for( ProcessBlock predecessor:predecessors ) {
				if( members.contains(predecessor.getBlockId())) {
					explanation = explanation + "-- truncated (circular reasoning)";
				}
				else if( blockState.equals(predecessor.getState())) {
						if(!explanation.isEmpty()) explanation = explanation + ", ";
						explanation = explanation + predecessor.getExplanation(diagram,members);
				}
			}
		}
		return explanation;
	}
	@Override
	public String getName() {return name;}
	@Override
	public long getProjectId() {return projectId;}
	@Override
	public void setProjectId(long projectId) {this.projectId = projectId;}
	@Override
	public TruthValue getState() {
		//log.infof("%s.getState: %s",name,this.state.name());
		return this.state;
	}
	@Override
	public void setState(TruthValue newState) { 
		if(newState!=null && !this.state.equals(newState)) {
			this.state = newState; 
			recordActivity(Activity.ACTIVITY_STATE,state.name());
			this.stateChangeTimestamp = new Date(timer.getTestTime());
			// We assume that the only blocks where state is set are logical,
			// so setting the last value makes sense ...
			lastValue = new TestAwareQualifiedValue(timer,state.name());
			//log.infof("%s.setState: %s",name,this.state.name());
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
	 * @param nam the property (attribute) name.
	 * @return a particular property given its name.
	 */
	@Override
	public BlockProperty getProperty(String nam) {
		return propertyMap.get(nam);
	}
	
	/**
	 * @param nam the property (attribute) name.
	 * @return a particular property block given its name.
	 */
//	@Override
//	public ProcessBlock getPropertyBlock(String nam) {
//		return propertyBlocks.get(nam);
//	}
	
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
		attributes.put("Parent", getParentId().toString());
		if( stateIsMeaningful() ) {
			attributes.put("State", getState().toString());
			if( stateChangeTimestamp!=null ) {     // On start we'never had a state change
				attributes.put("StateChangeTimestamp",dateFormatter.format(stateChangeTimestamp));
			}
		}

		if( activities.size()>0 ) {
			List<Activity> buffer = descriptor.getActivities();
			synchronized(activities) {
				for( Activity act:activities) {
					buffer.add(act.clone());
				}
			}
		}

//		if( displayedPropertyBlocks.size()>0 ) {
//			List<String> buffer = descriptor.getDisplayedProperties();
//			synchronized(displayedPropertyBlocks) {
//				for( String act:displayedPropertyBlocks.keySet()) {
//					buffer.add(new String(act));
//				}
//			}
//		}
//
		Map<String,BlockProperty> properties = descriptor.getProperties();
		if( propertyMap.size()>0 ) {
			for (String key:propertyMap.keySet()) {
				BlockProperty thingy = propertyMap.get(key);
				properties.put(key, thingy);
			}
		}
		
		return descriptor;
	}
	@Override
	public QualifiedValue getLastValue() { return this.lastValue; }

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
	@Override
	public Date getTimeOfLastStateChange() { return this.stateChangeTimestamp; }
	
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
//	@Override
//	public boolean isReceiver() { return isReceiver; }
//	@Override
//	public boolean isTransmitter() { return isTransmitter; }
	
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
			synchronized(activities) {
				activities.add(activity);
			}
		}
	}
	/**
	 * Add a time-stamped entry to the block's activity log.
	 * The log is viewable as part of the internal status.
	 * @param desc description of the activity being recorded. 
	 *        Presumably this comes from a controlled vocabulary
	 * @param prop property receiving the new value, if any.
	 * @param value a new value associated with the activity, if any.
	 */
	public void recordActivity(String desc,String prop,String value) {
		if( activities.getBufferSize()>0) {
			if(prop==null)  prop="null";
			if(value==null) value="null";
			Activity activity = new Activity(desc,String.format("%s=%s", prop,value));
			synchronized(activities) {
				activities.add(activity);
			}
		}
	}
	/**
	 * This variant accepts a key argument that is probably a UUID
	 * @param desc description of the activity being recorded. 
	 *        Presumably this comes from a controlled vocabulary
	 * @param prop property receiving the new value, if any.
	 * @param value a new value associated with the activity, if any.
	 * @param key block identifier describing the activity
	 */
	public void recordActivity(String desc,String prop,String value,String key) {
		if( activities.getBufferSize()>0) {
			if(prop==null)  prop="";
			if(value==null) value="";
			Activity activity = new Activity(desc,String.format("%s=%s (%s)", prop,value,key));
			synchronized(activities) {
				activities.add(activity);
			}
		}
	}
	/**
	 * The default method sets the state to UNSET.
	 * It also sends notifications to block outputs setting them to empty or
	 * UNSET. This does NOT clear the lastValue, thus allowing it to be used
	 * for propagate().
	 * 
	 * NOTE: This has no effect on Python blocks. They must do this
	 * for themselves.
	 */
	@Override
	public void reset() {
		this.state = TruthValue.UNSET;
		this.lastValue = null;
		recordActivity(Activity.ACTIVITY_RESET,"");
		if( controller!=null ) {
			// Send notifications on all outputs to indicate empty connections.
			// For truth-values, actually propagate UNSET.
			for(AnchorPrototype ap:getAnchors()) {
				if( ap.getAnchorDirection().equals(AnchorDirection.OUTGOING) ) {
					if( ap.getConnectionType().equals(ConnectionType.TRUTHVALUE) ) {
						QualifiedValue UNSET_TRUTH_VALUE = new TestAwareQualifiedValue(timer,TruthValue.UNSET);
						controller.sendConnectionNotification(getBlockId().toString(), ap.getName(),UNSET_TRUTH_VALUE);
						OutgoingNotification nvn = new OutgoingNotification(this,ap.getName(),UNSET_TRUTH_VALUE);
						controller.acceptCompletionNotification(nvn);
					}
					else if( ap.getConnectionType().equals(ConnectionType.DATA)) {
						QualifiedValue EMPTY_DATA_VALUE = new TestAwareQualifiedValue(timer,"");
						controller.sendConnectionNotification(getBlockId().toString(), ap.getName(),EMPTY_DATA_VALUE);
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
			recordActivity(Activity.ACTIVITY_PROPERTY,name,value.toString());
			prop.setValue(value);
			
			// check if this property is displayed in a DisplayPropertyBlock and update it.
			if (prop.isDisplayed() && prop.getDisplayedBlockId() != null && prop.getDisplayedBlockId().length() > 1) {  // so, there is a small chance that this could result in an infinite update loop

				// need to have a way to inject this into the notification buffer of the execution controller.  Add a signal connection if none exist
				// the destination block won't have an input defined, so no line will be drawn

				Signal siggy = new Signal(BlockConstants.COMMAND_CONFIGURE, BlockConstants.BLOCK_PROPERTY_TEXT, value.toString());  // this should always be a string anyway.
				QualifiedValue qv = new TestAwareQualifiedValue(timer,siggy);
				
				OutgoingNotification note = new OutgoingNotification(this,BlockConstants.SIGNAL_PORT_NAME, qv);
				
				controller.sendPropertyUpdateNotification(note, prop.getDisplayedBlockId());
				
			}
		}
	}
	/**
	 * The block is notified that a new value has appeared on one of its input anchors.
	 * The base implementation simply logs the value.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param incoming notification of the new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		checkIncomingValue(incoming );
		QualifiedValue qv = incoming.getValue();
		String port = incoming.getPropertyName();
		// Check to see if the notification applies to a bound property.
		if(port!=null) {
			if( qv!=null && qv.getValue()!=null ) {
				// Trigger a property change in the block
				BlockPropertyChangeEvent e = new BlockPropertyChangeEvent(getName(),port,getProperty(port).getValue(),qv.getValue());
				propertyChange(e);
			}
			else {
				recordActivity(Activity.ACTIVITY_RECEIVE_NULL,port);
			}
		}
		else {
			port = incoming.getConnection().getDownstreamPortName();
			String value ="NULL";
			if( qv!=null && qv.getValue()!=null ) {
				value = qv.getValue().toString();
				String key = incoming.getConnection().getSource().toString();
				recordActivity(Activity.ACTIVITY_RECEIVE,port,value,key);
			}
			else {
				recordActivity(Activity.ACTIVITY_RECEIVE_NULL,port);
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
		recordActivity(Activity.ACTIVITY_RECEIVE,sig.getCommand());
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

	
	private void updatePropertyDisplays() {
		for (BlockProperty prop:propertyMap.values()) {
			if (prop.isDisplayed() && prop.getDisplayedBlockId() != null && prop.getDisplayedBlockId().length() > 1) {  // so, there is a small chance that this could result in an infinite update loop
		
				// need to have a way to inject this into the notification buffer of the execution controller.  Add a signal connection if none exist
				// the destination block won't have an input defined, so no line will be drawn
		
				Signal siggy = new Signal(BlockConstants.COMMAND_CONFIGURE, BlockConstants.BLOCK_PROPERTY_TEXT, prop.getValue().toString());  // this should always be a string anyway.
				QualifiedValue qv = new TestAwareQualifiedValue(timer,siggy);
				
				OutgoingNotification note = new OutgoingNotification(this,BlockConstants.SIGNAL_PORT_NAME, qv);
				
				controller.sendPropertyUpdateNotification(note, prop.getDisplayedBlockId());
				
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
		this.lastValue = null;
		recordActivity(Activity.ACTIVITY_START,"");
		this.stateChangeTimestamp = new Date(timer.getTestTime());
		updatePropertyDisplays();
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
	 * This method is called by the WatchdogTimer subsystem on
	 * a WatchdogObserver to indicate a timeout.
	 */
	public void evaluate() {}
	/**
	 * The expectation is that the block will output its current value.
	 * This is an external interface and will not necessarily duplicate
	 * what the block will do on a value change.
	 * 
	 * This is never called internally. This implementation handles
	 * the most common configurations.
	 */
	@Override
	public void propagate() {
		// lastValue is used for blocks that send data
		if( lastValue!=null ) {
			recordActivity(Activity.ACTIVITY_PROPAGATE,lastValue.toString());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			if(controller!=null) controller.acceptCompletionNotification(nvn);
			notifyOfStatus();
		}
	}
	
	// =================================  Convenience Methods   ================================
	// A null is equivalent to UNSET
	protected TruthValue qualifiedValueAsTruthValue(QualifiedValue qv) {
		TruthValue result = TruthValue.UNSET;  
		Object value = qv.getValue();
		if( value!= null && !value.toString().isEmpty() && !value.equals(BLTProperties.UNDEFINED) ) {
			result = fcns.coerceToTruthValue(value);
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
						// Either dates or doubles are legal
						if( val instanceof String ) {
							try {
								val = dateFormatter.parse(val.toString());
							}
							catch(ParseException pe) {
								try {
									val = new Double(fcns.coerceToDouble(val));
								}
								catch(NumberFormatException nfe) {
									val = Double.NaN;
								}
							}
						}
					}
					else if( ConnectionType.TRUTHVALUE.equals(ap.getConnectionType())) {
						val = fcns.coerceToTruthValue(val);
					}
					else if( ConnectionType.TEXT.equals(ap.getConnectionType())) {
						val = val.toString();
					}
					// For type ANY, just leave alone
					break;
				}
			}
		}
		// Coerce a null value
		else if( !anchors.isEmpty() ) {
			for(AnchorPrototype ap: anchors) {
				if(ap.getName().equals(port)) {
					log.debugf("%s.coerceToMatchOutput: %s %s type = %s",getName(),ap.getName(),"null",ap.getConnectionType());
					if( ConnectionType.DATA.equals(ap.getConnectionType()))  {
						val = Double.NaN;
					}
					else if( ConnectionType.TRUTHVALUE.equals(ap.getConnectionType())) {
						val = TruthValue.UNSET;
					}
					else  {
						val = "";
					}
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
				synchronized(activities) {
					activities.setBufferSize(bufferSize);
				}
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
		attributes.put(BLTProperties.BLOCK_ATTRIBUTE_PARENT,getParentId().toString());
		return descriptor;
	}
	/**
	 * This is useful for blocks that have no real interest in the state. It's merely
	 * a bookkeeping step that denotes pass-thru of a truth-value.
	 * @param qv new qualified value
	 */
	protected void updateStateForNewValue(QualifiedValue qv) {
		if( qv==null || qv.getValue()==null ) return;  // Do nothing
		if(qv.getValue() instanceof TruthValue ) { 
			setState((TruthValue)qv.getValue());
		}
		else if(qv.getValue() instanceof Boolean ) {
			if( ((Boolean)qv.getValue()).booleanValue()) setState(TruthValue.TRUE);
			else setState(TruthValue.FALSE);
		}
		else if(qv.getValue().toString().equalsIgnoreCase("false") ) {
			setState(TruthValue.FALSE);
		}
		else if(qv.getValue().toString().equalsIgnoreCase("true") ) {
			setState(TruthValue.TRUE);
		}
		else if(qv.getValue().toString().equalsIgnoreCase("unknown") ) {
			setState(TruthValue.UNKNOWN);
		}
		else setState(TruthValue.UNSET);
	}
	/**
	 * @param tagpath provider-free tag path
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
				// In general, we allow un-configured bindings. Some classes may enforce
				// a more restrictive policy.
				if( tagPath!=null && tagPath.length()>0 && !tagPath.endsWith("]") ) {
					String reason = controller.validateTag(getParentId(),tagPath);
					if( reason!=null ) {
						summary.append(String.format("%s: tag (%s) %s\t",property.getName(),tagPath,reason));
					}
					if( !bindingType.equals(BindingType.TAG_WRITE) && !controller.hasActiveSubscription(this, property,tagPath)) {
						summary.append(String.format("%s: has no subscription for tag %s\t",property.getName(),tagPath));
					}
				}
			}
		}
		if( summary.length()==0 ) return null;
		else return summary.toString();
	}
	/**
	 * In some circumstances, e.g. the user has edited the diagram,
	 * the connections to a port that accepts multiples may have changed.
	 * The default version of this method does nothing.  
	 * @param portName name of the port
	 * @param cxns incoming connections attached at the port.
	 */
	public void validateConnections() {}
	
	/**
	 * Used by a handful of logic blocks that allow variable number of connections at a
	 * particular input port.
	 * 
	 *  Effectively removes duplicate entries that have no port specified (initial values) if  
	 *  a new value has been added with a port value
	 *  
	 * @param qualifiedValueMap map of last values keyed by uuid of upstream block.
	 */
	protected void pruneInitialConnections(Map<String,QualifiedValue> qualifiedValueMap) {
		ArrayList<String> initializedValues = new ArrayList<String>();
		ArrayList<String> removeList = new ArrayList<String>();
		for(String idString:qualifiedValueMap.keySet() ) {
			if (idString.contains(":")) {
//				log.errorf("EREIAM JH - initializedValues.add :%s: trimmed to :%s:",idString,idString.substring(0,idString.indexOf(':')));
				initializedValues.add(idString.substring(0,idString.indexOf(':')));
			}
		}
		for(String idString:qualifiedValueMap.keySet() ) {
			if (initializedValues.contains(idString)) {
//				log.errorf("EREIAM JH - found :%s:, removing",idString);
				// Duplicate found, remove it
				removeList.add(idString);
			}
		}
		for(String idString:removeList ) {
			qualifiedValueMap.remove(idString);
		}
	}

	/**
	 * Used by a handful of logic blocks that allow variable number of connections at a
	 * particular input port. 
	 * @param uuids UUIDS of connected blocks as strings
	 * @param qualifiedValueMap map of last values keyed by uuid of upstream block.
	 * @param unset the object to be used for the initial value of a new connection
	 */
	protected void reconcileQualifiedValueMap(String port,Map<String,QualifiedValue> qualifiedValueMap,Object unset) {
		List<SerializableBlockStateDescriptor> descriptors = controller.listBlocksConnectedAtPort(parentId.toString(), 
				blockId.toString(), port);
		
		recordActivity(Activity.ACTIVITY_INITIALIZE,"reconcile entry map",String.format("%d inputs", descriptors.size()));
		log.debugf("%s.reconcileQualifiedValueMap: checking ------------- -",getName());
		List<String> toBeAdded = new ArrayList<>();
		List<String> toBeDeleted = new ArrayList<>();
		List<String> toBeRetained = new ArrayList<>();
		for( SerializableBlockStateDescriptor desc:descriptors ) {
			String idString = desc.getIdString();
			if(qualifiedValueMap.get(idString)==null ) toBeAdded.add(idString);
			else if( qualifiedValueMap.keySet().contains(idString) ) {
				toBeRetained.add(idString);
			}
		}
		for(String idString:qualifiedValueMap.keySet() ) {
			if( !toBeRetained.contains(idString) ) {
				toBeDeleted.add(idString);
			}
		}
		for( String key:toBeDeleted ) {
			log.debugf("%s.reconcileQualifiedValueMap: removing connection from %s",getName(),key);
			qualifiedValueMap.remove(key);
		}
		for( String key:toBeAdded ) {
			log.debugf("%s.reconcileQualifiedValueMap: adding connection to %s",getName(),key);
			qualifiedValueMap.put(key,new BasicQualifiedValue(unset));
		}	
	}
	/**
	 * Check any properties that are bound to tags. Verify that the
	 * property matches the current value of the tag.
	 * @return a validation summary. Null if everything checks out.
	 */
	public String validateSubscription() {
		StringBuffer summary = new StringBuffer();
		for(BlockProperty property:propertyMap.values()) {
			BindingType bindingType = property.getBindingType();
			if( bindingType.equals(BindingType.TAG_MONITOR) ||
					bindingType.equals(BindingType.TAG_READ)    ||
					bindingType.equals(BindingType.TAG_READWRITE) ) {

				String tagPath = property.getBinding();
				// In general, we allow un-configured bindings. Some classes may enforce
				// a more restrictive policy.
				if( tagPath!=null && tagPath.length()>0 && !tagPath.endsWith("]") ) {
					String reason = controller.validateTag(getParentId(),tagPath);
					if( reason!=null ) {
						summary.append(String.format("%s: tag (%s) %s\t",property.getName(),tagPath,reason));
					}
					else {
						QualifiedValue tagValue = controller.getTagValue(getParentId(), tagPath);
						if(tagValue.getValue()!=null && property.getValue()!=null && !tagValue.getValue().equals(property.getValue())) {
							summary.append(String.format("%s = %s,but tag (%s) = %s\t",property.getName(),property.getValue().toString(),
									tagPath,tagValue.getValue().toString()));
						}
						else if( (tagValue.getValue()==null && property.getValue()!=null) || (tagValue.getValue()!=null && property.getValue()==null)) {
							summary.append(String.format("For property %s using tag (%s), one but not both values is null\t",property.getName(),tagPath));
						}
					}
				}
			}
		}
		if( summary.length()==0 ) return null;
		else return summary.toString();
	}
	
	/**
	 * Determine if the state should be displayed when viewing internal status.
	 * If state is an output, then it is important.
	 * @return true if the block is stateful in a truth-value sense.
	 */
	private boolean stateIsMeaningful() {
		
		boolean meaningful = false;
		for(AnchorPrototype ap:getAnchors()) {
			if( ap.getAnchorDirection().equals(AnchorDirection.OUTGOING) ) {
				if( ap.getConnectionType().equals(ConnectionType.TRUTHVALUE) ) {
					meaningful = true;
					break;
				}
			}
		}
		return meaningful;
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

//	public void setTransmitter(boolean isTransmitter) {
//		this.isTransmitter = isTransmitter;
//	}
//
//	public void setReceiver(boolean isReceiver) {
//		this.isReceiver = isReceiver;
//	}
//
}