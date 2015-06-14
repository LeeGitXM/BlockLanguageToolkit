/**
 *   (c) 2013-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.BlockPropertyChangeListener;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * This abstract class is the base of all blocks. It cannot in itself
 * be instantiated. It implements the CoreBlock interface.
 */
public abstract class AbstractBlock implements CoreBlock, BlockPropertyChangeListener {
	protected final static String DEFAULT_FORMAT = "YYYY/MM/dd hh:mm:ss";
	protected final static SimpleDateFormat formatter = new SimpleDateFormat(DEFAULT_FORMAT);
	protected ExecutionController controller = null;
	private UUID blockId;
	private UUID parentId;
	private long projectId = -1;    // This is the global project
	private String name = ".";
	protected PalettePrototype prototype = null;
	protected final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	/** Properties are a dictionary of attributes keyed by property name */
	protected final Map<String,BlockProperty> propertyMap;
	/** Describe ports/stubs where connections join the block */
	protected List<AnchorPrototype> anchors;
	protected final UtilityFunctions fcns = new UtilityFunctions();

	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 *              It does not correspond to a functioning block.
	 */
	public AbstractBlock() {
		propertyMap = new HashMap<>();
		anchors = new ArrayList<AnchorPrototype>();
		prototype = new PalettePrototype();
		prototype.setPaletteIconPath("unknown.png"); // see images of the BLT_Designer source area.
		BlockDescriptor blockDescriptor = prototype.getBlockDescriptor();
		blockDescriptor.setAnchors(anchors);
	}
	
	/**
	 * Constructor: Use this version to create a block that correlates to a block in the diagram.
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block. The id may be null for blocks that are "unattached"
	 * @param block universally unique Id for the block
	 */
	public AbstractBlock(ExecutionController ec, UUID parent, UUID block) {
		this();
		this.controller = ec;
		this.blockId = block;
		this.parentId = parent;
	}
	
	
	@Override
	public List<AnchorPrototype>getAnchors() { return anchors; }

	@Override
	public PalettePrototype getBlockPrototype() {return prototype; }
	@Override
	public String getClassName() {return this.getClass().getCanonicalName();}
	@Override
	public String getName() {return name;}
	@Override
	public long getProjectId() {return projectId;}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = new SerializableBlockStateDescriptor();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Name", getName());
		attributes.put("UUID", getBlockId().toString());
		return descriptor;
	}
	
	@Override
	public void setProjectId(long projectId) {this.projectId = projectId;}
	@Override
	public void setName(String lbl) {this.name = lbl;}
	

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
	@Override
	public void start(){}
	/**
	 * Terminate any active operations within the block.
	 */
	 @Override
	public void stop() {}
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
	 * There is no default behavior.
	 * 
	 * @param sn notification of a signal.
	 */
	@Override
	public void acceptValue(SignalNotification sn) {}
	
	/**
	 * In the case where the block has specified a coalescing time, this method
	 * will be called by the engine after receipt of input once the coalescing 
	 * "quiet" time has passed without further input.
	 * 
	 * The default implementation is appropriate for blocks that trigger calculation
	 * on every update of the inputs. It does nothing.
	 */
	@Override
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
				if( !controller.validateTag(getParentId(),tagPath) ) {
					summary.append(String.format("%s: configured tag (%s) dos not exist\t",property.getName(),tagPath));
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
	protected String valueToString(Object val) {
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
			result = String.format("%s:%s",((Signal)val).getCommand(),((Signal)val).getArg());
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
		if( arg instanceof AbstractBlock) {
			AbstractBlock that = (AbstractBlock)arg;
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