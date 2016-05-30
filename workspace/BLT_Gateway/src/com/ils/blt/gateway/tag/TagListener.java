/**
 *   (c) 2013-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.tag;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.IncomingValueChangeTask;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.PropertyChangeEvaluationTask;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.sqltags.model.Tag;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.model.TagProp;
import com.inductiveautomation.ignition.common.sqltags.model.event.TagChangeEvent;
import com.inductiveautomation.ignition.common.sqltags.model.event.TagChangeListener;
import com.inductiveautomation.ignition.common.sqltags.parser.TagPathParser;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.sqltags.SQLTagsManager;

/**
 *  The tag listener waits for inputs on a collection of tags and,
 *  whenever a tag value changes, the collector posts a change notice
 *  task directly to the block properties for which it is a listening proxy.
 *  
 *  WARNING: Access this class through the BlockExecutionController
 *           interfaces. The controller munges tag paths depending
 *           on the state of the diagram (e.g. ISOLATION mode).
 */
public class TagListener implements TagChangeListener   {
	private static final String TAG = "TagListener";
	private static int THREAD_POOL_SIZE = 10;   // Notification threads
	private static final boolean DEBUG = true;
	private final LoggerEx log;
	private GatewayContext context = null;
	private final Map<String,List<BlockPropertyPair>> blockMap;  // Blocks-Properties keyed by tag path (case-insensitive)
	private final Map<BlockPropertyPair,String>       tagMap;    // Tag paths keyed by Block-Property
	private final SimpleDateFormat dateFormatter;
	private boolean stopped = true;
	private final BlockExecutionController controller;
	private final ExecutorService threadPool;
	
	/**
	 * Constructor: 
	 */
	public TagListener(BlockExecutionController ec) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.blockMap = new HashMap<String,List<BlockPropertyPair>>();
		this.tagMap   = new HashMap<BlockPropertyPair,String>();
		this.dateFormatter = new SimpleDateFormat(BlockConstants.TIMESTAMP_FORMAT);
		this.controller = ec;
		this.threadPool = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
	}
	
	/**
	 * Clear the subscription maps so that subscriptions will not be re-established
	 * when the listener is re-started. This method is only valid when the listener
	 * is stopped.
	 */
	public void clearSubscriptions() {
		if( stopped ) {
			blockMap.clear();
			tagMap.clear();
		}
		else {
			throw new IllegalStateException("Attempt to clear subscriptions while TagListener is running");
		}
	}

	/**
	 * Define a tag subscription based on a block attribute. The subject attribute must be
	 * one associated with a tag. If we are running and
	 *     a) This is a new tag, then update the property from the subscription
	 *     b) We are sharing the tag, then update the property from the current value
	 *                                of a shared property.
	 */
	public synchronized void defineSubscription(ProcessBlock block,BlockProperty property,String tagPath) {
		
		if( DEBUG ) log.infof("%s.defineSubscription: %s:%s=%s",TAG,block.getName(),property.getName(),tagPath);
		if( tagPath!=null && tagPath.length() >0  ) {
			boolean needToStartSubscription = false;
			BlockPropertyPair key = new BlockPropertyPair(block,property);
			// If we've seen this before, then ignore
			List<BlockPropertyPair> list = blockMap.get(tagPath.toUpperCase()); 
			if( list==null ) {
				// First time we've seen this tag, start subscription
				list = new ArrayList<BlockPropertyPair>();
				blockMap.put(tagPath.toUpperCase(),list );
				needToStartSubscription = true;
			}
			if( list.contains(key))  {   
				// Duplicate request, nothing to do
				if( DEBUG) log.infof("%s.defineSubscription: %s:%s already subscribes to: %s",TAG,block.getName(),property.getName(),tagPath);
				return;
			}
			
			list.add(key);
			tagMap.put(key,tagPath);
			if( DEBUG ) log.infof("%s.defineSubscription: %s:%s now subscribes to: %s (%s)",TAG,block.getName(),property.getName(),
					tagPath,(needToStartSubscription?"START":"PIGGY-BACK"));
			if(!stopped ) {
				if(needToStartSubscription) startSubscriptionForTag(tagPath);
				else updatePropertyValueFromLinkedProperty(key,list);   // Get the value from another block's property
			}
		}
	}
	/**
	 * As a debugging aid, show the tag path that is currently associated with
	 * the indicated block and property.
	 * 
	 * @param block
	 * @param property
	 * @return the tag path as a String the corresponds to the current subscription
	 *         for the indicated block and property. If there is none, return null.
	 */
	public String getSubscribedPath(ProcessBlock block,BlockProperty property) {
		BlockPropertyPair key = new BlockPropertyPair(block,property);
		String path = tagMap.get(key);
		return path;
	}
	
	/**
	 * Check and see if the current block/property has a subscription.
	 * @param block
	 * @param property
	 * @return
	 */
	public boolean hasActiveSubscription(ProcessBlock block,BlockProperty property,String path) {
		BlockPropertyPair key = new BlockPropertyPair(block,property);
		boolean result = false;
		List<BlockPropertyPair> list = blockMap.get(path.toUpperCase());
		if( list!=null && list.contains(key)) result = true;
		return result;
	}
	/**
	 * Remove a subscription based on a property. We assume that the tag path in
	 * the property has been updated. Attempt to find the old path based on a
	 * key formed from the block id and property name.
	 * 
	 * @param block
	 * @param property
	 */
	public void removeSubscription(ProcessBlock block,BlockProperty property) {
		BlockPropertyPair key = new BlockPropertyPair(block,property);
		String path = tagMap.get(key);
		if( path!=null ) {
			removeSubscription(block,property,path);
		}
	}
	/**
	 * Remove a subscription based on a tag path. Unsubscribe if this
	 * was the last reference to the path for any block.
	 * 
	 * @param tagPath
	 */
	public synchronized void removeSubscription(ProcessBlock block,BlockProperty property,String tagPath) {
		if( tagPath==null) return;    // There was no subscription
		//log.debugf("%s.removeSubscription: considering %s:%s=%s",TAG,block.getName(),property.getName(),tagPath);
		List<BlockPropertyPair> list = blockMap.get(tagPath.toUpperCase());
		if(list==null) return;
		BlockPropertyPair key = new BlockPropertyPair(block,property);
		list.remove(key);
		// Once the list is empty, we cancel the subscription
		if(list.isEmpty()) {
			if( DEBUG ) log.infof("%s.removeSubscription: cancelled %s:%s=%s",TAG,block.getName(),property.getName(),tagPath);
			blockMap.remove(tagPath.toUpperCase());
			if(!stopped) {
				// If we're running unsubscribe
				SQLTagsManager tmgr = context.getTagManager();
				try {
					TagPath tp = TagPathParser.parse(tagPath);
					tmgr.unsubscribe(tp, this);
					if( DEBUG ) log.infof("%s.removeSubscription: unsubscribed to %s",TAG,tagPath);
				}
				catch(IOException ioe) {
					log.errorf("%s.removeSubscription (%s)",TAG,ioe.getMessage());
				}
			}
		}
	}
	
	/**
	 * Un-subscribe to a path. Does not modify the map. We assume that this occurs
	 * because the listener is being shutdown.
	 * @param tagPath
	 */
	private void stopSubscription(String tagPath) {
		if( tagPath==null) return;    // There was no subscription
		if( stopped ) return;         // Everything is unsubscribed if we're stopped
		SQLTagsManager tmgr = context.getTagManager();
		try {
			TagPath tp = TagPathParser.parse(tagPath);
			if( DEBUG ) log.infof("%s.stopSubscription: %s",TAG,tagPath);
			tmgr.unsubscribe(tp, this);
		}
		catch(IOException ioe) {
			log.warnf("%s.stopSubscription: Error tag %s (%s)",TAG,tagPath,ioe.getMessage());
		}
	}
	/**
	 * Re-start. Create subscriptions for everything in the tag map.
	 * @param ctxt
	 */
	public void start(GatewayContext ctxt) {
		this.context = ctxt;
		log.infof("%s: start tagListener ...",TAG);
		for( String tagPath:blockMap.keySet()) {
			startSubscriptionForTag(tagPath);
		}
		stopped = false;
	}
	
	/**
	 * Call this method only once per tag path. We are either subscribing to a novel path,
	 * or are re-starting the listener. In either case we iterate through all the block-properties
	 * and update values.
	 * @param tagPath
	 */
	private void startSubscriptionForTag(String tagPath) {
		SQLTagsManager tmgr = context.getTagManager();
		List<BlockPropertyPair> list = blockMap.get(tagPath.toUpperCase());    // Should never be null
		if( list==null || list.size()==0 ) {
			log.warnf("%s.startSubscriptionForTag: %s - found no block/properties",TAG,tagPath);
			return;
		}
		ProcessBlock typicalBlock = list.get(0).getBlock();
		try {
			// If tag path isn't in canonical form, make it that way by prepending provider
			// We assume that all tags in the list have the same default provider
			String providerName = providerNameFromPath(tagPath);
			if( providerName.length()==0) {
				providerName = context.getProjectManager().getProps(typicalBlock.getProjectId(), ProjectVersion.Staging).getDefaultSQLTagsProviderName();
				if(providerName==null || providerName.length()==0 ) {
					log.warnf("%s.startSubscriptionForTag: %s - NO PROVIDER",TAG,tagPath);
				}
				int pos = tagPath.indexOf("]");
				if(pos>0) tagPath = tagPath.substring(pos+1);
				tagPath = String.format("[%s]%s",providerName,tagPath);
			}

			TagPath tp = TagPathParser.parse(tagPath);
			Tag tag = tmgr.getTag(tp);
			if( tag!=null ) {
				QualifiedValue value = tag.getValue();
				if( DEBUG ) log.infof("%s.startSubscriptionForTag: %s = %s (%s at %s)",TAG,
						tp.toStringFull(),value.getValue(),
						(value.getQuality().isGood()?"GOOD":"BAD"),
						dateFormatter.format(value.getTimestamp()));
				// Do not pass along nulls -- tag was never set
				if(value.getValue()!=null ) {
					// Update all properties with new value
					for(BlockPropertyPair key:list ) {
						ProcessBlock block = key.getBlock();
						BlockProperty property = key.getProperty();
						updateProperty(block,property,value);
					}
				}
				tmgr.subscribe(tp, this);
			}
			else {
				log.errorf("%s.startSubscriptionForTag: Failed. (%s unknown to provider %s)",TAG,tp.toStringFull(),providerName);
				setPropertiesForBadTag(list);
			}
		}
		catch(IOException ioe) {
			log.errorf("%s.startSubscriptionForTag (%s)",TAG,ioe.getMessage());
			setPropertiesForBadTag(list);
		}
		catch(IllegalArgumentException iae) {
			log.errorf("%s.startSubscriptionForTag - illegal argument for %s (%s)",TAG,tagPath,iae.getMessage());
			setPropertiesForBadTag(list);
		}
		catch(Exception ex) {
			log.errorf("%s.startSubscriptionForTag - Exception %s (%s)",TAG,ex.getMessage(),tagPath);
			setPropertiesForBadTag(list);
		}
	}
	/**
	 * Shutdown completely.
	 */
	public void stop() {
		log.infof("%s.stop tagListener, shutdown executor",TAG);
		for( String tagPath:blockMap.keySet()) {
			stopSubscription(tagPath);
		}
		stopped = true;
	}
	
	/** 
	 * NOTE: Previously we only listened on the property TagProp.VALUE.
	 * @return the tag property that we care about.
	 *         The null means all attributes.
	 */
	@Override
	public TagProp getTagProperty() {
		return null;
	}

	/**
	 * When a tag value/quality or timestamp changes, create a new property change task and
	 * execute it in its own thread. The change property is always a null 'cause that's what
	 * we're listening for.
	 * 
	 * @param event
	 */
	@Override
	public void tagChanged(TagChangeEvent event) {
		TagPath tp = event.getTagPath();
		Tag tag = event.getTag();
		if( tag!=null && tag.getValue()!=null && tp!=null ) {
			try {
				if( DEBUG || log.isTraceEnabled() ) log.infof("%s.tagChanged: %s received %s (%s at %s)",TAG,tp.toStringFull(),
						tag.getValue().getValue(),
						(tag.getValue().getQuality().isGood()?"GOOD":"BAD"),
						dateFormatter.format(tag.getValue().getTimestamp()));
				// The subscription may be to the fully qualified tag path
				// and/or the path assuming the default provider
				List<BlockPropertyPair> list1 = blockMap.get(tp.toStringFull().toUpperCase());
				List<BlockPropertyPair> list2 = blockMap.get(tp.toStringPartial().toUpperCase());
				List<BlockPropertyPair> list = new ArrayList<>();
				if( list1!=null ) list.addAll(list1);
				if( list2!=null ) list.addAll(list2);
				if( list.size()==0 ) {
					log.warnf("%s.tagChanged: %s - found no subscriber, unsubscribing",TAG,tp.toStringFull());
					stopSubscription(tp.toStringFull().toUpperCase());
					blockMap.remove(tp.toStringFull().toUpperCase());
					blockMap.remove(tp.toStringPartial());
					return;
				}
				for(BlockPropertyPair key:list) {
					ProcessBlock block = key.getBlock();
					BlockProperty property = key.getProperty();
					// Reject blocks that are in a disabled diagram
					ProcessDiagram parent = controller.getDiagram(block.getParentId());
					if( parent!=null ) {
						if( !parent.getState().equals(DiagramState.DISABLED)) {
							updateProperty(block,property,tag.getValue());
						}
					}
					else {
						log.warnf("%s.tagChanged: %s, subscriber %s has no parent diagram",TAG,tp.toStringFull(),block.getName());
					}
				}			
			}
			catch(Exception ex) {
				log.error(TAG+".tagChanged exception ("+ex.getMessage()+")",ex);
			}
		}
		else {
			// Tag or path is null
			log.warnf("%s.tagChanged: Unknown tag %s (%s)",TAG,(tag==null?"null":tag.getName()),(tp==null?"null":tp.toStringFull()));
		}
	}
	
	private String providerNameFromPath(String tagPath) {
		String provider = "";
		if( tagPath.startsWith("[") ) {
			int index = tagPath.indexOf(']');
			if( index>0 ) {
				provider = tagPath.substring(1,index);
			}
		}
		return provider;
	}
	
	private void updateProperty(ProcessBlock block,BlockProperty property,QualifiedValue value) {
		try {
			// Treat the notification differently depending on the binding
			if( property.getBindingType().equals(BindingType.TAG_MONITOR)) {
				if( DEBUG ) log.infof("%s.updateProperty: property change for %s:%s",TAG,block.getName(),property.getName());
				PropertyChangeEvaluationTask task = new PropertyChangeEvaluationTask(block,
								new BlockPropertyChangeEvent(block.getBlockId().toString(),property.getName(),property.getValue(),value.getValue()));
				Thread propertyChangeThread = new Thread(task, "PropertyChange");
				propertyChangeThread.start();
			}
			else if( property.getBindingType().equals(BindingType.TAG_READ) ||
					 property.getBindingType().equals(BindingType.TAG_READWRITE)) {
					// Set property with no notifications
					property.setValue(value.getValue());
					// The tag subscription acts as a pseudo input
					IncomingNotification notice = new IncomingNotification(property.getName(),value);
					threadPool.execute(new IncomingValueChangeTask(block,notice));	
			}
			else {
				log.warnf("%s.updateProperty: %s property no longer bound (%s)",TAG,property.getName(),property.getBindingType());
			}
		}
		catch(Exception ex) {
			log.warnf("%s.updateProperty: Failed to execute change event (%s)",TAG,ex.getLocalizedMessage()); 
		}
	}
	
	private void updatePropertyValueFromLinkedProperty(BlockPropertyPair key,List<BlockPropertyPair>list) {
		if( list.size()>1 ) {
			// Set the value of the new property from an old one.
			// We've just appended the key to the end of the list, so the first value ought to be a good one.
			BlockProperty property = key.getProperty();
			BlockProperty typicalProperty = list.get(0).getProperty();
			QualifiedValue value = new BasicQualifiedValue(typicalProperty.getValue());
			updateProperty(key.getBlock(),property,value);
		}
	}
	
	// The tag doesn't exist, or is in error
	private void setPropertiesForBadTag(List<BlockPropertyPair> list) {
		for(BlockPropertyPair key:list) {
			ProcessBlock block = key.getBlock();
			BlockProperty property = key.getProperty();
			// Reject blocks that are in a disabled diagram
			ProcessDiagram parent = controller.getDiagram(block.getParentId());
			if( parent!=null ) {
				if( property.getType().equals(PropertyType.BOOLEAN) ) updateProperty(block,property,new BasicQualifiedValue(TruthValue.UNKNOWN));
				else if( property.getType().equals(PropertyType.DOUBLE) ) updateProperty(block,property,new BasicQualifiedValue(new Double(Double.NaN)));
				else updateProperty(block,property,new BasicQualifiedValue(""));
			}
		}		
	}
	// ====================================== ProjectResourceKey =================================
	/**
	 * Class for keyed storage by ProcessBlock and Property
	 */
	private class BlockPropertyPair {
		private final ProcessBlock block;
		private final BlockProperty property;
		public BlockPropertyPair(ProcessBlock blk,BlockProperty prop) {
			this.block = blk;
			this.property = prop;
		}
		public ProcessBlock  getBlock()    { return block; }
		public BlockProperty getProperty() { return property; }

		// So that class may be used as a map key
		// Same blockId and propertyName is sufficient to prove equality
		@Override
		public boolean equals(Object arg) {
			boolean result = false;
			if( arg instanceof BlockPropertyPair) {
				BlockPropertyPair that = (BlockPropertyPair)arg;
				if( (this.getBlock().getBlockId().toString().equalsIgnoreCase(that.getBlock().getBlockId().toString()) ) &&
						(this.getProperty().getName().equalsIgnoreCase(that.getProperty().getName()) )   ) {
					result = true;
				}
			}
			return result;
		}
		@Override
		public int hashCode() {
			int code = 0;
			if( this.block!=null)     code+=this.block.hashCode();
			if( this.property!=null ) code+=this.property.getName().hashCode();
			return code;
		}
	}
}
