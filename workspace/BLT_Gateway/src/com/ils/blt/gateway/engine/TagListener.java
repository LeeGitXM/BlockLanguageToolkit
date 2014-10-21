/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.engine;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.control.BlockPropertyChangeEvent;
import com.ils.blt.common.control.IncomingNotification;
import com.ils.blt.common.serializable.DiagramState;
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
 */
public class TagListener implements TagChangeListener   {
	private static final String TAG = "TagListener";
	private static int THREAD_POOL_SIZE = 10;   // Notification threads
	private final LoggerEx log;
	private GatewayContext context = null;
	private final Map<String,List<BlockPropertyPair>> blockMap;  // Blocks-Properties keyed by tag path
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
	public void defineSubscription(ProcessBlock block,BlockProperty property) {
		if( block==null || property==null || 
				!(property.getBindingType()==BindingType.TAG_READ || 
				  property.getBindingType()==BindingType.TAG_READWRITE ||
				  property.getBindingType()==BindingType.TAG_MONITOR )   ) return;
		log.infof("%s.defineSubscription: considering %s:%s",TAG,block.getName(),property.getName());
		String tagPath = property.getBinding();
		if( tagPath!=null && tagPath.length() >0  ) {
			boolean needToStartSubscription = false;
			BlockPropertyPair key = new BlockPropertyPair(block,property);
			// If we've seen this before, then ignore
			List<BlockPropertyPair> list = blockMap.get(tagPath); 
			if( list==null ) {
				// First time we've seen this tag, start subscription
				list = new ArrayList<BlockPropertyPair>();
				blockMap.put(tagPath,list );
				needToStartSubscription = true;
			}
			if( list.contains(key))  {   
				// Duplicate request, nothing to do
				log.infof("%s.defineSubscription: %s:%s already subscribes to: %s",TAG,block.getName(),property.getName(),tagPath);
				return;
			}
			
			list.add(key);
			tagMap.put(key,tagPath);
			log.infof("%s.defineSubscription: %s:%s now subscribes to: %s",TAG,block.getName(),property.getName(),tagPath);
			if(!stopped ) {
				if(needToStartSubscription) startSubscriptionForTag(tagPath);
				else updatePropertyValueFromLinkedProperty(key,list);   // Get the value from another block's property
			}
		}
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
	public void removeSubscription(ProcessBlock block,BlockProperty property,String tagPath) {
		if( tagPath==null) return;    // There was no subscription

		List<BlockPropertyPair> list = blockMap.get(tagPath);
		BlockPropertyPair key = new BlockPropertyPair(block,property);
		list.remove(key);
		// Once the list is empty, we cancel the subscription
		if(list.isEmpty()) {
			log.infof("%s.removeSubscription: %s",TAG,tagPath);
			blockMap.remove(tagPath);
			if(!stopped) {
				// If we're running unsubscribe
				SQLTagsManager tmgr = context.getTagManager();
				try {
					TagPath tp = TagPathParser.parse(tagPath);
					tmgr.unsubscribe(tp, this);
				}
				catch(IOException ioe) {
					log.errorf("%s.stopSubscription (%s)",TAG,ioe.getMessage());
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
			log.infof("%s.stopSubscription: %s",TAG,tagPath);
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
		List<BlockPropertyPair> list = blockMap.get(tagPath);    // Should never be null
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
				providerName = context.getProjectManager().getProps(typicalBlock.getProjectId(), ProjectVersion.Published).getDefaultSQLTagsProviderName();
				tagPath = String.format("[%s]%s",providerName,tagPath);
			}

			TagPath tp = TagPathParser.parse(tagPath);
			log.infof("%s.startSubscriptionForTag: on tag path %s",TAG,tp.toStringFull());

			Tag tag = tmgr.getTag(tp);
			if( tag!=null ) {
				QualifiedValue value = tag.getValue();
				log.infof("%s.startSubscriptionForTag: got a %s value for %s (%s at %s)",TAG,
						(value.getQuality().isGood()?"GOOD":"BAD"),
						tag.getName(),value.getValue(),
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
			}
			tmgr.subscribe(tp, this);
		}
		catch(IOException ioe) {
			log.errorf("%s.startSubscriptionForProperty (%s)",TAG,ioe.getMessage());
		}
		catch(IllegalArgumentException iae) {
			log.errorf("%s.startSubscriptionForProperty - illegal argument for %s (%s)",TAG,tagPath,iae.getMessage());
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
	 * NOTE: We tried returning null without observing any difference.
	 * @return the tag property that we care about,
	 *         that is the current value of the tag.
	 */
	@Override
	public TagProp getTagProperty() {
		return TagProp.Value;
	}

	/**
	 * When a tag value changes, create a new property change task and
	 * execute it in its own thread.
	 * 
	 * @param event
	 */
	@Override
	public void tagChanged(TagChangeEvent event) {
		TagPath tp = event.getTagPath();
		Tag tag = event.getTag();
		TagProp prop = event.getTagProperty();
		if( prop == TagProp.Value) {
			try {
				log.infof("%s: tagChanged: got a %s value for %s (%s at %s)",TAG,
					(tag.getValue().getQuality().isGood()?"GOOD":"BAD"),
					tag.getName(),tag.getValue().getValue(),
					dateFormatter.format(tag.getValue().getTimestamp()));
				// The subscription may be to the fully qualified tag path
				// and/or the path assuming the default provider
				List<BlockPropertyPair> list1 = blockMap.get(tp.toStringFull());
				List<BlockPropertyPair> list2 = blockMap.get(tp.toStringPartial());
				List<BlockPropertyPair> list = new ArrayList<>();
				if( list1!=null ) list.addAll(list1);
				if( list2!=null ) list.addAll(list2);
				if( list.size()==0 ) {
					log.warnf("%s.tagChanged: %s - found no targets for %s or %s -- unsubscribing",TAG,tp.toStringPartial(),tp.toStringFull());
					stopSubscription(tp.toStringFull());
					blockMap.remove(tp.toStringFull());
					blockMap.remove(tp.toStringPartial());
					return;
				}
				for(BlockPropertyPair key:list) {
					ProcessBlock block = key.getBlock();
					BlockProperty property = key.getProperty();
					// Reject blocks that are in a disabled diagram
					ProcessDiagram parent = controller.getDiagram(block.getParentId());
					if( !parent.getState().equals(DiagramState.DISABLED)) {
						updateProperty(block,property,tag.getValue());
					}
				}			
			}
			catch(Exception ex) {
				log.error(TAG+".tagChanged exception ("+ex.getMessage()+")",ex);
			}
		}
		else {
			// For some reason every other update is a null property.
			log.tracef("%s.tagChanged: %s got a null ... ignored",TAG,(tp==null?"null":tp.toStringFull()));
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
				log.infof("%s.tagChanged: property change for %s:%s",TAG,block.getName(),property.getName());
				PropertyChangeEvaluationTask task = new PropertyChangeEvaluationTask(block,
								new BlockPropertyChangeEvent(block.getBlockId().toString(),property.getName(),property.getValue(),value.getValue()));
				Thread propertyChangeThread = new Thread(task, "PropertyChange");
				propertyChangeThread.start();
			}
			else if( property.getBindingType().equals(BindingType.TAG_READ) ||
					 property.getBindingType().equals(BindingType.TAG_READWRITE)) {
					// The tag subscription acts as a pseudo input
					IncomingNotification notice = new IncomingNotification(value);
					threadPool.execute(new IncomingValueChangeTask(block,notice));	
			}
			else {
				log.warnf("%s.tagChanged: %s property no longer bound (%s)",TAG,property.getName(),property.getBindingType());
			}
		}
		catch(Exception ex) {
			log.warnf("%s.tagChanged: Failed to execute change event (%s)",TAG,ex.getLocalizedMessage()); 
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
				return (int)(this.block.hashCode()+this.property.getName().hashCode());
			}
		}
}
