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
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
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
 *  The data collector waits for inputs on a collection of tags and,
 *  whenever a tag value changes, the collector posts a change notice
 *  task directly to the block for which it is a listening proxy.
 */
public class TagListener implements TagChangeListener   {
	private static final String TAG = "TagListener";
	private static int THREAD_POOL_SIZE = 10;   // Notification threads
	private final LoggerEx log;
	private GatewayContext context = null;
	private final Map<String,List<ProcessBlock>> blockMap;  // Executable blocks keyed by tag path
	private final SimpleDateFormat dateFormatter;
	private boolean stopped = true;
	private final BlockExecutionController controller;
	private final ExecutorService threadPool;
	
	/**
	 * Constructor: 
	 */
	public TagListener(BlockExecutionController ec) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.blockMap = new HashMap<String,List<ProcessBlock>>();
		this.dateFormatter = new SimpleDateFormat(BlockConstants.TIMESTAMP_FORMAT);
		this.controller = ec;
		this.threadPool = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
	}

	/**
	 * Define a tag subscription based on a block attribute. The subject attribute must be
	 * one associated with a tag. If we are running, start the subscription.
	 */
	public void defineSubscription(ProcessBlock block,BlockProperty property) {
		if( block==null || property==null || property.getBindingType()!=BindingType.TAG_READ ) return;
		log.infof("%s.defineSubscription: considering %s:%s",TAG,block.getName(),property.getName());
		String tagPath = property.getBinding();
		if( tagPath!=null && tagPath.length() >0 && property.isEditable() ) {
			if( blockMap.get(tagPath) == null ) blockMap.put(tagPath, new ArrayList<ProcessBlock>());
			List<ProcessBlock> blocks = blockMap.get(tagPath);
			if( blocks.contains(block) ) {
				log.infof("%s.defineSubscription: share %s:%s on tag path %s",TAG,block.getName(),property.getName(),tagPath);
				return;    // We already have a subscription
			}
			blocks.add(block);
			if(!stopped) startSubscriptionForProperty(block,property,tagPath);
		}
	}

	/**
	 * Remove a subscription based on a tag path. Unsubscribe if this
	 * was the last reference to the path for any block.
	 * 
	 * @param tagPath
	 */
	public void removeSubscription(ProcessBlock block,String tagPath) {
		if( tagPath==null) return;    // There was no subscription

		List<ProcessBlock> blocks = blockMap.get(tagPath);
		blocks.remove(block);
		if(blocks.isEmpty()) {
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
	 * Unsubscribe to a path. Does not modify the map.
	 * @param tagPath
	 */
	public void stopSubscription(String tagPath) {
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
			List<ProcessBlock> blocks = blockMap.get(tagPath);
			for(ProcessBlock block:blocks ) {
				for( String name: block.getPropertyNames()) {
					BlockProperty property = block.getProperty(name);
					if( property.getBindingType()==BindingType.TAG_READ && 
					    property.getBinding().equals(tagPath )        ) {
						startSubscriptionForProperty(block,property,tagPath);
					}
				}
			}
		}
		stopped = false;
	}
	
	private void startSubscriptionForProperty(ProcessBlock block,BlockProperty property,String tagPath) {
		SQLTagsManager tmgr = context.getTagManager();
		try {
			TagPath tp = TagPathParser.parse(tagPath);
			log.infof("%s.startSubscriptionForProperty: for %s on tag path %s",TAG,property.getName(),tp.toStringFull());
			// Make sure the attribute is in canonical form
			property.setBinding( tp.toStringFull());
			Tag tag = tmgr.getTag(tp);
			if( tag!=null ) {
				QualifiedValue value = tag.getValue();
				log.infof("%s.startSubscriptionForProperty: got a %s value for %s (%s at %s)",TAG,
						(value.getQuality().isGood()?"GOOD":"BAD"),
						tag.getName(),value.getValue(),
						dateFormatter.format(value.getTimestamp()));
				// Do not pass along nulls -- tag was never set
				if(value.getValue()!=null ) {
					try {
						log.debugf("%s.startSubscriptionForProperty: %s for %s:%s",TAG,block.getName(),property.getBindingType().name(),property.getName());
						// There are two types of bindings here
						if( property.getBindingType().equals(BindingType.TAG_MONITOR)) {
							// The tag change updates the property value
							PropertyChangeEvaluationTask task = new PropertyChangeEvaluationTask(block,
									new BlockPropertyChangeEvent(block.getBlockId().toString(),property.getName(),property.getValue(),value.getValue()));
								Thread propertyChangeThread = new Thread(task, "PropertyChange");
								propertyChangeThread.start();
						}
						else if( property.getBindingType().equals(BindingType.TAG_READ)) {
							// The tag subscription acts as a pseudo input
							IncomingNotification notice = new IncomingNotification(value);
							threadPool.execute(new IncomingValueChangeTask(block,notice));		
						}
						else {
							log.warnf("%s.startSubscriptionForProperty: %s property no longer bound (%s)",TAG,property.getName(),property.getBindingType());
						}
					}
					catch(Exception ex) {
						log.warnf("%s.startSubscriptionForProperty: Failed to execute subscription start (%s)",TAG,ex.getLocalizedMessage()); 
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
		TagProp property = event.getTagProperty();
		if( property == TagProp.Value) {
			try {
				log.infof("%s: tagChanged: got a %s value for %s (%s at %s)",TAG,
					(tag.getValue().getQuality().isGood()?"GOOD":"BAD"),
					tag.getName(),tag.getValue().getValue(),
					dateFormatter.format(tag.getValue().getTimestamp()));
				
				List<ProcessBlock> blocks = blockMap.get(tp.toStringFull());
				if( blocks!=null ) {
					
					for(ProcessBlock blk:blocks) {
						// Reject blocks that are in a disabled diagram
						ProcessDiagram parent = controller.getDiagram(blk.getParentId());
						if( parent.getState().equals(DiagramState.DISABLED)) continue;
						
						// Search properties of the block looking for any bound to tag
						for( String name: blk.getPropertyNames()) {
							BlockProperty prop = blk.getProperty(name);
							String path = prop.getBinding().toString();
							if(prop.getBindingType()==BindingType.TAG_READ) {
								if( path.equals(tp.toStringFull()) && prop.getBindingType()==BindingType.TAG_READ ) {
									try {
										// Treat the notification differently depending on the binding
										if( prop.getBindingType().equals(BindingType.TAG_MONITOR)) {
											log.debugf("%s.tagChanged: property change for %s:%s",TAG,blk.getName(),prop.getName());
											PropertyChangeEvaluationTask task = new PropertyChangeEvaluationTask(blk,
													new BlockPropertyChangeEvent(blk.getBlockId().toString(),prop.getName(),prop.getValue(),tag.getValue().getValue()));
											Thread propertyChangeThread = new Thread(task, "PropertyChange");
											propertyChangeThread.start();
										}
										else if( prop.getBindingType().equals(BindingType.TAG_READ)) {
											// The tag subscription acts as a pseudo input
											IncomingNotification notice = new IncomingNotification(tag.getValue());
											threadPool.execute(new IncomingValueChangeTask(blk,notice));	
										}
										else {
											log.warnf("%s.tagChanged: %s property no longer bound (%s)",TAG,property.getName(),prop.getBindingType());
										}
									}
									catch(Exception ex) {
										log.warnf("%s.tagChanged: Failed to execute change event (%s)",TAG,ex.getLocalizedMessage()); 
									}
								}
							}
						}
					}
					if( blocks.size()==0) {
						log.warnf("%s.tagChanged: No blocks corresponding to tag %s -- unsubscribing",TAG,tag.getName());
						stopSubscription(tp.toStringFull());
						blockMap.remove(tp.toStringFull());
					}
				}
				else {
					log.warnf("%s.tagChanged: Null list of blocks corresponding to tag %s -- unsubscribing",TAG,tag.getName());
					stopSubscription(tp.toStringFull());
					blockMap.remove(tp.toStringFull());
				}			
			}
			catch(Exception ex) {
				log.error(TAG+".tagChanged exception ("+ex.getMessage()+")",ex);
			}
		}
		else {
			// For some reason every other update is a null property.
			log.tracef("%s.tagChanged: %s got a %s property, ... ignored",TAG,(tp==null?"null":tp.toStringFull()),(property==null?"null":property.toString()) );
		}
	}
}
