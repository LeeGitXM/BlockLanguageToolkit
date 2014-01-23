/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.engine;

import java.beans.PropertyChangeEvent;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BindingType;
import com.ils.block.common.BlockConstants;
import com.ils.block.common.BlockProperty;
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

	private final LoggerEx log;
	private final GatewayContext context;
	private final Map<String,List<ProcessBlock>> blockMap;  // Executable block keyed by tag path
	private final SimpleDateFormat dateFormatter;
	
	/**
	 * Constructor: 
	 * @param ctxt
	 */
	public TagListener(GatewayContext ctxt) {
		this.context = ctxt;
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.blockMap = new HashMap<String,List<ProcessBlock>>();
		this.dateFormatter = new SimpleDateFormat(BlockConstants.TIMESTAMP_FORMAT);
	}

	/**
	 * Start a subscription for a block attribute. The subject attribute must be
	 * one associated with a tag.
	 */
	public void startSubscription(ProcessBlock block,BlockProperty property) {
		if( block==null || property==null ) return;
		
		String tagPath = property.getBinding();
		if( tagPath!=null && tagPath.length() >0 && property.getBindingType()==BindingType.TAG) {
			if( blockMap.get(tagPath) == null ) blockMap.put(tagPath, new ArrayList<ProcessBlock>());
			List<ProcessBlock> blocks = blockMap.get(tagPath);
			if( blocks.contains(block) ) {
				log.debugf("%s: shareSubscription: for %s on tag path %s",TAG,property.getName(),tagPath);
				return;    // We already have a subscription
			}
			blocks.add(block);
			SQLTagsManager tmgr = context.getTagManager();
			try {
				TagPath tp = TagPathParser.parse(tagPath);
				log.debugf("%s: startSubscription: for %s on tag path %s",TAG,property.getName(),tp.toStringFull());
				// Make sure the attribute is in canonical form
				property.setBinding( tp.toStringFull());
				// Initialize the value in this data point
				Tag tag = tmgr.getTag(tp);
				if( tag!=null ) {
					QualifiedValue value = tag.getValue();
					log.debugf("%s: startSubscription: got a %s value for %s (%s at %s)",TAG,
							(value.getQuality().isGood()?"GOOD":"BAD"),
							tag.getName(),value.getValue(),
							dateFormatter.format(value.getTimestamp()));
					try {
						context.getExecutionManager().executeOnce(new PropertyChangeEvaluationTask(block,
							new PropertyChangeEvent(block.getBlockId().toString(),property.getName(),property.getValue(),value.getValue())));
					}
					catch(Exception ex) {
						log.warnf("%s: startSubscription: Failed to execute change event (%s)",TAG,ex.getMessage()); 
					}
				}
				tmgr.subscribe(tp, this);
			}
			catch(IOException ioe) {
				log.errorf("%s: startSubscription (%s)",TAG,ioe.getMessage());
			}
			catch(IllegalArgumentException iae) {
				log.errorf("%s: startSubscription - illegal argument for %s (%s)",TAG,tagPath,iae.getMessage());
			}
		}
	}

	/**
	 * Stop a subscription based on a tag path.
	 * 
	 * @param tagPath
	 */
	public void stopSubscription(String tagPath,ProcessBlock block) {
		if( tagPath==null) return;    // There was no subscription
		SQLTagsManager tmgr = context.getTagManager();
		try {
			TagPath tp = TagPathParser.parse(tagPath);
			log.debug(TAG+"stopSubscription: "+tagPath);
			tmgr.unsubscribe(tp, this);
		}
		catch(IOException ioe) {
			log.error(TAG+"stopSubscription ("+ioe.getMessage()+")");
		}
	}
	
	/**
	 * Unsubscribes to a path. Does not modify the map.
	 * @param tagPath
	 */
	public void stopSubscription(String tagPath) {
		if( tagPath==null) return;    // There was no subscription
		SQLTagsManager tmgr = context.getTagManager();
		try {
			TagPath tp = TagPathParser.parse(tagPath);
			log.debug(TAG+"stopSubscription: "+tagPath);
			tmgr.unsubscribe(tp, this);
		}
		catch(IOException ioe) {
			log.error(TAG+"stopSubscription ("+ioe.getMessage()+")");
		}
	}
	
	/**
	 * Shutdown completely.
	 */
	public void stop() {
		log.infof("%s: stop tagListener, shutdown executor",TAG);
		context.getExecutionManager().shutdown();
		for( String tagPath:blockMap.keySet()) {
			stopSubscription(tagPath);
		}
		blockMap.clear();
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
				log.debugf("%s: tagChanged: got a %s value for %s (%s at %s)",TAG,
					(tag.getValue().getQuality().isGood()?"GOOD":"BAD"),
					tag.getName(),tag.getValue().getValue(),
					dateFormatter.format(tag.getValue().getTimestamp()));
				
				List<ProcessBlock> blocks = blockMap.get(tp.toStringFull());
				if( blocks!=null ) {
					
					for(ProcessBlock blk:blocks) {
						// Search properties of the block looking for any bound to tag
						for( String name: blk.getPropertyNames()) {
							BlockProperty prop = blk .getProperty(name);
							String path = prop.getBinding().toString();
							if( path.equals(tp) && prop.getBindingType()==BindingType.TAG ) {
								try {
									context.getExecutionManager().executeOnce(new PropertyChangeEvaluationTask(blk,
										new PropertyChangeEvent(blk.getBlockId().toString(),prop.getName(),prop.getValue(),tag.getValue())));
								}
								catch(Exception ex) {
									log.warnf("%s: tagChanged: Failed to execute change event (%s)",TAG,ex.getLocalizedMessage()); 
								}
							}
						}
					}
					if( blocks.size()==0) {
						log.warnf("%s: tagChanged: No blocks corresponding to tag %s -- unsubscribing",TAG,tag.getName());
						stopSubscription(tp.toStringFull());
						blockMap.remove(tp.toStringFull());
					}
				}
				else {
					log.warnf("%s: tagChanged: Null list of blocks corresponding to tag %s -- unsubscribing",TAG,tag.getName());
					stopSubscription(tp.toStringFull());
					blockMap.remove(tp.toStringFull());
				}			
			}
			catch(Exception ex) {
				log.error(TAG+"tag changed exception ("+ex.getMessage()+")",ex);
			}
		}
		else {
			// For some reason every other update is a null property.
			log.tracef("%s: tagChanged: %s got a %s property, ... ignored",TAG,(tp==null?"null":tp.toStringFull()),(property==null?"null":property.toString()) );
		}
	}
}
