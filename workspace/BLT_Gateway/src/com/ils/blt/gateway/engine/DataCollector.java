/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.engine;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Hashtable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BlockConstants;
import com.ils.block.common.BlockProperty;
import com.ils.block.control.NewValueNotification;
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
public class DataCollector implements TagChangeListener   {
	private static final String TAG = "DataCollector";

	private final LoggerEx log;
	private final GatewayContext context;
	private final Hashtable<String,ProcessBlock> blockMap;  // Executable block keyed by tag path
	private final SimpleDateFormat dateFormatter;
	private ExecutorService executor = Executors.newCachedThreadPool();
	
	/**
	 * Constructor: 
	 * @param ctxt
	 */
	public DataCollector(GatewayContext ctxt) {
		this.context = ctxt;
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.blockMap = new Hashtable<String,ProcessBlock>();
		this.dateFormatter = new SimpleDateFormat(BlockConstants.TIMESTAMP_FORMAT);
	}

	/**
	 * Start a subscription for a block attribute. The subject attribute must be
	 * one associated with a tag.
	 */
	public void startSubscription(ProcessBlock block,String propertyName) {
		BlockProperty property = block.getProperty(propertyName);
		if( property!=null ) {
			String tagPath = property.getValue().toString();
			if( tagPath!=null) {
				SQLTagsManager tmgr = context.getTagManager();
				try {
					TagPath tp = TagPathParser.parse(tagPath);
					log.debugf("%s: startSubscription: for tag path %s",TAG,tp.toStringFull());
					// Make sure the attribute is in canonical form
					property.setValue( tp.toStringFull());
					// Initialize the value in this data point
					Tag tag = tmgr.getTag(tp);
					if( tag!=null ) {
						QualifiedValue value = tag.getValue();
						log.debugf("%s: startSubscription: got a %s value for %s (%s at %s)",TAG,
								(tag.getValue().getQuality().isGood()?"GOOD":"BAD"),
								tag.getName(),tag.getValue().getValue(),
								dateFormatter.format(tag.getValue().getTimestamp()));
						NewValueNotification notification = new NewValueNotification(block,propertyName,value);
						executor.execute(new PropertyChangeEvaluationTask(notification));
					}
					blockMap.put(tp.toStringFull(), block);
					tmgr.subscribe(tp, this);
				}
				catch(IOException ioe) {
					log.error(TAG+"startSubscription ("+ioe.getMessage()+")");
				}
				catch(IllegalArgumentException iae) {
					log.error(TAG+"startSubscription ("+iae.getMessage()+")");
				}
			}
			else {
				log.warnf("%s: startSubscription: tagPath %s for property %s not found",TAG,tagPath,propertyName);
			}
		}
		else {
			log.warnf("%s: startSubscription: property %s not found",TAG,propertyName);
		}
		
	}
	/**
	 * Stop a subscription based on a tag path.
	 * 
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
		executor.shutdown();
		for( String tp:blockMap.keySet()) {
			stopSubscription(tp);
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
				
				ProcessBlock block = blockMap.get(tp.toStringFull());
				if( block!=null ) {
					String propertyName = getPropertyForTagpath(block,tp.toStringFull());
					if( propertyName != null ) {
						NewValueNotification notification = new NewValueNotification(block,propertyName,tag.getValue());
						executor.execute(new PropertyChangeEvaluationTask(notification));
					}
					
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
	
	/**
	 * Search properties of a block looking for one of datatype "tag"
	 * and a particular tag path.
	 */
	private String getPropertyForTagpath(ProcessBlock block,String tp) {
		String result = null;
		for( String name: block.getPropertyNames()) {
			BlockProperty property = block.getProperty(name);
			String path = property.getValue().toString();
			if( path.equals(tp)) {
				result = name;
				break;
			}
		}
		return result;	
	}
}
