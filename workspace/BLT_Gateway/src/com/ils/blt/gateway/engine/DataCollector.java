/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.engine;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Hashtable;

import com.ils.block.BasicBlock;
import com.ils.block.BlockProperties;
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
	private final Hashtable<String,BasicBlock> blockMap;  // Executable block keyed by tag path
	private final SimpleDateFormat dateFormatter;
	/**
	 * Constructor: 
	 * @param ctxt
	 */
	public DataCollector(GatewayContext ctxt) {
		this.context = ctxt;
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.blockMap = new Hashtable<String,BasicBlock>();
		this.dateFormatter = new SimpleDateFormat(BlockProperties.TIMESTAMP_FORMAT);
	}

	/**
	 * Start a subscription for this data point from its provider.
	 * Ignore data points with no path, as these are assumed to
	 * be derived (or calculated). 
	 * 
	 * Populate the data point with the current tag value. This 
	 * handles the issue of the point never updating, because 
	 * it never changed.
	 * 
	 * If the quality is BAD initially, then its state is BAD immediately.
	 */
	public void startSubscription(BasicBlock block,String propertyName) {
		Hashtable<String,String> property = block.getProperty(propertyName);
		if( property!=null ) {
			String tagPath = property.get(BlockProperties.BLOCK_ATTRIBUTE_TAGPATH);
			if( tagPath!=null) {
				SQLTagsManager tmgr = context.getTagManager();
				try {
					TagPath tp = TagPathParser.parse(tagPath);
					log.debugf("%s: startSubscription: for tag path %s",TAG,tp.toStringFull());
					// Initialize the value in this data point
					Tag tag = tmgr.getTag(tp);
					if( tag!=null ) {
						Object value = tag.getValue().getValue();
						//isGood= tag.getValue().getQuality().isGood();
						//point.timestamp = tag.getValue().getTimestamp();
						//log.trace(TAG+"startSubscription: initial value="+point.value+" at "+point.timestamp.toString());
					}
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
				//reportValueChanged(tp.toStringFull(),tag.getValue().getTimestamp(),tag.getValue().getValue(),
				//	tag.getValue().getQuality().isGood());
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
