/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.tag;

import java.io.IOException;
import java.text.SimpleDateFormat;

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
 *  The tag handler listens for changes to a list of tags. Each tag is associated with
 *  an "Entry" block in the diagnostics diagram. Make this class a singleton as there 
 */
public class TagHandler implements TagChangeListener   {
	private static final String TAG = "DataCollector: ";
	private static final String TIMESTAMP_FORMAT = "yyyy.MM.dd HH:mm:ss.SSS";
	private final SimpleDateFormat dateFormatter;
	private final LoggerEx log;
	private final GatewayContext context;

	/**
	 * Constructor: 
	 * @param context
	 * @param timeout, data collection timeout.
	 */
	public TagHandler(GatewayContext ctxt) {
		this.context = ctxt;
		this.dateFormatter = new SimpleDateFormat(TIMESTAMP_FORMAT);
		log = LogUtil.getLogger(getClass().getPackage().getName());
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
	public void startSubscription(String tagPath) {
		SQLTagsManager tmgr = context.getTagManager();
		try {
			TagPath tp = TagPathParser.parse(tagPath);
			log.debug(TAG+String.format("startSubscription: for tag path %s",tp.toStringFull()));
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
	 *         the current value of the tag.
	 * 
	 */
	@Override
	public TagProp getTagProperty() {
		return TagProp.Value;
	}

	@Override
	public void tagChanged(TagChangeEvent event) {
		TagPath tp = event.getTagPath();
		Tag tag = event.getTag();
		TagProp property = event.getTagProperty();
		if( property == TagProp.Value) {
			try {
				log.debug(TAG+String.format("tagChanged: got a %s value for %s (%s at %s)",
					(tag.getValue().getQuality().isGood()?"GOOD":"BAD"),
					tag.getName(),tag.getValue().getValue(),
					dateFormatter.format(tag.getValue().getTimestamp())));
				//reportValueChanged(tp.toStringFull(),tag.getValue().getTimestamp(),tag.getValue().getValue(),
				//	tag.getValue().getQuality().isGood());
			}
			catch(Exception ex) {
				log.error(TAG+"tag changed exception ("+ex.getMessage()+")",ex);
			}
		}
		else {
			// For some reason every other update is a null property.
			log.trace(TAG+String.format("tagChanged: %s got a %s property, ... ignored",(tp==null?"null":tp.toStringFull()),(property==null?"null":property.toString()) ));
		}
	}
}
