/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *   The tag factory is designed to be called from the client
 *   via RPC. The client presents the same interface to scripting functions.
 */
package com.ils.blt.gateway.engine;

import java.io.IOException;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.sqltags.model.Tag;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.parser.TagPathParser;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.sqltags.TagProvider;

/**
 *  A Tag reader obtains the current value of a tag without
 *  creating a subscription.
 */
public class TagReader  {
	private static final String TAG = "TagReader";
	private final LoggerEx log;
	private GatewayContext context=null;
	
	/**
	 * Constructor.
	 */
	public TagReader() {
		log = LogUtil.getLogger(getClass().getPackage().getName());	
	}
	
	/**
	 * The context is set sometime after the instance is created.
	 * @param ctx the Gateway context
	 */
	public void initialize(GatewayContext ctx) {
		this.context = ctx;
	}
	/**
	 * Update tags with values from model results. The time assigned is the current
	 * time. The list of tags to be updated varies with model type.
	 * 
	 * @param provider tag provider. Use an empty string for the default provider
	 * @param path fully qualified tag path
	 */
	public QualifiedValue readTag(String path) {
		log.infof("%s..readTag: %s",TAG,path);
		if( context==null) return null;                   // Not initialized yet.
		if(path==null || path.isEmpty() ) return null;    // Path not set
		QualifiedValue result = null;
		try {
			TagPath tp = TagPathParser.parse(path);
		    TagProvider provider = context.getTagManager().getTagProvider("default");
		    Tag tag = provider.getTag(tp);
		    result = tag.getValue();
		}
		catch(IOException ioe) {
			log.warnf("%s.readTag: Exception parsing path %s",TAG,path);
		}
		return result;
	}
}
