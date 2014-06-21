/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *   The tag factory is designed to be called from the client
 *   via RPC. The client presents the same interface to scripting functions.
 */
package com.ils.blt.gateway.engine;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.parser.TagPathParser;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.sqltags.TagProvider;
import com.inductiveautomation.ignition.gateway.sqltags.model.BasicAsyncWriteRequest;
import com.inductiveautomation.ignition.gateway.sqltags.model.WriteRequest;

/**
 *  A Tag writer updates tags with model outputs. The tag path structure
 *  is "well-known" as is the list of parameters to be written.
 */
public class TagWriter  {
	private static final String TAG = "TagWriter";
	private final LoggerEx log;
	private GatewayContext context=null;
	
	/**
	 * Constructor.
	 */
	public TagWriter() {
		log = LogUtil.getLogger(getClass().getPackage().getName());	
	}
	
	/**
	 * The context is set sometime after the instance is created.
	 * @param ctx the Gateway context
	 */
	public void start(GatewayContext ctx) {
		this.context = ctx;
	}
	/**
	 * Update tags with values from model results. The time assigned is the current
	 * time. The list of tags to be updated varies with model type.
	 * 
	 * @param provider tag provider. Use an empty string for the default provider
	 * @param path fully qualified tag path
	 */
	public void updateTag(String path,QualifiedValue qv) {
		if( context==null) return;                   // Not initialized yet.
		if(path==null || path.isEmpty() ) return;    // Path not set
		List<WriteRequest<TagPath>> list = createTagList(path,qv);
		if(list.size()==0) log.info(TAG+".updateTags: No results");
		try {
		    TagProvider provider = context.getTagManager().getTagProvider("");
		    // We assume the same provider
		    if( provider!= null && list!=null ) provider.write(list, null, true);	
		}
		catch(Exception ex) {
			log.warn(TAG+".updateTags: Exception ("+ex.getLocalizedMessage()+")");
		}
	}


	/** 
	 * Tediously create a list of desired tag outputs. (We have only one).
	 */
	private List<WriteRequest<TagPath>> createTagList(String path,QualifiedValue qv) {
		List<WriteRequest<TagPath>> list = new ArrayList<WriteRequest<TagPath>>();
		LocalRequest req = null;
		log.infof("%s.createTagList: path = %s",TAG,path);
		req = new LocalRequest(path,qv);
		if(req.isValid)list.add(req);

		return list;
	}
	/**
	 * Create a tag write request. 
	 */
	private class LocalRequest extends BasicAsyncWriteRequest<TagPath> {
		public boolean isValid = false;
		public LocalRequest( String path,QualifiedValue qv) {
			super();
			if( qv!=null ) {
				try {
				    TagPath tp = TagPathParser.parse(path);
				    if( log.isTraceEnabled()) log.tracef("%s.localRequest: adding %s",TAG,tp.toStringFull());
				    this.setTarget(tp);
				    this.setValue(qv.getValue());
				    this.setResult(qv.getQuality());
				    this.isValid = true;
				}
				catch( IOException ioe) {
					log.warnf(TAG+"%s.localRequest: parse exception for path %s (%s)",TAG,path,ioe.getMessage());
				}
			}
		}
	}
}
