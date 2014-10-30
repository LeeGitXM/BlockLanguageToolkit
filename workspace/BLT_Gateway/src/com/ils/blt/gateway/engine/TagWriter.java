/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.engine;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.parser.TagPathParser;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.sqltags.TagProvider;
import com.inductiveautomation.ignition.gateway.sqltags.model.BasicAsyncWriteRequest;
import com.inductiveautomation.ignition.gateway.sqltags.model.WriteRequest;

/**
 *  A Tag writer updates tags with diagram outputs.
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
	public void initialize(GatewayContext ctx) {
		this.context = ctx;
	}
	/**
	 * Update a tag - if the provider name is not supplied, then use the default
	 * assigned to the project. The time assigned is the current
	 * time. The list of tags to be updated varies with model type.
	 * 
	 * @param path fully qualified tag path
	 */
	public void updateTag(long projectId,String path,QualifiedValue qv) {
		log.debugf("%s..updateTag: %s",TAG,path);
		if( context==null) return;                   // Not initialized yet.
		if(path==null || path.isEmpty() ) return;    // Path not set
		List<WriteRequest<TagPath>> list = createTagList(path,qv);
		if(list.size()==0) log.info(TAG+".updateTags: No results");
		try {
			String providerName = providerNameFromPath(path);
			if( providerName.length()==0) providerName = context.getProjectManager().getProps(projectId, ProjectVersion.Published).getDefaultSQLTagsProviderName();
		    TagProvider provider = context.getTagManager().getTagProvider(providerName);
		    // We assume the same provider
		    if( provider!= null && list!=null ) {
		    	log.debugf("%s..updateTag: writing .... %s",TAG,qv.toString());
		    	provider.write(list, null, true);	
		    }
		    else {
		    	log.warnf("%s..updateTag: write to %s failed for provider",TAG,path,providerName);
		    }
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
		log.debugf("%s.createTagList: path = %s",TAG,path);
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
}
