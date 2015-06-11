/**
 *   (c) 2013-2015  ILS Automation. All rights reserved.
 *
 */
package com.ils.blt.gateway.tag;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.sqltags.model.Tag;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
import com.inductiveautomation.ignition.common.sqltags.parser.TagPathParser;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.sqltags.TagProvider;
import com.inductiveautomation.ignition.gateway.sqltags.model.BasicAsyncWriteRequest;
import com.inductiveautomation.ignition.gateway.sqltags.model.WriteRequest;

/**
 *  A Tag writer updates tags with diagram outputs. 
 *  WARNING: Access this class through the BlockExecutionController
 *           interfaces. The controller munges tag paths depending
 *           on the state of the diagram (e.g. ISOLATION mode).
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
		if( context==null) return;                             // Not initialized yet.
		if(path==null || path.isEmpty() || qv==null ) return;  // Path or value not set
		try {
			TagPath tp = TagPathParser.parse(path);

			String providerName = providerNameFromPath(path);
			if( providerName.length()==0) providerName = context.getProjectManager().getProps(projectId, ProjectVersion.Published).getDefaultSQLTagsProviderName();
			TagProvider provider = context.getTagManager().getTagProvider(providerName);
			// We assume the same provider
			if( provider!= null && qv.getValue()!=null ) {
				log.debugf("%s..updateTag: writing %s = %s",TAG,path,qv.getValue().toString());
				Tag tag = provider.getTag(tp);
				if( tag!=null ) {
					// Coerce to the proper datatype
					DataType dtype = tag.getDataType();
					Object value = qv.getValue();
					String strValue = value.toString();
					if(      dtype==DataType.Float4 ||
							dtype==DataType.Float8 )     value = Double.parseDouble(strValue);
					else if( dtype==DataType.Int1 ||
							dtype==DataType.Int2 ||
							dtype==DataType.Int4 ||
							dtype==DataType.Int8   )     value = (int)Double.parseDouble(strValue);
					else if( dtype==DataType.Boolean)    value = Boolean.parseBoolean(strValue);
					else value = strValue;

					LocalRequest request = new LocalRequest(tp,value,qv.getQuality());
					List<WriteRequest<TagPath>> list = createTagList(request);
					provider.write(list, null, true);    // true-> isSystem to bypass permission checks
				}
				else {
					log.warnf("%s.updateTags: Provider %s did not find tag %s",TAG,providerName,path);
				}
			}
			else {
				log.warnf("%s.updateTag: write to %s failed for provider %s",TAG,path,providerName);
			}
		}
		catch( IOException ioe) {
			log.warnf(TAG+"%s.localRequest: parse exception for path %s (%s)",TAG,path,ioe.getMessage());
		}
		catch(NumberFormatException nfe) {
			log.warn(TAG+".updateTag: NumberFormatException setting "+path+" to "+qv.getValue().toString()+"("+nfe.getLocalizedMessage()+")");
			return;
		}
		catch(Exception ex) {
			log.warn(TAG+".updateTags: Exception ("+ex.getLocalizedMessage()+")");
		}
	}

	/**
	 * Determine if the tag path is valid - if the provider name is not supplied, then use the default
	 * assigned to the project. The time assigned is the current
	 * time. The list of tags to be updated varies with model type.
	 * 
	 * @param path fully qualified tag path
	 * @return trueif this tag is managed by the tag manager
	 */
	public boolean validateTag(long projectId,String path) {
		log.debugf("%s..validateTag: %s",TAG,path);
		if( context==null) return true;
		// Not initialized yet.
		boolean result = false;
		if(path==null || path.isEmpty() ) return result;  // Path or value not set
		try {
			TagPath tp = TagPathParser.parse(path);

			String providerName = providerNameFromPath(path);
			if( providerName.length()==0) providerName = context.getProjectManager().getProps(projectId, ProjectVersion.Published).getDefaultSQLTagsProviderName();
			TagProvider provider = context.getTagManager().getTagProvider(providerName);
			// We assume the same provider
			if( provider!= null  ) {
				Tag tag = provider.getTag(tp);
				if( tag!=null ) {
					result = true;
				}
				else {
					log.warnf("%s.validateTag: Provider %s did not find tag %s",TAG,providerName,path);
				}
			}
			else {
				log.warnf("%s.validateTag: no provider for %s ",TAG,path);
			}
		}
		catch( IOException ioe) {
			log.warnf(TAG+"%s.localRequest: parse exception for path %s (%s)",TAG,path,ioe.getMessage());
		}
		catch(Exception ex) {
			log.warn(TAG+".validateTag: Exception ("+ex.getLocalizedMessage()+")");
		}
		return result;
	}

	/** 
	 * Tediously create a list of desired tag outputs. (We have only one).
	 */
	private List<WriteRequest<TagPath>> createTagList(LocalRequest request) {
		List<WriteRequest<TagPath>> list = new ArrayList<WriteRequest<TagPath>>();
		list.add(request);
		return list;
	}
	
	/**
	 * Create a tag write request. 
	 */
	private class LocalRequest extends BasicAsyncWriteRequest<TagPath> {

		public LocalRequest(TagPath tagpath,Object value,Quality quality) {
			super();
			{
				this.setTarget(tagpath);
				this.setValue(value);
				this.setResult(quality);
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
