/**
 *   (c) 2013-2018  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.tag;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.inductiveautomation.ignition.common.sqltags.TagDefinition;
import com.inductiveautomation.ignition.common.sqltags.model.TagManagerBase;
import com.inductiveautomation.ignition.common.sqltags.model.TagNode;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.model.types.TagType;
import com.inductiveautomation.ignition.common.sqltags.parser.TagPathParser;
import com.inductiveautomation.ignition.common.sqltags.tags.TagDiff;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.sqltags.TagProvider;

/**
 *  The tag handler is a utility for creation and deletion of tags
 *  and associated folders.
 *  
 *  WARNING: Access this class through the BlockExecutionController
 *           interfaces. The controller munges tag paths depending
 *           on the state of the diagram (e.g. ISOLATION mode).
 */
public class TagHandler    {
	private static final String TAG = "TagHandler";
	private static int THREAD_POOL_SIZE = 10;   // Notification threads
	private static final boolean DEBUG = false;
	private final LoggerEx log;
	private GatewayContext context = null;
	
	/**
	 * Constructor: 
	 */
	public TagHandler(GatewayContext ctx) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = ctx;
	}
	public void deleteTag(String path) {
		String providerName = providerFromPath(path);
		path = stripProviderFromPath(path);
		log.infof("%s.deleteTag [%s]%s",TAG,providerName,path);
		TagPath tp = null;
		try {
			tp = TagPathParser.parse(providerName,path);
		}
		catch(IOException ioe) {
			log.warnf("%s: deleteTag: Exception parsing tag [%s]%s (%s)",TAG,providerName,path,ioe.getLocalizedMessage());
			return;
		}
		TagProvider provider = context.getTagManager().getTagProvider(providerName);
		if( provider != null  ) {
			List<TagPath> tags = new ArrayList<TagPath>();
			tags.add(tp);
			try {
				context.getTagManager().removeTags(tags);
			}
			catch(Exception ex) {
				log.warnf("%s: deleteTag: Exception deleting tag [%s]%s (%s)",TAG,providerName,path,ex.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.deleteTag: Provider %s does not exist",TAG,providerName);
		}
	}
	public void renameTag(String name,String path) {
		String providerName = providerFromPath(path);
		path = stripProviderFromPath(path);
		log.infof("%s.renameTag %s [%s]%s",TAG,name,providerName,path);
		TagPath tp = null;
		try {
			tp = TagPathParser.parse(providerName,path);
			TagDiff diff = new TagDiff();
			diff.setName(name);
			List<TagPath> tags = new ArrayList<>();
			tags.add(tp);
			context.getTagManager().editTags(tags,diff);
		}
		catch(IOException ioe) {
			log.warnf("%s: renameTag: Exception parsing tag [%s]%s (%s)",TAG,providerName,path,ioe.getLocalizedMessage());
			return;
		}
		catch(Exception ex) {
			log.warnf("%s: renameTag: Exception renaming tag [%s]%s (%s)",TAG,providerName,path,ex.getLocalizedMessage());
			return;
		}
	}
	
	// We expect the provider name to be bounded by brackets.
	private String providerFromPath(String path) {
		int pos = path.indexOf("]");
		if(pos>0) path = path.substring(1,pos);
		return path;
	}
	
	// If the tag path has a source (provider), strip it off.
	// This is for use with commands that explicitly specify
	// the provider.
	private String stripProviderFromPath(String path) {
		int pos = path.indexOf("]");
		if(pos>0) path = path.substring(pos+1);
		return path;
	}
	
}
