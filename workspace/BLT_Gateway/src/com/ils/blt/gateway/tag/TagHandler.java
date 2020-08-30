/**
 *   (c) 2013-2018  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.tag;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.ils.common.tag.TagValidator;
import com.inductiveautomation.ignition.common.sqltags.TagDefinition;
import com.inductiveautomation.ignition.common.sqltags.model.TagManagerBase;
import com.inductiveautomation.ignition.common.sqltags.model.TagNode;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.model.types.AccessRightsType;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
import com.inductiveautomation.ignition.common.sqltags.model.types.TagType;
import com.inductiveautomation.ignition.common.sqltags.parser.BasicTagPath;
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
	// Create directories as needed, then a memory tag with correct data type.
	public void createTag(DataType type,String fullpath) {
		String providerName = providerFromPath(fullpath);
		String path = stripProviderFromPath(fullpath);
		log.infof("%s.createTag [%s]%s",TAG,providerName,path);
		TagPath tp = null;
		try {
			tp = TagPathParser.parse(providerName,path);
			// Guarantee that parent paths exist
			createParents(tp);	
			TagDefinition node = new TagDefinition(tp.getItemName(),TagType.DB);
			node.setDataType(type);
			node.setEnabled(true);
			node.setAccessRights(AccessRightsType.Read_Write);    // Or Custom?
			List<TagNode> toAdd = new ArrayList<>();
			toAdd.add(node);
			context.getTagManager().addTags(tp.getParentPath(), toAdd, TagManagerBase.CollisionPolicy.Ignore);
		}
		catch(IOException ioe) {
			log.warnf("%s: createTag: Exception parsing tag [%s]%s (%s)",TAG,providerName,path,ioe.getLocalizedMessage());
			return;
		}
		catch(Exception ex) {
			log.warnf("%s: createTag: Exception creating tag [%s]%s (%s)",TAG,providerName,path,ex.getLocalizedMessage());
			return;
		}
	}
	/**
	 * Rename a tag keeping the folder structure intact. If the tag does not
	 * exist or the rename fails, then create the tag as a String.
	 * @param name new name
	 * @param path existing complete path
	 */
	public void renameTag(String name,String fullpath) {
		String providerName = providerFromPath(fullpath);
		String path = stripProviderFromPath(fullpath);
		log.infof("%s.renameTag %s [%s]%s",TAG,name,providerName,path);
		TagValidator validator = new TagValidator(context);
		if( validator.exists(path) ) {
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
				path = replaceTagNameInPath(name,fullpath);
				createTag(DataType.String,path);
				return;
			}
		}
		else {
			path = replaceTagNameInPath(name,fullpath);
			log.warnf("%s: renameTag: referenced tag [%s] did not exist. %s created",TAG,fullpath,path);
			createTag(DataType.String,path);
			return;
		}
	}
	private void createParents(TagPath path) {
		int segcount = path.getPathLength();
		int seg = 1;
		while(seg<segcount) {
			TagPath tp = BasicTagPath.subPath(path,0, seg);
			log.infof("%s.createParents: Subpath = %s",TAG,tp.toStringFull());
			TagDefinition tag = new TagDefinition(tp.getItemName(),TagType.Folder);
			try {
				List<TagNode> toAdd = new ArrayList<>();
				toAdd.add(tag);
				context.getTagManager().addTags(tp.getParentPath(), toAdd, TagManagerBase.CollisionPolicy.Ignore);
			}
			catch(Exception ex) {
				log.warnf("%s.createParents: Exception creating tag folder %s (%s)",TAG,tp,ex.getLocalizedMessage());
			}
			seg++;
		}
	}
	// We expect the provider name to be bounded by brackets.
	private String providerFromPath(String path) {
		int pos = path.indexOf("]");
		if(pos>0) path = path.substring(1,pos);
		else path="";
		return path;
	}
	// We expect the provider name to be bounded by brackets.
	private String replaceTagNameInPath(String name,String path) {
		int pos = path.lastIndexOf("/");
		if( pos<0 ) pos = path.lastIndexOf("]");
		if( pos<0 ) {
			path = name;
		}
		else {
			path = path.substring(0,pos+1)+name;
		}
		return path;
	}
	// If the tag path has a source (provider), strip it off.
	// This is for use with commands that explicitly specify
	// the provider.
	public String stripProviderFromPath(String path) {
		int pos = path.indexOf("]");
		if(pos>0) path = path.substring(pos+1);
		return path;
	}
	
}
