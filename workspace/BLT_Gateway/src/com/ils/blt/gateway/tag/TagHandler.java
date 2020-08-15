/**
 *   (c) 2013-2018  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.tag;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.IncomingValueChangeTask;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.PropertyChangeEvaluationTask;
import com.ils.common.tag.BasicILSTagProvider;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
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
