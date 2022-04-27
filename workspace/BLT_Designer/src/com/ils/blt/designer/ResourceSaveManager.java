/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Optional;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;


/**
 * Search the descendants of the specified node, looking for open diagrams.
 * Close them and save them along with any dirty nodes to the project and gateway.
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * Do not re-execute the same instance.
 *
 */
public class ResourceSaveManager implements Runnable {
	private static final String CLSS = "ResourceSaveManager";
	private final LoggerEx log;
	private static final boolean DEBUG = true;
	private static DesignerContext context = null;
	private final AbstractResourceNavTreeNode root;	      // Root of our save.
	private final DiagramWorkspace workspace;
	private final ObjectMapper mapper;
	private final ApplicationRequestHandler requestHandler;
	private final NodeStatusManager statusManager;
	
	public ResourceSaveManager(DiagramWorkspace wksp,AbstractResourceNavTreeNode node) {
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.root = node;
		this.workspace = wksp;
		this.requestHandler = new ApplicationRequestHandler();
		this.statusManager = NodeStatusManager.getInstance();
		this.mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param ctx designer context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
	}
	

	/**
	 * Traverse the entire node hierarchy looking for diagrams that need saving.
	 * When found, serialize into the project resource. This is in anticipation
	 * of a top-level save. This method is called from the designer hook and 
	 * runs in the foreground,
	 */
	public void saveSynchronously() {
		if( DEBUG ) log.infof("%s.saveSynchronously()", CLSS);
		saveModifiedResource(root);
	}
	
	@Override
	public void run() {
		if( DEBUG ) log.infof("%s.run()", CLSS);
		saveModifiedResource(root);
	}

	/**
	 *  Recursively descend the node tree, looking for resources in need of saving.
	 *  These are the cases:
	 *  1) Any diagram that is open and "dirty". (Only open diagrams can be dirty).
	 *  2) Diagrams that are in a different state than the gateway version
	 *  3) A folder or diagram that has been renamed
	 *  4) Either a folder or diagram that is new (i.e. never edited)
	 */
	private void saveModifiedResource(AbstractResourceNavTreeNode node) {
		Optional<ProjectResource>option = node.getProjectResource();
		if(option.isPresent()) {
			ProjectResource res = option.get();
			if( res!=null && !res.getResourcePath().getPath().isRoot()) {
				ProjectResourceId resid = res.getResourceId();
				if( statusManager.getDirtyState(resid)) {
					if( res.isFolder()) {
						if( DEBUG ) log.infof("%s.saveModifiedResources: Saving %s...", CLSS, res.getResourceName());
						new ResourceUpdateManager(res).run();
					}
					// Diagram
					else {
						BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(res.getResourcePath());
						if( tab!=null ) {
							ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
							view = (ProcessDiagramView)tab.getModel();
							if( DEBUG ) log.infof("%s.saveModifiedResources, %s (%s)", CLSS, view.getName(), (view.isDirty()?"DIRTY":"CLEAN"));
							if (view.isDirty()){
								view.registerChangeListeners();     // The diagram may include new components
								if( DEBUG ) log.infof("%s.saveModifiedResource: Saving %s...", CLSS, view.getName());
								new ResourceUpdateManager(res,view).run();
							}
							view.setClean();
							workspace.setDiagramClean(view);
						}
						// Diagram is closed, but still dirty
						else {
							byte[] bytes = res.getData();
							try {
								SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
								if( sd!=null ) {
									ProcessDiagramView view = new ProcessDiagramView(res.getResourceId(),sd, context);
									if( DEBUG ) log.infof("%s.saveModifiedResource: Saving %s...", CLSS, view.getName());
									new ResourceUpdateManager(res,view).run();
								}
							}
							catch(JsonParseException jpe) {
								log.warnf("%s.saveModifiedResource: Parse exception saving %s...(%s)", CLSS, res.getResourceName(),jpe.getLocalizedMessage());
							}
							catch(JsonMappingException jme) {
								log.warnf("%s.saveModifiedResource: Mapping exception saving %s...(%s)", CLSS, res.getResourceName(),jme.getLocalizedMessage());
							}
							catch(IOException ioe) {
								log.warnf("%s.saveModifiedResource: IO exception saving %s...(%s)", CLSS, res.getResourceName(),ioe.getLocalizedMessage());
							}
						}
					}
				}
			}

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				Object child = walker.nextElement();
				saveModifiedResource((AbstractResourceNavTreeNode)child);
			}
		}
	}
}
