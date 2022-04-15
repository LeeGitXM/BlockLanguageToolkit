/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.Enumeration;
import java.util.Optional;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
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
 * NOTE: There is a lot of dead code here. I believe only saveSynchronously() is ever used.
 * 
 * @author chuckc
 *
 */
public class ResourceSaveManager implements Runnable {
	private static final String CLSS = "ResourceSaveManager";
	private final LoggerEx log;
	private static final boolean DEBUG = true;
	private static DesignerContext context = null;
	private final AbstractResourceNavTreeNode root;	      // Root of our save.
	private final DiagramWorkspace workspace;
	private final ApplicationRequestHandler requestHandler;
	private static NodeStatusManager statusManager;
	
	public ResourceSaveManager(DiagramWorkspace wksp,AbstractResourceNavTreeNode node) {
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.root = node;
		this.workspace = wksp;
		this.requestHandler = new ApplicationRequestHandler();
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param ctx designer context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
	}
	

	/**
	 * Traverse the entire node hierarchy looking for diagrams that need saving.
	 * When found, serialize into the project resource. This is in anticipation
	 * of a top-level save. This method is called from the designer hook and 
	 * runs in the foreground,
	 */
	public void saveSynchronously() {
		if( DEBUG ) log.infof("%s.saveSynchronously()", CLSS);
		saveModifiedResources(root);
	}
	
	@Override
	public void run() {
		if( DEBUG ) log.infof("%s.run()", CLSS);
		saveModifiedResources(root);
	}

	/**
	 *  Recursively descend the node tree, looking for resources in need of saving.
	 *  These are the cases:
	 *  1) Any diagram that is open and "dirty". (Only open diagrams can be dirty).
	 *  2) Diagrams that are in a different state than the gateway version
	 *  3) Either a folder or digram that is new (i.e. never edited)
	 */

	private void saveModifiedResources(AbstractResourceNavTreeNode node) {
		Optional<ProjectResource>option = node.getProjectResource();
		if(option.isPresent()) {
			ProjectResource res = option.get();
			ProcessDiagramView view = null;
			node.setItalic(false);

			if( res!=null ) {
				if(res.getResourcePath().getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {	
					BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(res.getResourcePath());
					if( tab!=null ) {
						view = (ProcessDiagramView)tab.getModel();
						if( DEBUG ) log.infof("%s.saveModifiedResources, %s (%s)", CLSS, view.getName(), (view.isDirty()?"DIRTY":"CLEAN"));
						if (view.isDirty()){
							view.registerChangeListeners();     // The diagram may include new components
							if( DEBUG ) log.infof("%s.saveModifiedResources: Saving %s...", CLSS, view.getName());
							new ResourceUpdateManager(res,view.createSerializableRepresentation().serialize()).run();
						}
						view.setClean();
						workspace.setDiagramClean(view);

					}
					// The resource is also dirty if the state does not match its counterpart in the gateway
					// If there is a mismatch, simply set the correct state directly into the gateway
					DiagramState designerState = statusManager.getResourceState(res.getResourceId());
					if(designerState!=null ) {
						DiagramState gwState = requestHandler.getDiagramState(res.getResourceId());
						if( !designerState.equals(gwState)) {
							requestHandler.setDiagramState(res.getResourceId(), designerState.name());
							new ResourceUpdateManager(res).run();
						}
					}
				}
				// We also need to save newly created resources of any type
				else if(node.isItalic()) {
					node.setItalic(false);
					new ResourceUpdateManager(res).run();

				}
			}

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				Object child = walker.nextElement();
				saveModifiedResources((AbstractResourceNavTreeNode)child);
			}
		}
	}
}
