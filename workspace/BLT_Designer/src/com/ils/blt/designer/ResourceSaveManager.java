/**
 *   (c) 2014-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.Enumeration;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.navtree.NavTreeNodeInterface;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayException;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.IgnitionDesigner;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.gateway.DTGatewayInterface;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;


/**
 * Search the descendants of the specified node, looking for open diagrams.
 * Close them and save them along with any dirty nodes to the project and gateway.
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * Do not re-execute the same instance.
 * 
 * @author chuckc
 *
 */
public class ResourceSaveManager implements Runnable {
	private static final String CLSS = "ResourceSaveManager";
	private static final LoggerEx log = LogUtil.getLogger(ResourceSaveManager.class.getPackage().getName());
	private static final boolean DEBUG = true;
	private static DesignerContext context = null;
	private final AbstractResourceNavTreeNode root;	      // Root of our save.
	private final DiagramWorkspace workspace;
	private final ThreadCounter counter = ThreadCounter.getInstance();
	private final ApplicationRequestHandler requestHandler;
	
	public ResourceSaveManager(DiagramWorkspace wksp,AbstractResourceNavTreeNode node) {
		this.root = node;
		this.workspace = wksp;
		this.counter.incrementCount();
		this.requestHandler = new ApplicationRequestHandler();
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param ctx designer context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
	}
	/**
	 * Save all application, family or diagram nodes.
	 */
	public void saveAll() {
		Project diff = context.getProject().getEmptyCopy();
		accumulateNodeResources(root,diff);
		
		try {
			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, true, "Committing/Publishing ...");  // publish		
		}
		catch(GatewayException ge) {
			log.warnf("%s.saveAll: Exception saving project resource %d (%s)",CLSS,root.getProjectResource().getResourceId(),ge.getMessage());
		}
	}
	/**
	 * Traverse the entire node hierarchy looking for diagrams that need saving.
	 * When found, serialize into the project resource. This is in anticipation
	 * of a top-level save. This method is called from the designer hook.
	 */
	public void saveSynchronously() {
		if( DEBUG ) log.infof("%s.saveSynchronously()", CLSS);
		saveOpenDiagrams(root);
	}
	
	@Override
	public void run() {
		if( DEBUG ) log.infof("%s.run()", CLSS);
		int dirtyCount = saveNodeAndDescendants();
		// Update UI
		if( dirtyCount>0 ) {
			requestHandler.triggerStatusNotifications();
		}
		this.counter.decrementCount();
	}
	
	// Recursively traverse the nav tree. Choose only diagrams and then only those that are open.
	// These are the only diagrams that can be out-of-sync with the gateway. Save them. 
	// Note: the notion of dirtiness is simply a UI indicator for the user.
	// We simply save everything that is open.
	private void saveOpenDiagrams(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		ProcessDiagramView view = null;  // PH 7/16/21
		node.setItalic(false);
		if( res!=null ) {
			if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				/*
				 * We might be managing the dirty flag incorrectly!  The dirty check shown below is not consistent with the dirty check
				 * on ProcessDiagramView.  The check below shows the resource as clean but the ProcessDiagramView as dirty.
				 * PAH 07/18/21
				 */
				if( DEBUG ) log.infof("%s.saveOpenDiagrams(), found: %s (%d) %s", CLSS, res.getName(), res.getResourceId(),
						  (context.getProject().isResourceDirty(res.getResourceId())?"DIRTY":"CLEAN"));

				/*
				 * Is there a way to get the view without it being open? No because a ProcessDiagramView doesn't exist if it's not showing.
				 * Also the only way for it to get dirty is for it to be open.
				 */
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(res.getResourceId());
				if( tab!=null ) {
					view = (ProcessDiagramView)tab.getModel();
					if( DEBUG ) log.infof("%s.saveOpenDiagrams, %s (%s)", CLSS, view.getName(), (view.isDirty()?"DIRTY":"CLEAN"));
					if (view.isDirty()){
						view.registerChangeListeners();     // The diagram may include new components
						if( DEBUG ) log.infof("%s.saveOpenDiagrams: Saving %s...", CLSS, res.getName());
						new ResourceUpdateManager(workspace, res,view).run();
						if( DEBUG ) log.infof("%s.saveOpenDiagrams: %s saved!", CLSS, res.getName());
					}
				}
			}
		}
		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			saveOpenDiagrams((AbstractResourceNavTreeNode)child);
		}
	}
	
	// Recursively descend the node tree, gathering up associated resources.
	// Since this is used during a save, set the resources clean.
	private int accumulateDirtyNodeResources(AbstractResourceNavTreeNode node,Project diff) {
		ProjectResource res = node.getProjectResource();
		int dirtyCount = 0;
		if( res!=null ) {
			long resid = res.getResourceId();
			// For a diagram include either dirty or "dirty children"
			if( node instanceof DiagramTreeNode && 
				( ((NavTreeNodeInterface)node).isDirty() || context.getProject().isResourceDirty(resid)  )  ) {
				if( DEBUG ) log.infof("%s.accumulateDirtyNodeResources: diagram %s (%d) %s",CLSS,res.getName(),resid,
						    (context.getProject().isResourceDirty(res.getResourceId())?"DIRTY":"CLEAN"));
				diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
				workspace.saveOpenDiagram(res.getResourceId());   // Close if open
				dirtyCount++;
				
			}
			// For other nodes include only "dirty"
			else if( node instanceof NavTreeNodeInterface && ( ((NavTreeNodeInterface)node).isDirty()  )  ) {
					if( DEBUG ) log.infof("%s.accumulateDirtyNodeResources: %s (%d) %s",CLSS,res.getName(),resid,
							(context.getProject().isResourceDirty(res.getResourceId())?"DIRTY":"CLEAN"));
					diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
					dirtyCount++;
			}
			// No matter what we're going to save it.
			node.setItalic(false);
		}
		
		
		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			dirtyCount += accumulateDirtyNodeResources((AbstractResourceNavTreeNode)child,diff);
		}
		return dirtyCount;
	}
	
	/**
	 * Save the current node and its descendants. During
	 * the accumulation, we set the resources to "clean".
	 */
	private int saveNodeAndDescendants() {
		if( DEBUG ) log.infof("%s.saveNodeAndDescendants()", CLSS);
		Project diff = context.getProject().getEmptyCopy();

		// Scoop up the dirty nodes (that aren't deleted).
		int dirtyCount = accumulateDirtyNodeResources(root,diff);
		// Update the project with these nodes (informs the gateway also)
		try {
			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ...");  // Do not publish
			for(ProjectResource res:diff.getResources()) {
				if( DEBUG ) log.infof("%s.saveNodeAndDescendants: Saved %s (%d)",CLSS,res.getName(),res.getResourceId());
			}

		}
		catch(GatewayException ge) {
			log.warnf("%s.saveNodeAndDescendants: Exception saving project resource %d (%s)",CLSS,root.getProjectResource().getResourceId(),ge.getMessage());
		}

		// Mark these as "clean" in the current project so that we don't save again.
		Project project = context.getProject();
		project.applyDiff(diff,false);      // Apply diff, not dirty
		return dirtyCount;
	}
	
	// Recursively descend the node tree, gathering all nested resources. Mark all as dirty.
	// Turn off italics.
	private void accumulateNodeResources(AbstractResourceNavTreeNode node,Project diff) {
		ProjectResource res = node.getProjectResource();
		if( res!=null ) {
			if( node instanceof DiagramTreeNode   ) {
				workspace.saveOpenDiagram(res.getResourceId());   // Close if open
			}
			diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
		}

		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			accumulateNodeResources((AbstractResourceNavTreeNode)child,diff);
		}
	}

}
