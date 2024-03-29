/**
 *   (c) 2014-2016  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.Enumeration;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.navtree.NavTreeNodeInterface;
import com.ils.blt.designer.workspace.DiagramWorkspace;
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
	
	public ResourceSaveManager(DiagramWorkspace wksp,AbstractResourceNavTreeNode node) {
		this.root = node;
		this.workspace = wksp;
		this.counter.incrementCount();
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
		saveOpenDiagrams(root);
	}
	
	@Override
	public void run() {
		int dirtyCount = saveNodeAndDescendants();
		// Update UI
		if( dirtyCount>0 ) {
			((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler().triggerStatusNotifications();
		}
		this.counter.decrementCount();
	}
	
	// Recursively descend the node tree, looking for diagram resources where
	// the associated DiagramView is open. These are the only diagrams that
	// can be out-of-sync with the gateway.
	private void saveOpenDiagrams(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		node.setItalic(false);
		if( res!=null ) {
			if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				log.debugf("%s.saveOpenDiagrams found: %s (%d) %s",CLSS,res.getName(),res.getResourceId(),
						  (context.getProject().isResourceDirty(res.getResourceId())?"DIRTY":"CLEAN"));
				// If the resource is open, we need to save it
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(res.getResourceId());
				if( tab!=null ) {
					new ResourceUpdateManager(workspace,res).run();
					log.infof("%s.saveOpenDiagrams: saved %s",CLSS,res.getName());
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
		Project diff = context.getProject().getEmptyCopy();

		// Scoop up the dirty nodes (that aren't deleted).
		int dirtyCount = accumulateDirtyNodeResources(root,diff);
		// Update the project with these nodes (informs the gateway also)
		try {
			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ...");  // Do not publish
			for(ProjectResource res:diff.getResources()) {
				log.infof("%s.saveNodeAndDescendants: Saved %s (%d)",CLSS,res.getName(),res.getResourceId());
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
