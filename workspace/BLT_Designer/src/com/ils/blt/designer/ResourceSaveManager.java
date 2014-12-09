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
import com.inductiveautomation.ignition.designer.gateway.DTGatewayInterface;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;


/**
 * Search the descendants of the specified node, looking for open diagrams.
 * Close them and save them along with any dirty nodes to the project and gateway.
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * 
 * @author chuckc
 *
 */
public class ResourceSaveManager implements Runnable {
	private static final String TAG = "ResourceSaveManager";
	private static final LoggerEx logger = LogUtil.getLogger(ResourceSaveManager.class.getPackage().getName());
	private static DesignerContext context = null;
	private static NodeStatusManager statusManager = null;
	private final AbstractResourceNavTreeNode root;	      // Root of our save.
	private final DiagramWorkspace workspace;
	
	public ResourceSaveManager(DiagramWorkspace wksp,AbstractResourceNavTreeNode node) {
		this.root = node;
		this.workspace = wksp;
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
	}
	
	/**
	 * Traverse the entire node hierarchy looking for diagrams that need saving.
	 * When found, serialize into the project resource. This is in anticipation
	 * of a top-level save.
	 */
	public void saveSynchronously() {
		saveDirtyDiagrams(root);
	}
	
	@Override
	public void run() {
		saveNodeAndDescendants();
	}
	
	// Recursively descend the node tree, looking for diagram resources where
	// the associated DiagramView is out-of-synch with the project resource.
	// When found update the project resource.
	private void saveDirtyDiagrams(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		if( res!=null ) {
			logger.infof("%s.saveDirtyDiagrams: %s (%d)",TAG,res.getName(),res.getResourceId());
			if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				// If the resource is open, we need to save it
				workspace.saveOpenDiagrams();
			}
		}
		
		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			saveDirtyDiagrams((AbstractResourceNavTreeNode)child);
		}
	}
	


	// Recursively descend the node tree, gathering up associated resources.
	// Since this is used during a save, set the resources clean.
	private void accumulateDirtyNodeResources(AbstractResourceNavTreeNode node,Project diff) {
		ProjectResource res = node.getProjectResource();
		if( res!=null ) {
			long resid = res.getResourceId();
			// For a diagram include either dirty or "dirty children"
			if( node instanceof DiagramTreeNode && 
				( ((NavTreeNodeInterface)node).isDirty() ||
				  statusManager.isResourceDirty(resid)      )  ) {
				logger.infof("%s.accumulateDirtyNodeResources: diagram %s (%d)",TAG,res.getName(),resid);
				diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
				workspace.saveOpenDiagrams();   // Close if open
			}
			// For other nodes include only "dirty"
			else if( node instanceof NavTreeNodeInterface && 
					( ((NavTreeNodeInterface)node).isDirty()  )  ) {
					logger.infof("%s.accumulateDirtyNodeResources: %s (%d)",TAG,res.getName(),resid);
					diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
			}
			statusManager.clearDirtyChildCount(resid);
		}
		else {
			statusManager.clearDirtyChildCount(BLTProperties.ROOT_RESOURCE_ID);
		}
		
		
		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			accumulateDirtyNodeResources((AbstractResourceNavTreeNode)child,diff);
		}
	}
	
	/**
	 * Save the current node and its descendants. During
	 * the accumulation, we set the resources to "clean".
	 */
	private void saveNodeAndDescendants() {
		Project diff = context.getProject().getEmptyCopy();

		// Scoop up the dirty nodes (that aren't deleted).
		accumulateDirtyNodeResources(root,diff);
		// Update the project with these nodes (informs the gateway also)
		try {
			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ...");  // Do not publish
		}
		catch(GatewayException ge) {
			logger.warnf("%s.saveNodeAndDescendants: Exception saving project resource %d (%s)",TAG,root.getProjectResource().getResourceId(),ge.getMessage());
		}
		// Mark these as "clean" in the current project so that we don't save again.
		Project project = context.getProject();
		project.applyDiff(diff,false);      // Apply diff, not dirty
	}
}
