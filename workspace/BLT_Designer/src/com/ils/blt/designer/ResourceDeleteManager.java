package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Optional;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.project.DesignableProject;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;


/**
 * Delete the project resources and NavTreeNodes at or below the specified nodes.
 * We do not support undo() as the InductiveAutomation project system already supports 
 * project versions.
 */
public class ResourceDeleteManager implements Runnable {
	private static final String CLSS = "ResourceDeleteManager";
	private final LoggerEx log;
	private static DesignerContext context = null;
	private static NodeStatusManager statusManager = null;
	private final AbstractResourceNavTreeNode root;   // The highest node of the tree to delete.
	private final List<ProjectResource> resources;

	public ResourceDeleteManager(AbstractResourceNavTreeNode treeNode) {
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.root = treeNode;
		this.resources = new ArrayList<>();
		// Add the current resource, then its children
		acquireResourcesToDelete();
	}

	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param ctx designer context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
		statusManager = NodeStatusManager.getInstance();
	}

	public void acquireResourcesToDelete() {
		resources.clear();    		// Get up-to-date list each time we run.
		accumulateResources(root);  // Root and its children
	}

	/**
	 * Now that the individual project resources have been acquired. create a change operation for each one then apply.
	 * As we do this, inform the status manager.
	 */
	public void run() {
		DesignableProject dp = context.getProject();
		for( ProjectResource res: resources) {
			try {
				dp.deleteResource(res.getResourceId());
				statusManager.removeResource(res.getResourceId());
			}
			catch(ResourceNotFoundException rnf) {
				log.warnf("%s.run: Project resource not found %s:%s (%s)",CLSS,res.getResourceId().getProjectName(),
						res.getResourceId().getResourcePath().getPath().toString(),rnf.getMessage());
			}
			catch(Exception ex) {
				log.warnf("%s.run: Exception deleting resource %s:%s (%s)",CLSS,res.getResourceId().getProjectName(),
						res.getResourceId().getResourcePath().getPath().toString(),ex.getMessage());
			}
		}
	}



	// Recursively descend the node tree, gathering up descendants.
	// While we're at it, prepare the nodes for deletion.
	// We add at the beginning of the list so that we delete bottom-up
	private void accumulateResources(AbstractResourceNavTreeNode node) {
		if( node.getProjectResource()!=null ) { 
			Optional<ProjectResource> option = node.getProjectResource();
			if( option.isPresent()) {
				resources.add(0,option.get());
			}
		}

		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();   // Note: actually instantiates the nav tree node
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			accumulateResources((AbstractResourceNavTreeNode)child);
		}
	}
}
