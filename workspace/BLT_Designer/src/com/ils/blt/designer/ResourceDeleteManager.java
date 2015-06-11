package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import com.ils.blt.common.BLTProperties;
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
 * Update the single project resource belonging to the specified node.
 * 
 * 
 * @author chuckc
 *
 */
public class ResourceDeleteManager {
	private static final String TAG = "ResourceDeleteManager";
	private static final LoggerEx logger = LogUtil.getLogger(ResourceDeleteManager.class.getPackage().getName());
	private static DesignerContext context = null;
	private static NodeStatusManager statusManager = null;
	private final AbstractResourceNavTreeNode root;
	private final List<ProjectResource> resources;
	private final long rootId;
	
	public ResourceDeleteManager(AbstractResourceNavTreeNode treeNode) {
		this.root = treeNode;
		if( root.getProjectResource()==null) rootId = BLTProperties.ROOT_RESOURCE_ID;
		else rootId = root.getProjectResource().getResourceId();
		this.resources = new ArrayList<>();
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
		statusManager = ((BLTClassicDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
	}
	
	
	public void deleteInProject() {
		// Before we delete everything, update the status in the parent node
		ProjectResource pres = root.getProjectResource();
		if( pres!=null ) {
			long pid = statusManager.parentResourceId(pres.getResourceId());
			statusManager.decrementDirtyNodeCount(pid);
		}
		
		// Delete the current node and all its children. 
		Project diff = context.getProject().getEmptyCopy();
		for( ProjectResource pr:resources) {
			if( pr.getResourceId()==BLTProperties.ROOT_RESOURCE_ID) continue; 
			logger.infof("%s.deleteInProject: Adding %d to delete list",TAG,pr.getResourceId());
			diff.deleteResource(pr.getResourceId(), true);
			statusManager.deleteResource(pr.getResourceId());  // Prepares the node for deletion
		}

		if( !diff.getDeletedResources().isEmpty() ) {
			// Update the project with these nodes (informs the gateway also)
			try {
				DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ...");  // Do not publish
			}
			catch(GatewayException ge) {
				logger.warnf("%s.deleteInProject: Exception deleting project resources under %d (%s)",TAG,rootId,ge.getMessage());
			}
			
			// Mark these as "clean" in the current project so that we don't save again.
			Project project = context.getProject();
			project.applyDiff(diff,false);      // Apply diff, not dirty
		}
		else {
			logger.errorf("%s.deleteInProject: Resource %d not deleted",TAG,rootId);
		}
		
	}
	public void acquireResourcesToDelete() {
		resources.clear();    // Get up-to-date list each time we run.
		accumulateDescendantResources(root,resources);
	}
	
	// We have grave difficulty here with the locking. Nodes that are newly create appear to be 
	// initially locked by some entity, not us. For now we give up an simply ignore the locks.
	public boolean deleteResources() {
		// First get all the locks
		boolean success = true;
	
//		for(ProjectResource pr:resources) {
//			rid = pr.getResourceId();
//			try {
//				if( !context.requestLockQuietly(rid) ) {
//					success = false;
//					break;
//				}
//			}
//			catch(IllegalArgumentException iae) {
//				logger.errorf("%s.DeleteNodeAction: Exception attempting lock for resource %d (%s)",TAG,rid,iae.getMessage());
//				break;      // Probably hit the resource where we couldn't get the lock
//			}
//		}
		if( success ) {
			for(ProjectResource pr:resources) {
				long rid = pr.getResourceId();
				context.deleteResource(rid);
				logger.infof("%s.DeleteNodeAction: deleted resource %d",TAG,rid);
			}
		}
//		else {
//			logger.errorf("%s.DeleteNodeAction: Failed to obtain lock for resource %d",TAG,rid);
//		}
//		// Release the locks no matter what
//		for(ProjectResource pr:resources) {
//			rid = pr.getResourceId();
//			try {
//				context.updateLock(rid);
//				context.releaseLock(rid);
//			}
//			catch(RuntimeException ignore) {
//				break;      // Probably hit the resource where we couldn't get the lock
//			}
//		}
		return success;
	}

	public boolean undo() {
		// First get all the locks
		boolean success = true;
//		for(ProjectResource pr:resources) {
//			long rid = pr.getResourceId();
//			if( !context.requestLock(rid) ) {
//				success = false;
//				break;
//			}
//		}
		if( success ) {
			for(ProjectResource pr:resources) {
				context.updateResource(pr);	
			}
		}

//		// Release the locks no matter what
//		for(ProjectResource pr:resources) {
//			long rid = pr.getResourceId();
//			context.releaseLock(rid);
//		}
		return success;
	}

	
	// Recursively descend the node tree, gathering up descendants.
	// While we're at it, prepare the nodes for deletion.
	private void accumulateDescendantResources(AbstractResourceNavTreeNode node,List<ProjectResource> rlist) {
		if( node.getProjectResource()!=null ) {
			rlist.add(node.getProjectResource());
		}

		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			accumulateDescendantResources((AbstractResourceNavTreeNode)child,rlist);
		}
	}
}
