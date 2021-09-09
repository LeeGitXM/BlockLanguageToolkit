package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Optional;

import com.ils.blt.common.BLTProperties;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayInterface;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;


/**
 * Delete the project resources and NavTreeNodes at or below the specified nodes.
 * We do not support undo() as the InductiveAutomation project system already supports 
 * versioning of the projects.
 */
public class ResourceDeleteManager implements Runnable {
	private static final String CLSS = "ResourceDeleteManager";
	private final ILSLogger log;
	private static DesignerContext context = null;
	private static NodeStatusManager w = null;
	private final AbstractResourceNavTreeNode root;   // The highest node of the tree to delete.
	private final List<ProjectResource> resources;
	private final ThreadCounter counter = ThreadCounter.getInstance();

	public ResourceDeleteManager(AbstractResourceNavTreeNode treeNode) {
		this.log = LogMaker.getLogger(this);
		this.root = treeNode;
		this.resources = new ArrayList<>();
		this.counter.incrementCount();
	}

	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param ctx designer context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
	}

	public void acquireResourcesToDelete() {
		resources.clear();    // Get up-to-date list each time we run.
		accumulateChildResources(root);
	}

	/**
	 * Now that the individual project resources have been acquired. create a change operation for each one then apply.
	 * As we do this, inform the status manager.
	 * 
	 */
	public void run() {
		GatewayInterface gw = GatewayConnectionManager.getInstance().getGatewayInterface();
		for( ProjectResource res: resources) {
			try {
				ChangeOperation.DeleteResourceOperation co = ChangeOperation.DeleteResourceOperation.newDeleteOp(res.getResourceSignature());
				List<ChangeOperation> ops = new ArrayList<>();
				ops.add(co);
				gw.pushProject(ops);
			}
			catch(ResourceNotFoundException rnf) {
				log.warnf("%s.run: Project resource not found %s:%s (%s)",CLSS,res.getResourceId().getProjectName(),
						res.getResourceId().getResourcePath().getPath().toString(),rnf.getMessage());
			}
			catch(Exception ex) {
				log.warnf("%s.run: Exception creating resource %s:%s (%s)",CLSS,res.getResourceId().getProjectName(),
						res.getResourceId().getResourcePath().getPath().toString(),ex.getMessage());
			}
		}
		this.counter.decrementCount();
	}



	// Recursively descend the node tree, gathering up descendants.
	// While we're at it, prepare the nodes for deletion.
	private void accumulateChildResources(AbstractResourceNavTreeNode node) {
		if( node.getProjectResource()!=null ) { 
			Optional<ProjectResource> option = node.getProjectResource();
			if( option.isPresent()) {
				resources.add(option.get());
			}
		}

		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();   // Note: actually instantiates the nav tree node
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			accumulateChildResources((AbstractResourceNavTreeNode)child);
		}
	}
}
