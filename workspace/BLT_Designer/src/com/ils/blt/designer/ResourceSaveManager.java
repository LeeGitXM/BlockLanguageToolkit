/**
 *   (c) 2014-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Optional;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayInterface;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;


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
	private final LoggerEx log;
	private static final boolean DEBUG = true;
	private static DesignerContext context = null;
	private final AbstractResourceNavTreeNode root;	      // Root of our save.
	private final DiagramWorkspace workspace;
	private final List<ProjectResource> resources;
	private final ThreadCounter counter = ThreadCounter.getInstance();
	private final ApplicationRequestHandler requestHandler;
	private List<ChangeOperation> ops = null;
	
	public ResourceSaveManager(DiagramWorkspace wksp,AbstractResourceNavTreeNode node) {
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.root = node;
		this.workspace = wksp;
		this.counter.incrementCount();
		this.requestHandler = new ApplicationRequestHandler();
		this.resources = new ArrayList<>();
		this.ops = new ArrayList<>();
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param ctx designer context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
	}
	
	/**
	 * Now that the individual project resources have been acquired. create a change operation for each one then apply.
	 * As we do this, inform the status manager.
	 */
	 public void saveResources() {
		 GatewayInterface gw = GatewayConnectionManager.getInstance().getGatewayInterface();
		 for( ProjectResource res: resources) {
			 try {
				 ChangeOperation.ModifyResourceOperation co = ChangeOperation.ModifyResourceOperation.newModifyOp(res,res.getResourceSignature());
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
	 
	/**
	 * Save all application, family or diagram nodes.
	 */
	public void saveAll() {
		accumulateNodeResources(root);
		saveResources();
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
		accumulateNodeResources(root);
		if( ops.size() > 0 ) requestHandler.triggerStatusNotifications(context.getProjectName());
		this.counter.decrementCount();
	}
	
	/**
	 *  Recursively descend the node tree, looking for diagram resources where
	 * the associated DiagramView is open. These are the only diagrams that
	 * can be out-of-sync with the gateway.
	 */
	
	// TODO Make this saveDirtyDiagrams PAH 7/16/21
	private void saveOpenDiagrams(AbstractResourceNavTreeNode node) {
		Optional<ProjectResource>option = node.getProjectResource();
		ProjectResource res = option.get();
		ProcessDiagramView view = null;  // PH 7/16/21
		node.setItalic(false);
		if( res!=null ) {
			if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				
				if( DEBUG ) log.infof("%s.saveOpenDiagrams(), found: %s %s", CLSS, res.getResourceId().getProjectName(),
						  res.getResourceId().getResourcePath().getPath().toString());
				

				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(res.getResourcePath());
				if( tab!=null ) {
					view = (ProcessDiagramView)tab.getModel();
					if( DEBUG ) log.infof("%s.run(), %s (%s)", CLSS, view.getName(), (view.isDirty()?"DIRTY":"CLEAN"));
					if (view.isDirty()){
						if( DEBUG ) log.infof("%s.saveOpenDiagrams: Saving %s...", CLSS, res.getResourcePath().getPath().toString());
						new DiagramUpdateManager(workspace, res).run();
						view.setDirty(false);
						if( DEBUG ) log.infof("%s.saveOpenDiagrams: %s saved!", CLSS, res.getResourcePath().getPath().toString());
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
	

	
	
	// Recursively descend the node tree, gathering all nested resources. Mark all as dirty.
	// Turn off italics.
	private void accumulateNodeResources(AbstractResourceNavTreeNode node) {
		Optional<ProjectResource> option = node.getProjectResource();
		if( option.isPresent() ) { 
			ProjectResource res = option.get();
			if( node instanceof DiagramTreeNode   ) {
				workspace.saveOpenDiagram(res.getResourceId());   // Close if open
			}
		
		}

		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			accumulateNodeResources((AbstractResourceNavTreeNode)child);
		}
	}
}
