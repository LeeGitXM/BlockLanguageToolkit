package com.ils.blt.designer;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.navtree.DiagramTreeNode;
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
 * Update or add the single project resource belonging to the specified node.
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * 
 * @author chuckc
 *
 */
public class ResourceUpdateManager implements Runnable {
	private static final String TAG = "ResourceUpdateManager";
	private static final LoggerEx logger = LogUtil.getLogger(ResourceUpdateManager.class.getPackage().getName());
	private static DesignerContext context = null;
	private static NodeStatusManager statusManager = null;
	private final AbstractResourceNavTreeNode node;
	
	public ResourceUpdateManager(AbstractResourceNavTreeNode treeNode) {
		this.node = treeNode;
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
	}
	
	@Override
	public void run() {
		// Now save the resource, as it is.
		Project diff = context.getProject().getEmptyCopy();
		ProjectResource res = node.getProjectResource();
		if( res==null ) return;
		
		if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
			// If the resource is open, we need to save it
			DiagramTreeNode dnode = (DiagramTreeNode)node;
			dnode.updateOpenResource();  // Serializes open resource
		}
		
		diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
		try {
			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ...");  // Don't publish
		}
		catch(GatewayException ge) {
			logger.warnf("%s.saveDiagram: Exception saving project resource %d (%s)",TAG,res.getResourceId(),ge.getMessage());
		}
		// Make every thing clean again.
		statusManager.clearDirtyChildCount(res.getResourceId());
		Project project = context.getProject();
		project.applyDiff(diff,false);
		
	}
}
