package com.ils.blt.designer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.serializable.SerializableDiagram;
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
	private final ProjectResource res;
	private final DiagramWorkspace workspace;
	private static ToolkitRequestHandler applicationRequestHandler = null;
	
	public ResourceUpdateManager(DiagramWorkspace wksp,ProjectResource pr) {
		this.workspace = wksp;
		this.res = pr;
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param context
	 */
	public static void setup(DesignerContext ctx,NodeStatusManager sm,ToolkitRequestHandler apphandle) {
		context = ctx;
		statusManager =sm;
		applicationRequestHandler = apphandle;
	}
	
	@Override
	public void run() {
		if( res==null ) return;
		// Now save the resource, as it is.
		Project diff = context.getProject().getEmptyCopy();
		
		if(res.getResourceType().equals(BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE) ) {
			// If the resource is open, we need to save it
			long resourceId = res.getResourceId();
			BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
			if( tab!=null ) {
				// If the diagram is open on a tab, call the workspace method to update the project resource
				// from the diagram view. This method handles re-paint of the background.
				// The diagram may not have been dirty in a structural sense, but update the resource
				// anyway as block properties may have changed.
				ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
				view.registerChangeListeners();
				logger.infof("%s.run: updating ... %s(%d) ",TAG,tab.getName(),resourceId);
				SerializableDiagram sd = view.createSerializableRepresentation();
				sd.setName(tab.getName());
				ObjectMapper mapper = new ObjectMapper();
				try{
					byte[] bytes = mapper.writeValueAsBytes(sd);
					logger.tracef("%s.run JSON = %s",TAG,new String(bytes));
					res.setData(bytes);
				}
				catch(JsonProcessingException jpe) {
					logger.warnf("%s.run: Exception serializing diagram, resource %d (%s)",TAG,resourceId,jpe.getMessage());
				}
				view.setDirty(false);
			}
		}
		try {
			context.updateResource(res.getResourceId(),res.getData());   // Force an update
			diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ...");  // Don't publish
			// Make every thing clean again.
			statusManager.clearDirtyChildCount(res.getResourceId());
			Project project = context.getProject();
			project.applyDiff(diff,false);
		}
		catch(IllegalArgumentException iae) {
			logger.warnf("%s.run: Updating resource %d, it has been deleted (%s)",TAG,res.getResourceId(),iae.getMessage());
			statusManager.deleteResource(res.getResourceId());
		}
		catch(GatewayException ge) {
			logger.warnf("%s.run: Exception saving project resource %d (%s)",TAG,res.getResourceId(),ge.getMessage());
		}
		applicationRequestHandler.triggerStatusNotifications();
	}
}