/**
 *   (c) 2014-2016  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
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
	private static final String CLSS = "ResourceUpdateManager";
	private static final LoggerEx log = LogUtil.getLogger(ResourceUpdateManager.class.getPackage().getName());
	private static DesignerContext context = null;
	private static NodeStatusManager statusManager = null;
	private final ProjectResource res;
	private final DiagramWorkspace workspace;
	private final ThreadCounter counter = ThreadCounter.getInstance();
	
	public ResourceUpdateManager(DiagramWorkspace wksp,ProjectResource pr) {
		this.workspace = wksp;
		this.res = pr;
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
	
	@Override
	public void run() {
		if( res!=null ) {
			// Now save the resource, as it is.
			Project diff = context.getProject().getEmptyCopy();

			if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				// If the resource is open, we need to save it
				long resourceId = res.getResourceId();
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
				if( tab!=null ) {
					// If the diagram is open on a tab, call the workspace method to update the project resource
					// from the diagram view. This method handles re-paint of the background.
					// The diagram may not have been dirty in a structural sense, but update the resource
					// anyway as block properties may have changed.
					ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
					SerializableDiagram sd = view.createSerializableRepresentation();
					log.infof("%s.run: updating ... %s(%d) %s",CLSS,tab.getName(),resourceId,sd.getState().name());
					sd.setName(tab.getName());
					ObjectMapper mapper = new ObjectMapper();
					try{
						byte[] bytes = mapper.writeValueAsBytes(sd);
						log.tracef("%s.run JSON = %s",CLSS,new String(bytes));
						res.setData(bytes);
					}
					catch(JsonProcessingException jpe) {
						log.warnf("%s.run: Exception serializing diagram, resource %d (%s)",CLSS,resourceId,jpe.getMessage());
					}
					view.setDirty(false);
					view.registerChangeListeners();
				}
			}
			try {
				context.updateResource(res.getResourceId(),res.getData());   // Force an update
				diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
				DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ...");  // Don't publish
				for(ProjectResource pr:diff.getResources()) {
					log.infof("%s.run: Saved %s (%d) %s",CLSS,pr.getName(),pr.getResourceId(),
							  (context.getProject().isResourceDirty(pr)?"DIRTY":"CLEAN"));
				}
				// Make every thing clean again.
				Project project = context.getProject();
				project.applyDiff(diff,false);
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s.run: Updating resource %d, it has been deleted (%s)",CLSS,res.getResourceId(),iae.getMessage());
				statusManager.deleteResource(res.getResourceId());
			}
			catch(GatewayException ge) {
				log.warnf("%s.run: Exception saving project resource %d (%s)",CLSS,res.getResourceId(),ge.getMessage());
			}
			((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler().triggerStatusNotifications();
		}
		this.counter.decrementCount();
	}
}
