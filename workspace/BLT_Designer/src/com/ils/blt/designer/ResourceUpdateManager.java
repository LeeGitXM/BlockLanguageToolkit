/**
 *   (c) 2014-2016  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.concurrent.locks.ReentrantLock;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayException;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.IgnitionDesigner;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.gateway.DTGatewayInterface;
import com.inductiveautomation.ignition.designer.model.DesignerContext;


/**
 * Update or add the single project resource belonging to the specified node.
 *
 * Note that a diagram is equivalent to a project resource.
 * 
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * Implement a locking scheme to prevent concurrent execution.
 * 
 * @author chuckc
 *
 */
public class ResourceUpdateManager implements Runnable {
	private static final String CLSS = "ResourceUpdateManager";
	private static final LoggerEx log = LogUtil.getLogger(ResourceUpdateManager.class.getPackage().getName());
	private static final boolean DEBUG = true;
	private static DesignerContext context = null;
	private static NodeStatusManager statusManager = null;
	private ReentrantLock sharedLock = new ReentrantLock(); 
	private final ProjectResource res;
	private final DiagramWorkspace workspace;
	private final ThreadCounter counter = ThreadCounter.getInstance();
	private final ApplicationRequestHandler requestHandler;
	
	public ResourceUpdateManager(DiagramWorkspace wksp,ProjectResource pr) {
		if(DEBUG) log.infof("%s.run: Creating a new ResourceUpdateManager for DiagramWorkspace %s...", CLSS, wksp.getName());
		this.workspace = wksp;
		this.res = pr;
		this.counter.incrementCount();
		this.requestHandler = new ApplicationRequestHandler();
	}
	
	/**
	 * This constructor is not valid for diagram updates
	 * @param pr
	 */
	public ResourceUpdateManager(ProjectResource pr) {
		this.workspace = null;
		this.res = pr;
		this.counter.incrementCount();
		this.requestHandler = new ApplicationRequestHandler();
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
			sharedLock.lock();
			// Now save the resource, as it is.
			Project diff = context.getProject().getEmptyCopy();
			ProcessDiagramView view = null;

			if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				// If the resource is open and it is dirty then we need to save it
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(res.getResourcePath());
				if( tab!=null ) {
					// If the diagram is open on a tab, call the workspace method to update the project resource
					// from the diagram view. This method handles re-paint of the background.
					// The diagram may not have been dirty in a structural sense, but update the resource
					// anyway as block properties may have changed.
					view = (ProcessDiagramView)tab.getModel();
					SerializableDiagram sd = view.createSerializableRepresentation();

					if( DEBUG ) log.infof("%s.run(), %s-%s (%s)", CLSS, view.getName(), sd.getName(), (view.isDirty()?"DIRTY":"CLEAN"));
					
					if(DEBUG) log.infof("%s.run(): serializing ... %s(%d) %s",CLSS,tab.getName(),resourceId,sd.getState().name());
					sd.setName(tab.getName());
					ObjectMapper mapper = new ObjectMapper();
					try{
						byte[] bytes = mapper.writeValueAsBytes(sd);
						//log.tracef("%s.run JSON = %s",CLSS,new String(bytes));
						res.setData(bytes);
					}
					catch(JsonProcessingException jpe) {
						log.warnf("%s.run: Exception serializing diagram, resource %d (%s)",CLSS,resourceId,jpe.getMessage());
					}
					view.setDirty(false);
					// This is the culprit PH 06/08/2021
					// Move this down.
					//view.registerChangeListeners();
				}
			}
			try {
				
				if( context.requestLockQuietly(res.getResourceId()) )
				{
					if(DEBUG) log.infof("%s.run(): forcing an update on %s...", CLSS, res.getName());
					context.updateResource(res.getResourceId(),res.getData());   // Force an update

					diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
					DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, true, "Committing ...");  // Do publish
					for(ProjectResource pr:diff.getResources()) {
						if(DEBUG) {
							log.infof("%s.run: Saved %s (%d) %s %s",CLSS,pr.getName(),pr.getResourceId(),
								(context.getProject().isResourceDirty(pr)?"DIRTY":"CLEAN"),(pr.isLocked()?"LOCKED":"UNLOCKED"));
						}
						if( pr.isLocked()) pr.setLocked(false);
					}
					// Make every thing clean again.
					Project project = context.getProject();
					project.applyDiff(diff,false);  // Mark diff resources as dirty in diff and deleted in target 
					project.clearAllFlags();
					context.updateLock(res.getResourceId());
					context.releaseLock(res.getResourceId());
					
					
					if(DEBUG) log.infof("%s.run: released lock",CLSS);
				}
				else {
					log.warnf("%s.run: Updating resource %s, failed to obtain lock (aborted)",CLSS,res.getName());
				}
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s.run: Updating resource %d, it has been deleted (%s)",CLSS,res.getResourceId(),iae.getMessage());
				statusManager.deleteResource(res.getResourceId());
			}
			catch(GatewayException ge) {
				log.warnf("%s.run: Exception saving project resource %d (%s)",CLSS,res.getResourceId(),ge.getMessage());
			}

			requestHandler.triggerStatusNotifications();
			sharedLock.unlock();
			
			// PH 06/28/2021 - This should update the connections for a new block, but if this is an existing view don't we already have a listener registered??
			// This does fix the problem updating connections, but it breaks the property update and reverts to the original value. 
			//if(DEBUG) log.infof("%s.run: registering a new change listener", CLSS);
			//if (view != null) view.registerChangeListeners();
			
			if(DEBUG) log.infof("%s.run(): complete",CLSS);
		}
		this.counter.decrementCount();
	}
}
