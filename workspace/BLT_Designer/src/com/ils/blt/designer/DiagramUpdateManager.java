/**
 *   (c) 2014-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayInterface;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;


/**
 * Update or add the specified project resource which must be a diagram..
 * 
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * Implement a locking scheme to prevent concurrent execution.
 * 
 * @author chuckc
 *
 */
public class DiagramUpdateManager implements Runnable {
	private static final String CLSS = "DiagramUpdateManager";
	private final LoggerEx log;
	private static final boolean DEBUG = true;
	private static DesignerContext context = null; 
	private ProjectResource res;
	private final DiagramWorkspace workspace;
	private final ThreadCounter counter = ThreadCounter.getInstance();
	private final ApplicationRequestHandler requestHandler;

public DiagramUpdateManager(DiagramWorkspace wksp,ProjectResource pr) {
	this.log = LogUtil.getLogger(getClass().getPackageName());
	if(DEBUG) log.infof("%s.run: Creating a new ResourceUpdateManager for DiagramWorkspace %s...", CLSS, wksp.getName());
	this.workspace = wksp;
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
}
/**
 *  Now save the resource, as it is.
 */
@Override
public void run() {
	if( res!=null && res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
			ProcessDiagramView view = null;
			BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(res.getResourcePath());
			if( tab!=null ) {
				// If the diagram is open on a tab, call the workspace method to update the project resource
				// from the diagram view. This method handles re-paint of the background.
				// The diagram may not have been dirty in a structural sense, but update the resource
				// anyway as block properties may have changed.

				view = (ProcessDiagramView)tab.getModel();
				SerializableDiagram sd = view.createSerializableRepresentation();
				if( DEBUG ) log.infof("%s.run(), %s-%s (%s)", CLSS, view.getName(), sd.getName(), (view.isDirty()?"DIRTY":"CLEAN"));

				if(DEBUG) log.infof("%s.run(): serializing ... %s(%s) %s",CLSS,tab.getName(),res.getResourceId().getProjectName(),
						res.getResourceId().getResourcePath().getPath().toString(),sd.getState().name());
				sd.setName(tab.getName());
				ObjectMapper mapper = new ObjectMapper();
				try{
					byte[] bytes = mapper.writeValueAsBytes(sd);
					ProjectResourceBuilder builder = res.toBuilder();
					builder.clearData();
					builder.putData(bytes);
					res = builder.build();
				}
				catch(JsonProcessingException jpe) {
					log.warnf("%s.run: Exception serializing diagram %s:%s (%s)",CLSS,res.getResourceId().getProjectName(),
							res.getResourceId().getResourcePath().getPath().toString(),jpe.getMessage());
					return;
				}
				view.setDirty(false);
				try {

					context.getProject().modifyResource(res);
					GatewayInterface gw = GatewayConnectionManager.getInstance().getGatewayInterface();
					ChangeOperation.ModifyResourceOperation co = ChangeOperation.ModifyResourceOperation.newModifyOp(res,res.getResourceSignature());
					List<ChangeOperation> ops = new ArrayList<>();
					ops.add(co);
					gw.pushProject(ops);
					requestHandler.triggerStatusNotifications(context.getProjectName());
				}
				catch(ResourceNotFoundException rnf) {
					log.warnf("%s.run: Project resource not found %s:%s (%s)",CLSS,res.getResourceId().getProjectName(),
							res.getResourceId().getResourcePath().getPath().toString(),rnf.getMessage());
				}
				catch(Exception ex) {
					log.warnf("%s.run: Exception modifying resource %s:%s (%s)",CLSS,res.getResourceId().getProjectName(),
							res.getResourceId().getResourcePath().getPath().toString(),ex.getMessage());
				}
			}
		}
	
	if(DEBUG) log.infof("%s.run(): complete",CLSS);
	this.counter.decrementCount();
}
}
