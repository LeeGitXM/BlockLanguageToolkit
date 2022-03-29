/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.List;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
<<<<<<< HEAD
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayInterface;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
=======
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayException;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.IgnitionDesigner;
import com.inductiveautomation.ignition.designer.gateway.DTGatewayInterface;
>>>>>>> master
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;


/**
 * Update the single specified resource.
 * 
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * 
 * @author chuckc
 *
 */
public class ResourceUpdateManager implements Runnable {
	private static final String CLSS = "ResourceUpdateManager";
	private final LoggerEx log;
	private static final boolean DEBUG = true;
	private static DesignerContext context = null;
<<<<<<< HEAD
	private static NodeStatusManager statusManager = null; 
	private ProjectResource resource;
	private final byte[] bytes;
=======
	private static NodeStatusManager statusManager = null;
	private ReentrantLock sharedLock = new ReentrantLock(); 
	private final ProjectResource res;
	private final ProcessDiagramView diagram;
>>>>>>> master
	private final ThreadCounter counter = ThreadCounter.getInstance();
	private final ApplicationRequestHandler requestHandler;
	private DiagramState newState = null;
	

	// DiagramWorkspace.onClose of a tab.
	public ResourceUpdateManager(ProjectResource pr,ProcessDiagramView dia) {
		if(DEBUG) log.infof("%s.run: Creating a new ResourceUpdateManager for %s...", CLSS, dia.getName());
		this.res = pr;
		this.diagram = dia;
		this.counter.incrementCount();
		this.requestHandler = new ApplicationRequestHandler();
	}
	// On a save of a diagram that is not open
	public ResourceUpdateManager(ProjectResource pr,DiagramState state) {
		this.res = pr;
		this.diagram = null;
		this.newState = state;
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
	/**
	 *  Now save the resource, as it is.
	 */
	@Override
	public void run() {
<<<<<<< HEAD
		synchronized(this) {
			ProjectResourceBuilder builder = resource.toBuilder();
			builder.clearData();
			builder.putData(bytes);
			resource = builder.build();
=======

		if( res!=null ) {
			sharedLock.lock();
			// Now save the resource, as it is.
			Project diff = context.getProject().getEmptyCopy();

			/*
			 * Serialize the diagram resource and save it into the project. It should be open on a tab to be considered.
			 * We ignore dirtiness if called from the main menu. If called as a result of closing a tab, the diagram
			 * will be a dirty.
			 */
			long resourceId = res.getResourceId();
			SerializableDiagram sd = null;
			if( diagram!=null) {
					sd = diagram.createSerializableRepresentation();
			}
			else {
					sd = GeneralPurposeTreeNode.deserializeDiagram(res);	
			}

			if( DEBUG ) log.infof("%s.run(), %s", CLSS, sd.getName());
			if( diagram!=null) sd.setName(diagram.getName());
			if(newState!=null) sd.setState(newState);
			ObjectMapper mapper = new ObjectMapper();
			try{
				byte[] bytes = mapper.writeValueAsBytes(sd);
				//log.tracef("%s.run JSON = %s",CLSS,new String(bytes));
				res.setData(bytes);
			}
			catch(JsonProcessingException jpe) {
				log.warnf("%s.run: Exception serializing diagram, resource %d (%s)",CLSS,resourceId,jpe.getMessage());
			}
			
			/*
			 * Now save the resource back into the project.
			 */
>>>>>>> master
			try {
				context.getProject().modifyResource(resource);
				GatewayInterface gw = GatewayConnectionManager.getInstance().getGatewayInterface();
				ChangeOperation.ModifyResourceOperation co = ChangeOperation.ModifyResourceOperation.newModifyOp(resource,resource.getResourceSignature());
				List<ChangeOperation> ops = new ArrayList<>();
				ops.add(co);
				gw.pushProject(ops);
				requestHandler.triggerStatusNotifications(context.getProjectName());
			}
			catch(ResourceNotFoundException rnf) {
				log.warnf("%s.run: Project resource not found %s:%s (%s)",CLSS,resource.getResourceId().getProjectName(),
						resource.getResourceId().getResourcePath().getPath().toString(),rnf.getMessage());
			}
			catch(Exception ex) {
				log.warnf("%s.run: Exception modifying resource %s:%s (%s)",CLSS,resource.getResourceId().getProjectName(),
						resource.getResourceId().getResourcePath().getPath().toString(),ex.getMessage());
			}
		}
		if(DEBUG) log.infof("%s.run(): complete",CLSS);
		this.counter.decrementCount();
	}
}
