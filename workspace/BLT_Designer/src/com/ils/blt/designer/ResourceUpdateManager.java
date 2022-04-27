/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.List;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayInterface;
import com.inductiveautomation.ignition.common.StringPath;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;


/**
 * Update the single specified resource.
 * 
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 *
 */
public class ResourceUpdateManager implements Runnable {
	private static final String CLSS = "ResourceUpdateManager";
	private static final LoggerEx log = LogUtil.getLogger(ResourceUpdateManager.class.getPackage().getName());
	private static final boolean DEBUG = true;
	private static DesignerContext context = null;
	private final ProcessDiagramView view;
	private ProjectResource resource;

	
	/**
	 * Use this form for a folder resource (no data). 
	 * @param pr
	 */
	public ResourceUpdateManager(ProjectResource pr) {
		this.resource = pr;
		this.view = null;
	}
	
	/**
	 * This constructor is used for diagram resources.
	 * @param pr
	 */
	public ResourceUpdateManager(ProjectResource pr,ProcessDiagramView pdv) {
		this.resource = pr;
		this.view = pdv;
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
		ProjectResourceId resid = resource.getResourceId();
		ResourcePath respath = resid.getResourcePath();
		if( respath.getParent()==null ) return;  // Ignore "system" resources
		
		NodeStatusManager statusManager = NodeStatusManager.getInstance();
		if( !statusManager.getDirtyState(resid)) return;  // Not dirty
		String pendingName = statusManager.getPendingName(resid);
		ApplicationRequestHandler requestHandler = new ApplicationRequestHandler();
		
		synchronized(this) {
			ProjectResourceBuilder builder = resource.toBuilder();
			builder.clearData();
			
			if( view==null ) {
				builder.setFolder(true);
			}
			else {
				builder.setFolder(false);
				view.setState(statusManager.getPendingState(resid));
				byte[] bytes = view.createSerializableRepresentation().serialize();
				builder.putData(bytes);
			}
			StringPath sp = respath.getPath();
			String name = sp.getLastPathComponent();
			if(!name.equalsIgnoreCase(pendingName)) {
				sp = StringPath.extend(sp.getParentPath(),pendingName);
				respath = new ResourcePath(BLTProperties.DIAGRAM_RESOURCE_TYPE,sp);
				builder.setResourcePath(respath);
			}
			resource = builder.build();

			try {
				context.getProject().modifyResource(resource);
				GatewayInterface gw = GatewayConnectionManager.getInstance().getGatewayInterface();
				ChangeOperation.ModifyResourceOperation co = ChangeOperation.ModifyResourceOperation.newModifyOp(resource,resource.getResourceSignature());
				List<ChangeOperation> ops = new ArrayList<>();
				ops.add(co);
				gw.pushProject(ops);
				requestHandler.triggerStatusNotifications(context.getProjectName());
				statusManager.commit(resource.getResourceId());
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
	}
}
