/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.List;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayInterface;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
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
	private static final LoggerEx log = LogUtil.getLogger(ResourceUpdateManager.class.getPackage().getName());
	private static final boolean DEBUG = true;
	private static DesignerContext context = null;
	private static NodeStatusManager statusManager = null;
	private final byte[] bytes;
	private ProjectResource resource;
	private final ApplicationRequestHandler requestHandler;
	
	// DiagramWorkspace.onClose of a tab.
	public ResourceUpdateManager(ProjectResource pr) {
		this.resource = pr;
		this.bytes = pr.getData();
		this.requestHandler = new ApplicationRequestHandler();
	}
	
	/**
	 * This constructor is not valid for diagram updates
	 * @param pr
	 */
	public ResourceUpdateManager(ProjectResource pr,byte[] contents) {
		this.resource = pr;
		this.bytes = contents;
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
		// Ignore any "system" resources
		if( resource.getResourcePath().getParent()==null ) return;
		
		synchronized(this) {
			ProjectResourceBuilder builder = resource.toBuilder();
			builder.clearData();
			if( bytes==null || bytes.length==0 ) {
				builder.setFolder(true);
			}
			else {
				builder.setFolder(false);
				builder.putData(bytes);
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
				statusManager.markResourceClean(resource.getResourceId());
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
