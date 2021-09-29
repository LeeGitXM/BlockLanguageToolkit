/**
 *   (c) 2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.List;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
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
	private final LoggerEx log;
	private static final boolean DEBUG = true;
	private static DesignerContext context = null;
	private static NodeStatusManager statusManager = null; 
	private ProjectResource resource;
	private final byte[] bytes;
	private final ThreadCounter counter = ThreadCounter.getInstance();
	private final ApplicationRequestHandler requestHandler;
	
	public ResourceUpdateManager(ProjectResource pr,byte[] contents) {
		this.log = LogUtil.getLogger(getClass().getPackageName());
		if(DEBUG) log.infof("%s.run: Creating a new ResourceUpdateManager ...", CLSS);;
		this.resource = pr;
		this.bytes = contents;
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
	/**
	 *  Now save the resource, as it is.
	 */
	@Override
	public void run() {
		synchronized(this) {
			ProjectResourceBuilder builder = resource.toBuilder();
			builder.clearData();
			builder.putData(bytes);
			resource = builder.build();
			try {
				context.getProject().modifyResource(resource);
				GatewayInterface gw = GatewayConnectionManager.getInstance().getGatewayInterface();
				ChangeOperation.ModifyResourceOperation co = ChangeOperation.ModifyResourceOperation.newModifyOp(resource,resource.getResourceSignature());
				List<ChangeOperation> ops = new ArrayList<>();
				ops.add(co);
				gw.pushProject(ops);
				requestHandler.triggerStatusNotifications();
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
