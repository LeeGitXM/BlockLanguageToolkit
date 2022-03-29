package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.List;

import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayInterface;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;


/**
 * Create a project resource and make it permanent.
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * Do not re-execute the same instance.
 * 
 * @author chuckc
 *
 */
public class ResourceCreateManager implements Runnable {
	private static final String CLSS = "ResourceCreateManager";
	private final LoggerEx log;
	private static DesignerContext context = null;
	private final ProjectResource res;
	private final String resName;
	private final ThreadCounter counter = ThreadCounter.getInstance();

	public ResourceCreateManager(ProjectResource pr,String nam) {
		this.res = pr;
		this.resName = nam;
		this.counter.incrementCount();
		this.log = LogUtil.getLogger(getClass().getPackageName());
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
		if( res!=null ) {
			try {
				context.getProject().createResource(res);
				context.getProject().renameResource(res.getResourcePath(),resName);
				GatewayInterface gw = GatewayConnectionManager.getInstance().getGatewayInterface();
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
				String msg = String.format("%s.run: Exception creating resource %s %s:%s (%s)", CLSS,resName,res.getResourceId().getProjectName(),
						res.getResourceId().getResourcePath().getPath().toString(),ex.getMessage());
				log.warn(msg,ex);
			}
		}
		this.counter.decrementCount();
	}
}
