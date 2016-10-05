package com.ils.blt.designer;

import com.inductiveautomation.ignition.client.gateway_interface.GatewayException;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.IgnitionDesigner;
import com.inductiveautomation.ignition.designer.gateway.DTGatewayInterface;
import com.inductiveautomation.ignition.designer.model.DesignerContext;


/**
 * Create a project resource and make it permanent.
 * Use ExecutionManager.executeOnce() to invoke this in the background.
 * Do not re-execute the same instance.
 * 
 * @author chuckc
 *
 */
public class ResourceCreateManager implements Runnable {
	private static final String TAG = "ResourceCreateManager";
	private static final LoggerEx logger = LogUtil.getLogger(ResourceCreateManager.class.getPackage().getName());
	private static DesignerContext context = null;
	private final ProjectResource res;
	private final ThreadCounter counter = ThreadCounter.getInstance();

	public ResourceCreateManager(ProjectResource pr) {
		this.res = pr;
		this.counter.incrementCount();
	}
	
	/**
	 * Call this method from the hook as soon as the context is established.
	 * @param ctx designer context
	 */
	public static void setContext(DesignerContext ctx) {
		context = ctx;
	}
	
	@Override
	public void run() {
		if( res!=null ) {
			// Now save the resource, as it is.
			Project diff = context.getProject().getEmptyCopy();
			context.updateResource(res);   // Force an update

			diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
			try {
				DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ...");  // Don't publish
			}
			catch(GatewayException ge) {
				logger.warnf("%s.run: Exception saving project resource %d (%s)",TAG,res.getResourceId(),ge.getMessage());
			}
			Project project = context.getProject();
			project.applyDiff(diff,false);
		}
		this.counter.decrementCount();
	}
}
