/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway;

import java.util.ArrayList;
import java.util.List;

import org.apache.wicket.markup.html.WebMarkupContainer;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.clientcomm.ClientReqSession;
import com.inductiveautomation.ignition.gateway.model.AbstractGatewayModuleHook;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.web.components.AbstractNamedTab;
import com.inductiveautomation.ignition.gateway.web.models.INamedTab;


/**
 * This is root node for specialty code dealing with the gateway. On startup
 * we obtain the gateway context. It serves as our entry point into the
 * Ignition core.
 * 
 * At present this code does nothing.
 */
public class BLTGatewayHook extends AbstractGatewayModuleHook  {
	public static String TAG = "BLTGatewayHook";
	public static String BUNDLE_NAME = "block";// Properties file is block.properties
	private final String prefix = "BLT";
	private transient GatewayRpcDispatcher dispatcher = null;
	private transient GatewayContext context = null;
	private transient ModelManager mmgr = null;
	private final LoggerEx log;
	
	public BLTGatewayHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		log.info(TAG+"Initializing BLT Gateway hook");
		BundleUtil.get().addBundle(prefix, getClass(), BUNDLE_NAME);
	}
		
	// NOTE: During this period, the module status is LOADED, not RUNNING

	@Override
	public void setup(GatewayContext ctxt) {
		this.context = ctxt;

		// NOTE: Get serialization exception if ModelResourceManager is saved as a class member
		//       Exception is thrown when we try to incorporate a StatusPanel
		log.info(TAG+"Setup - enabled project listeners.");
		ProxyHandler.getInstance().setContext(context);
		ControllerRequestHandler.getInstance().setContext(context);
		dispatcher = new GatewayRpcDispatcher();
	}

	@Override
	public void startup(LicenseState licenseState) {
	    
	    // Look for all block resources and inform the execution controller
		BlockExecutionController controller = BlockExecutionController.getInstance();
	    mmgr = new ModelManager(context);
	    controller.setDelegate(mmgr);
	    List<Project> projects = this.context.getProjectManager().getProjectsFull(ProjectVersion.Staging);
	    for( Project project:projects ) {
	    	List<ProjectResource> resources = project.getResources();
	    	for( ProjectResource res:resources ) {
	    		log.infof("%s.startup - found %s resource, %d = %s", TAG,res.getResourceType(),
	    				res.getResourceId(),res.getName());
	    		mmgr.analyzeResource(project.getId(),res);
	    	}
	    }
	    controller.start(context);
	    context.getProjectManager().addProjectListener(mmgr);
	    // Look for all "Controller Output" UDT instances
	    
	    log.infof("%s: Startup complete.",TAG);
	}

	@Override
	public void shutdown() {
		context.getProjectManager().removeProjectListener(mmgr);
		BlockExecutionController.getInstance().stop();
	}

	@Override
	public Object getRPCHandler(ClientReqSession session, Long projectId) {
		return dispatcher;
	}
	
	@Override
	public List<? extends INamedTab> getStatusPanels() {
		List<INamedTab>panels = new ArrayList<INamedTab>();
		panels.add(new ExecutionStatus());
		return panels;
	}
	
	private static class ExecutionStatus extends AbstractNamedTab {
		private static final long serialVersionUID = 64149723779427382L;

		public ExecutionStatus() {
			super("ExecutionStatus", "BLT.title");
		}
		
		@Override
		public WebMarkupContainer getPanel(String id) {
			return new StatusPanel(id);
		}
		
	}

}
