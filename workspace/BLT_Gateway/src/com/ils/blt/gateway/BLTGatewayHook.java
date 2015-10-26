/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.apache.wicket.markup.html.WebMarkupContainer;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.persistence.ToolkitRecordListener;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.blt.gateway.wicket.ToolkitStatusPanel;
import com.ils.common.persistence.ToolkitRecord;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.script.ScriptManager;
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
	private ToolkitRecord record = null;
	private ToolkitRecordListener recordListener;
	
	
	public static BLTGatewayHook get(GatewayContext ctx) { 
		return (BLTGatewayHook)ctx.getModule(BLTProperties.MODULE_ID);
	}
	
	public BLTGatewayHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		log.info(TAG+"Initializing BLT Gateway hook");
		BundleUtil.get().addBundle(prefix, getClass(), BUNDLE_NAME);
	}
		
	
	// NOTE: During this period, the module status is LOADED, not RUNNING
	//       This comes before startup().
	@Override
	public void setup(GatewayContext ctxt) {
		this.context = ctxt;

		// NOTE: Get serialization exception if ModelResourceManager is saved as a class member
		//       Exception is thrown when we try to incorporate a StatusPanel
		log.info(TAG+".setup - enable project listeners.");
		ProxyHandler.getInstance().setContext(context);
		ControllerRequestHandler.getInstance().setContext(context);
		dispatcher = new GatewayRpcDispatcher(context);
		recordListener = new ToolkitRecordListener(context);
		
		// Register the ToolkitRecord making sure that the table exists
		try {
			context.getSchemaUpdater().updatePersistentRecords(ToolkitRecord.META);
		}
		catch(SQLException sqle) {
			log.error("BLTGatewayHook.setup: Error registering ToolkitRecord",sqle);
		}
	}

	@Override
	public void startup(LicenseState licenseState) {
	    
	    // Look for all block resources and inform the execution controller
		BlockExecutionController controller = BlockExecutionController.getInstance();
	    mmgr = new ModelManager(context);
	    controller.setDelegate(mmgr);
	    controller.start(context);
	    // Initialize all the script modules from parameters stored in the ORM
	    ScriptExtensionManager sem = ScriptExtensionManager.getInstance();
	    for(String key: sem.scriptTypes()) {
	    	String pythonPath = dispatcher.getToolkitProperty(key);
	    	sem.setModulePath(key, pythonPath);
	    }
	    // Load existing projects - skip the global project and any that are disabled.
	    List<Project> projects = this.context.getProjectManager().getProjectsFull(ProjectVersion.Published);
	    for( Project project:projects ) {
	    	if( !project.isEnabled() || project.getId()==-1 ) continue;
	    	List<ProjectResource> resources = project.getResources();
	    	for( ProjectResource res:resources ) {
	    		// Model manager ignores resources that are not of interest to it.
	    		log.infof("%s.startup - found %s resource, %d = %s", TAG,res.getResourceType(),
	    				res.getResourceId(),res.getName());
	    		mmgr.analyzeResource(project.getId(),res);
	    	}
	    }

	    // Register for changes to our permanent settings
	    ToolkitRecord.META.addRecordListener(recordListener);
	    context.getProjectManager().addProjectListener(mmgr);
	    log.infof("%s: Startup complete.",TAG);
	}

	@Override
	public void shutdown() {
		ToolkitRecord.META.removeRecordListener(recordListener);
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
		panels.add(new ToolkitStatus());
		return panels;
	}
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTProperties.DIAGRAM_SCRIPT_PACKAGE, GatewayScriptFunctions.class);
	}
	
	private static class ToolkitStatus extends AbstractNamedTab {
		private static final long serialVersionUID = 64149723779427382L;

		public ToolkitStatus() {
			super("ToolkitStatus", "BLT.title");
		}
		
		@Override
		public WebMarkupContainer getPanel(String id) {
			//return new SimpleStatusPanel(id);
			return new ToolkitStatusPanel(id);
		}
	}
	
	public ToolkitRecord getPersistentRecord() { return record; }

}
