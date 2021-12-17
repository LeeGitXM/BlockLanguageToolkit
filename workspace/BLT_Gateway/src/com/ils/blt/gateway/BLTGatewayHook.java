/**
 *   (c) 2014-2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.wicket.markup.html.WebMarkupContainer;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.persistence.ToolkitRecordListener;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.ils.blt.gateway.wicket.ToolkitStatusPanel;
import com.ils.common.persistence.ToolkitProjectRecord;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.RuntimeProject;
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
	public static String CLSS = "BLTGatewayHook";
	public static String BUNDLE_NAME = "block";// Properties file is block.properties
	private static GatewayContext context = null;
	private final String prefix = "BLT";
	private transient GatewayRpcDispatcher dispatcher = null;
	private transient ModelManager mmgr = null;
	private final LoggerEx log;
	private ToolkitProjectRecord record = null;
	private final ControllerRequestHandler requestHandler;
	private ToolkitRecordListener recordListener = null;
	
	public static BLTGatewayHook get(GatewayContext ctx) { 
		return (BLTGatewayHook)ctx.getModule(BLTProperties.MODULE_ID);
	}
	
	public BLTGatewayHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		log.info(CLSS+".Initializing BLT Gateway hook FOO");
		BundleUtil.get().addBundle(prefix, getClass(), BUNDLE_NAME);
		requestHandler = ControllerRequestHandler.getInstance();
	}
		
	@Override
	public boolean isFreeModule() { return true; }
	
	// NOTE: At the end of this period, the module status is "Loading"
	//       This comes before startup().
	@Override
	public void setup(GatewayContext ctxt) {
		context = ctxt;

		// NOTE: Get serialization exception if ModelResourceManager is saved as a class member
		//       Exception is thrown when we try to incorporate a StatusPanel
		ProxyHandler.getInstance().setContext(context);
		requestHandler.setContext(context);
		dispatcher = new GatewayRpcDispatcher(context);
		recordListener = new ToolkitRecordListener(context);
		
		// Set context in the ScriptManager instance
		ScriptExtensionManager.getInstance().setContext(context);
		log.info(CLSS+".setup - done setting up scriptExtensionManager.");
		// Register the ToolkitRecord making sure that the table exists
		try {
			context.getSchemaUpdater().updatePersistentRecords(ToolkitProjectRecord.META);
		}
		catch(SQLException sqle) {
			log.error("BLTGatewayHook.setup: Error registering ToolkitProjectRecord",sqle);
		}
		log.info(CLSS+".setup() complete");
	}
	// NOTE: At the end of this period, the module status is "Running"
	@Override
	public void startup(LicenseState licenseState) {
		log.info(CLSS+".startup()");
		this.mmgr = new ModelManager(context);
		BlockExecutionController controller = BlockExecutionController.getInstance();
		controller.setDelegate(mmgr);
		
		// Analyze existing projects - skip the global project and any that are disabled.
		List<String> projectNames = context.getProjectManager().getProjectNames();
		for( String name:projectNames ) {
			Optional<RuntimeProject> optional = context.getProjectManager().getProject(name);
			RuntimeProject project = optional.get();
			if( !project.isEnabled() || project.getName().equals(Project.GLOBAL_PROJECT_NAME) ) continue;
				log.infof(CLSS+".startup() - adding project %s", project.getName());
				mmgr.projectAdded(project.getName()); 
		}

		// Register for changes to our permanent settings
		ToolkitProjectRecord.META.addRecordListener(recordListener);
		controller.start(context);     // Start the controller once the project has been analyzed
		
		context.getProjectManager().addProjectListener(mmgr);  
		log.infof("%s: Startup complete.",CLSS);
	}

	@Override
	public void shutdown() {
		ToolkitProjectRecord.META.removeRecordListener(recordListener);
		context.getProjectManager().removeProjectListener(mmgr);
		BlockExecutionController.getInstance().stop(context);
	}

	@Override
	public Object getRPCHandler(ClientReqSession session, String projectName) {
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
			return new ToolkitStatusPanel(id);
		}
	}
	
	public ToolkitProjectRecord getPersistentRecord() { return record; }

}
