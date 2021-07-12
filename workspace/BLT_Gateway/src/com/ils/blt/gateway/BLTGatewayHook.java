/**
 *   (c) 2014-2021  ILS Automation. All rights reserved. 
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
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.ils.common.persistence.ToolkitRecord;
import com.ils.common.persistence.ToolkitRecordHandler;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.script.ScriptManager;
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
	private ToolkitRecordHandler toolkitHandler;
	private final ILSLogger log;
	private ToolkitRecord record = null;
	private final ControllerRequestHandler requestHandler;
	private ToolkitRecordListener recordListener;
	
	public static BLTGatewayHook get(GatewayContext ctx) { 
		return (BLTGatewayHook)ctx.getModule(BLTProperties.MODULE_ID);
	}
	
	public BLTGatewayHook() {
		log = LogMaker.getLogger(this);
		log.info(CLSS+".Initializing BLT Gateway hook FOO");
		BundleUtil.get().addBundle(prefix, getClass(), BUNDLE_NAME);
		requestHandler = ControllerRequestHandler.getInstance();
	}
		
	@Override
	public boolean isFreeModule() { return true; }
	
	// NOTE: During this period, the module status is LOADED, not RUNNING
	//       This comes before startup().
	@Override
	public void setup(GatewayContext ctxt) {
		context = ctxt;

		// NOTE: Get serialization exception if ModelResourceManager is saved as a class member
		//       Exception is thrown when we try to incorporate a StatusPanel
		log.info(CLSS+".setup - enable project listeners.");
		ProxyHandler.getInstance().setContext(context);
		requestHandler.setContext(context);
		dispatcher = new GatewayRpcDispatcher(context);
		recordListener = new ToolkitRecordListener(context);
		
		// Set context in the ScriptManager instance
		ScriptExtensionManager.getInstance().setContext(context);
		log.info(CLSS+".setup - done setting up scriptExtensionManager.");
		// Register the ToolkitRecord making sure that the table exists
		try {
			context.getSchemaUpdater().updatePersistentRecords(ToolkitRecord.META);
		}
		catch(SQLException sqle) {
			log.error("BLTGatewayHook.setup: Error registering ToolkitRecord",sqle);
		}
		log.info(CLSS+".setup() complete");
		
	}

	@Override
	public void startup(LicenseState licenseState) {
		log.info(CLSS+".startup()");
		this.toolkitHandler = new ToolkitRecordHandler(context);
		this.mmgr = new ModelManager(context);
		BlockExecutionController controller = BlockExecutionController.getInstance();
		controller.setDelegate(mmgr);


		// Analyze existing projects - skip the global project and any that are disabled.
		List<Project> projects = context.getProjectManager().getProjectsFull(ProjectVersion.Staging);
		for( Project project:projects ) {
			if( !project.isEnabled() || project.getId()==-1 ) continue;
				log.infof(CLSS+".startup() - adding project %s", project.getName());
				mmgr.projectAdded(project,null); 
		}

		// Register for changes to our permanent settings
		ToolkitRecord.META.addRecordListener(recordListener);
		controller.start(context);     // Start the controller once the project has been analyzed
		
		context.getProjectManager().addProjectListener(mmgr);  
		log.infof("%s: Startup complete.",CLSS);
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
			return new ToolkitStatusPanel(id);
		}
	}
	
	public ToolkitRecord getPersistentRecord() { return record; }

}
