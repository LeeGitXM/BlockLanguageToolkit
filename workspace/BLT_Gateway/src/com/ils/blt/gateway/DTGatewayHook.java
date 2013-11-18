/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway;

import java.util.Hashtable;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelResourceManager;
import com.ils.diagnostics.common.DTProperties;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.clientcomm.ClientReqSession;
import com.inductiveautomation.ignition.gateway.model.AbstractGatewayModuleHook;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.project.ProjectListener;


/**
 * This is root node for specialty code dealing with the gateway. On startup
 * we obtain the gateway context. It serves as our entry point into the
 * Ignition core.
 * 
 * At present this code does nothing.
 */
public class DTGatewayHook extends AbstractGatewayModuleHook implements ProjectListener {
	public static String TAG = "DTGatewayHook";
	public static String BUNDLE_NAME = "diagnostics";// Properties file is sct.properties
	private final Hashtable<Long,GatewayRpcDispatcher> dispatchers;
	private ModelResourceManager mrm = null;

	private GatewayContext context = null;
	private final LoggerEx log;
	
	public DTGatewayHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		log.info(TAG+"Initializing DT Gateway hook");
		dispatchers = new Hashtable<Long,GatewayRpcDispatcher>();
	}
		
	// NOTE: During this period, the module status is LOADED, not RUNNING

	@Override
	public void setup(GatewayContext ctxt) {
		this.context = ctxt;
		// Initialize the controller.
		BlockExecutionController.getInstance().setContext(ctxt);
		mrm = new ModelResourceManager(context);
		context.getProjectManager().addProjectListener(mrm);
		context.getProjectManager().addProjectListener(this);
		log.info(TAG+"Setup - enabled project listeners.");
		PropertiesUpdateHandler.getInstance().setContext(context);
	}

	// See PassThroughGatewayHook example
	@Override
	public void startup(LicenseState licenseState) {
	    log.info(TAG+"Startup complete.");
	}

	@Override
	public void shutdown() {
		context.getProjectManager().removeProjectListener(this);
		context.getProjectManager().removeProjectListener(mrm);
	}

	@Override
	public Object getRPCHandler(ClientReqSession session, Long projectId) {
		log.info(TAG+"getRPCHandler - request for project "+projectId);
		GatewayRpcDispatcher dispatcher = dispatchers.get(projectId);
		if( dispatcher==null ) {
			dispatcher = new GatewayRpcDispatcher(context,projectId);
			dispatchers.put(projectId, dispatcher);
		}
		return dispatcher;
	}
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(DTProperties.REPORTING_SCRIPT_PACKAGE,StatusReportingScriptFunctions.class);
		PropertiesUpdateHandler.getInstance().setScriptManager(mgr);
	}
	
	// ====================== Project Listener Interface ===================
	/**
	 * Enable scripting for any new project that might be added.
	 * @param staging
	 * @param published
	 */
	@Override
	public void projectAdded(Project staging, Project published) {
		ScriptManager mgr = context.getProjectManager().getProjectScriptManager(published.getId());
		if(mgr!=null) {
			mgr.addScriptModule(DTProperties.REPORTING_SCRIPT_PACKAGE, StatusReportingScriptFunctions.class);
			log.info(TAG+"projectAdded ... script manager ");
		}
		else {
			log.warn(TAG+"projectAdded ... unable to register script manager ");
		}
	}

	@Override
	public void projectDeleted(long projectId) {
		dispatchers.remove(new Long(projectId));
	}

	@Override
	public void projectUpdated(Project staging, ProjectVersion published) {
		log.info(TAG+"projectUpdated ...");
	}	
}
