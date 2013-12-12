/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import org.apache.wicket.markup.html.WebMarkupContainer;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelResourceManager;
import com.ils.blt.gateway.proxy.CallbackRegistrationScriptFunctions;
import com.ils.blt.gateway.proxy.ProxyHandler;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
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
	private final Hashtable<Long,GatewayRpcDispatcher> dispatchers;
	private ModelResourceManager mrm = null;
	String prefix = "BLT";

	private GatewayContext context = null;
	private final LoggerEx log;
	
	public BLTGatewayHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		log.info(TAG+"Initializing BLT Gateway hook");
		dispatchers = new Hashtable<Long,GatewayRpcDispatcher>();
		BundleUtil.get().addBundle(prefix, getClass(), BUNDLE_NAME);
	}
		
	// NOTE: During this period, the module status is LOADED, not RUNNING

	@Override
	public void setup(GatewayContext ctxt) {
		this.context = ctxt;
		// Initialize the controller.
		BlockExecutionController.getInstance().setContext(ctxt);
		mrm = new ModelResourceManager(context);
		context.getProjectManager().addProjectListener(mrm);

		log.info(TAG+"Setup - enabled project listeners.");
		PropertiesUpdateHandler.getInstance().setContext(context);
		ProxyHandler.getInstance().setContext(context);
	}

	@Override
	public void startup(LicenseState licenseState) {
	    log.info(TAG+"Startup complete.");
	    BlockExecutionController.getInstance().start();
	}

	@Override
	public void shutdown() {

		context.getProjectManager().removeProjectListener(mrm);
		BlockExecutionController.getInstance().stop();
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
		mgr.addScriptModule(BLTProperties.REGISTRATION_SCRIPT_PACKAGE,CallbackRegistrationScriptFunctions.class);
		mgr.addScriptModule(BLTProperties.REPORTING_SCRIPT_PACKAGE,BlockCompletionScriptFunctions.class);
	}
	@Override
	public List<? extends INamedTab> getStatusPanels() {
		ExecutionStatus panel = new ExecutionStatus();
		List<INamedTab>panels = new ArrayList<INamedTab>();
		panels.add(new ExecutionStatus());
		return panels;
	}
	
	private class ExecutionStatus extends AbstractNamedTab {
		
		

		public ExecutionStatus() {
			super("ExecutionStatus", "BLT.title");
		}
		
		@Override
		public WebMarkupContainer getPanel(String id) {
			return new StatusPanel(id);
		}
		
	}

}
