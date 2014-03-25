/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway;

import java.util.ArrayList;
import java.util.List;

import org.apache.wicket.markup.html.WebMarkupContainer;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.proxy.RegistrationScriptFunctions;
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
	private final String prefix = "BLT";
	private transient GatewayRpcDispatcher dispatcher = null;
	private transient GatewayContext context = null;
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
		DiagramPropertiesHandler.getInstance().setContext(context);
		ProxyHandler.getInstance().setContext(context);
		dispatcher = new GatewayRpcDispatcher(context);
	}

	@Override
	public void startup(LicenseState licenseState) {
	    log.infof("%s: Startup complete.",TAG);
	}

	@Override
	public void shutdown() {
		//context.getProjectManager().removeProjectListener(mrm);
		BlockExecutionController.getInstance().stop();
		
	}

	@Override
	public Object getRPCHandler(ClientReqSession session, Long projectId) {
		log.debugf("%s: getRPCHandler - request for project %s",TAG,projectId.toString());
		return dispatcher;
	}
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTProperties.REGISTRATION_SCRIPT_PACKAGE,RegistrationScriptFunctions.class);
		mgr.addScriptModule(BLTProperties.REPORTING_SCRIPT_PACKAGE,BlockCompletionScriptFunctions.class);
	}
	
	@Override
	public List<? extends INamedTab> getStatusPanels() {
		ExecutionStatus panel = new ExecutionStatus();
		List<INamedTab>panels = new ArrayList<INamedTab>();
		panels.add(new ExecutionStatus());
		return panels;
	}
	
	// So we're serializable ??
	public void setDispatcher(GatewayRpcDispatcher rpc) { this.dispatcher = rpc; }
	public GatewayRpcDispatcher getDispatcher() { return this.dispatcher; }
	
	private class ExecutionStatus extends AbstractNamedTab {
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
