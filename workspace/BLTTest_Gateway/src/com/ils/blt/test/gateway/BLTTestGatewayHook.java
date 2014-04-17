/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.test.gateway;

import com.ils.blt.test.common.BLTTestProperties;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.clientcomm.ClientReqSession;
import com.inductiveautomation.ignition.gateway.model.AbstractGatewayModuleHook;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 * This is root node for specialty code dealing with the gateway. On startup
 * we obtain the gateway context. It serves as our entry point into the
 * Ignition core.
 * 
 * At present this code does nothing.
 */
public class BLTTestGatewayHook extends AbstractGatewayModuleHook  {
	public static String TAG = "BLTGatewayHook";
	private transient GatewayRpcDispatcher dispatcher = null;
	private transient GatewayContext context = null;
	private final LoggerEx log;
	
	public BLTTestGatewayHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		log.info(TAG+"Initializing BLT Gateway hook (for test)");
	}
		
	// NOTE: During this period, the module status is LOADED, not RUNNING

	@Override
	public void setup(GatewayContext ctxt) {
		this.context = ctxt;

		MockBlockPropertiesHandler.getInstance().setContext(context);
		dispatcher = new GatewayRpcDispatcher(context);
	}

	@Override
	public void startup(LicenseState licenseState) {
	    log.infof("%s: Startup complete.",TAG);
	}

	@Override
	public void shutdown() {
	}

	@Override
	public Object getRPCHandler(ClientReqSession session, Long projectId) {
		log.debugf("%s: getRPCHandler - request for project %s",TAG,projectId.toString());
		return dispatcher;
	}
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTTestProperties.MOCK_SCRIPT_PACKAGE,MockDiagramScriptFunctions.class);
	}

}
