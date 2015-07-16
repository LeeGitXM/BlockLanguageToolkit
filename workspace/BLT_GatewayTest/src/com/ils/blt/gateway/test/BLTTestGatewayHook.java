/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.test;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.gateway.classic.ClassicModelManager;
import com.ils.blt.gateway.classic.ClassicRequestHandler;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
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
	private transient BLTTGatewayRpcDispatcher dispatcher = null;
	private transient GatewayContext context = null;
	private transient ModelManager mmgr = null;
	private final LoggerEx log;
	
	public BLTTestGatewayHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		log.info(TAG+"Initializing BLT Gateway hook (for test)");
	}
		
	// NOTE: During this period, the module status is LOADED, not RUNNING

	@Override
	public void setup(GatewayContext ctxt) {
		this.context = ctxt;
	}

	@Override
	public void startup(LicenseState licenseState) {
		// Look for all block resources and inform the execution controller
	    mmgr = new ClassicModelManager(context);
	    BlockExecutionController.getInstance().setDelegate(mmgr);
	    ClassicRequestHandler handler = new ClassicRequestHandler(context);
	    MockDiagramRequestHandler requestHandler = new MockDiagramRequestHandler(context,handler);
	    dispatcher = new BLTTGatewayRpcDispatcher(context,requestHandler);
		GatewayMockDiagramScriptFunctions.requestHandler = requestHandler;
		log.infof("%s.startup: complete.",TAG);
	}

	@Override
	public void shutdown() {
	}

	@Override
	public Object getRPCHandler(ClientReqSession session, Long projectId) {
		log.debugf("%s.getRPCHandler - request for project %s",TAG,projectId.toString());
		return dispatcher;
	}
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTProperties.TEST_SCRIPT_PACKAGE,GatewayMockDiagramScriptFunctions.class);
	}

}
