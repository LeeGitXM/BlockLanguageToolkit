/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *   This is the .
 */
package com.ils.blt.test.designer;


import com.ils.blt.test.client.MockDiagramScriptFunctions;
import com.ils.blt.test.client.TagProviderScriptFunctions;
import com.ils.blt.test.common.BLTTestProperties;
import com.inductiveautomation.ignition.common.expressions.ExpressionFunctionManager;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.AbstractDesignerModuleHook;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 *  This is the class that is instantiated on startup of the designer. Its purpose
 *  is to populate the project tree with a node for workspace creation.
 */
public class BLTTestDesignerHook extends AbstractDesignerModuleHook {
	private final LoggerEx logger;
	
	public BLTTestDesignerHook() {
		logger = LogUtil.getLogger(getClass().getPackage().getName());
		logger.info("Initializing designer hook");
	}
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTTestProperties.MOCK_SCRIPT_PACKAGE,MockDiagramScriptFunctions.class);
		mgr.addScriptModule(BLTTestProperties.TAG_SCRIPT_PACKAGE,TagProviderScriptFunctions.class);
	}
	
	@Override
	public void startup(DesignerContext context, LicenseState activationState) throws Exception {
		logger.info("Startup...");
		super.startup(context, activationState);
	}
	
	@Override
	public void configureFunctionFactory(ExpressionFunctionManager factory) {
		super.configureFunctionFactory(factory);
	}
}
