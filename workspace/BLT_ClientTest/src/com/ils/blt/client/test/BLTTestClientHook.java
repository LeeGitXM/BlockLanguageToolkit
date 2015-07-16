/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.client.test;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.client.model.ClientContext;
import com.inductiveautomation.ignition.common.expressions.ExpressionFunctionManager;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.xmlserialization.deserialization.XMLDeserializer;
import com.inductiveautomation.vision.api.client.ClientModuleHook;

public class BLTTestClientHook implements ClientModuleHook {

	/**
	 * Make the tag-creation script functions available.
	 */
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		mgr.addScriptModule(BLTProperties.TEST_SCRIPT_PACKAGE,MockDiagramScriptFunctions.class);
	}

	@Override
	public void configureDeserializer(XMLDeserializer arg0) {
	}
	
	@Override
	public void notifyActivationStateChanged(LicenseState arg0) {	
	}

	@Override
	public void shutdown() {
	}

	@Override
	public void startup(ClientContext arg0, LicenseState arg1) throws Exception {
	}
	
	@Override
	public void configureFunctionFactory(ExpressionFunctionManager factory) {
	}
}
