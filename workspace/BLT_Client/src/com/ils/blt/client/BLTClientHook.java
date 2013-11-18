/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.client;

import com.ils.diagnostics.common.DTProperties;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.model.ClientContext;
import com.inductiveautomation.ignition.common.expressions.ExpressionFunctionManager;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.xmlserialization.deserialization.XMLDeserializer;
import com.inductiveautomation.vision.api.client.ClientModuleHook;

public class BLTClientHook implements ClientModuleHook {
	/**
	 * Make the tag-creation script functions available.
	 */
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		mgr.addScriptModule(DTProperties.GRAPHICS_SCRIPT_PACKAGE,GraphicsScriptFunctions.class);
	}

	@Override
	public void configureDeserializer(XMLDeserializer arg0) {
		// TODO Auto-generated method stub
	}
	
	@Override
	public void notifyActivationStateChanged(LicenseState arg0) {
		// TODO Auto-generated method stub	
	}

	@Override
	public void shutdown() {
		// TODO Auto-generated method stub
	}

	@Override
	public void startup(ClientContext ctx, LicenseState arg1) throws Exception {
		GatewayConnectionManager.getInstance().addPushNotificationListener(new GatewayClientDelegate());
	}
	
	@Override
	public void configureFunctionFactory(ExpressionFunctionManager factory) {

		
	}

}
