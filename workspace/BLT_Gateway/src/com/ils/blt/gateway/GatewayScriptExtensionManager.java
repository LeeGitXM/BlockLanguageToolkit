/**
 *   (c) 2015-2106  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.ArrayList;
import java.util.List;

import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.script.AbstractScriptExtensionManager;
import com.ils.blt.common.script.ScriptConstants;


/**
 *  The manger is a singleton used to compile and execute Python scripts. The
 *  scripts come in 4 flavors (PROPERTY_GET_SCRIPT, PROPERTY_RENAME_SCRIPT,
 *  PROPERTY_SET_SCRIPT and NODE_SAVE_SCRIPT). The standard signatures are:
 *  	get/set(uuid,properties).
 *  	rename(uuid,oldName,newName)
 *  	save(uuid)
 *  This group of scripts must be defined 
 *  for every class that wants interact with external data.
 *  
 *  We maintain a script table to retain compiled versions of the scripts. Script 
 *  local variables are updated on each invocation.
 */
public class GatewayScriptExtensionManager extends AbstractScriptExtensionManager {
	private static GatewayScriptExtensionManager instance = null;
	
	/**
	 * The handler, make this private per Singleton pattern ...
	 * The initialization here is logically to a static initializer.
	 */
	private GatewayScriptExtensionManager() {	
	}
	
	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static GatewayScriptExtensionManager getInstance() {
		if( instance==null) {
			synchronized(GatewayScriptExtensionManager.class) {
				instance = new GatewayScriptExtensionManager();
			}
		}
		return instance;
	}
	
	/**
	 * In the gateway, the only class of interest is a diagram (for the time being).
	 * @return
	 */
	public List<BlockDescriptor> getClassDescriptors() {
		List<BlockDescriptor> descriptors = new ArrayList<>();
		
		BlockDescriptor diagDescriptor = new BlockDescriptor();
		diagDescriptor.setBlockClass(ScriptConstants.DIAGRAM_CLASS_NAME);
		diagDescriptor.setEmbeddedLabel("Diagram");
		descriptors.add(diagDescriptor);
		return descriptors;
	}
}
