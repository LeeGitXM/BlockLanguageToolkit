/**
 *   (c) 2015-2106  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.ArrayList;
import java.util.List;

import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.script.AbstractScriptExtensionManager;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.gateway.proxy.ProxyHandler;


/**
 *  The manger is a singleton used to compile and execute Python scripts. The
 *  scripts come in 4 flavors (PROPERTY_GET_SCRIPT, NODE_RENAME_SCRIPT,
 *  PROPERTY_SET_SCRIPT and NODE_SAVE_SCRIPT). The standard signatures are:
 *  	get/set(uuid,properties).
 *  	rename(uuid,oldName,newName)
 *  	save(uuid,properties)
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
	 * @return the singleton instance
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
	 * Query the blocks and return a list of descriptors for classes that require
	 * external interface scripts. We re-query each time we're asked. The "embedded label"
	 * is a good display label. In this, the gateway version, we look for only Python
	 * classes to have extensions.
	 * @return
	 */
	public List<BlockDescriptor> getClassDescriptors() {
		List<BlockDescriptor> descriptors = new ArrayList<>();
		// Start with the fixed ones
		BlockDescriptor appDescriptor = new BlockDescriptor();
		appDescriptor.setBlockClass(ScriptConstants.APPLICATION_CLASS_NAME);
		appDescriptor.setEmbeddedLabel("Application");
		descriptors.add(appDescriptor);
		
		BlockDescriptor famDescriptor = new BlockDescriptor();
		famDescriptor.setBlockClass(ScriptConstants.FAMILY_CLASS_NAME);
		famDescriptor.setEmbeddedLabel("Family");
		descriptors.add(famDescriptor);
		
		BlockDescriptor diagDescriptor = new BlockDescriptor();
		diagDescriptor.setBlockClass(ScriptConstants.DIAGRAM_CLASS_NAME);
		diagDescriptor.setEmbeddedLabel("Diagram");
		descriptors.add(diagDescriptor);
		
		ProxyHandler handler = ProxyHandler.getInstance();
		BlockDescriptor blockDescriptor = null;
		List<PalettePrototype> prototypes = handler.getPalettePrototypes();
		for( PalettePrototype proto:prototypes) {
			// log.tracef("%s.getClassDescriptors: block class = %s",TAG,proto.getBlockDescriptor().getBlockClass());
			if( proto.getBlockDescriptor().isExternallyAugmented() ) {
				blockDescriptor = proto.getBlockDescriptor();
				// Guarantee that the embedded label has a usable value.
				String label = blockDescriptor.getEmbeddedLabel();
				if( label==null || label.length()==0 ) blockDescriptor.setEmbeddedLabel(proto.getPaletteLabel());
				descriptors.add(blockDescriptor);
			}
		}
		return descriptors;
	}
}
