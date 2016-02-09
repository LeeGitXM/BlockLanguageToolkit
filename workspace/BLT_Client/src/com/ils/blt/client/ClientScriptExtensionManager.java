/**
 *   (c) 2016  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.python.core.PyDictionary;
import org.python.core.PyList;
import org.python.core.PyObject;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.script.AbstractScriptExtensionManager;
import com.ils.blt.common.script.Script;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;


/**
 *  The manger is a singleton used to compile and execute Python scripts. The
 *  scripts come in 4 flavors (PROPERTY_GET_SCRIPT, PROPERTY_RENAME_SCRIPT,
 *  PROPERTY_SET_SCRIPT and NODE_CREATE_SCRIPT). The standard signatures are:
 *  	get/set(uuid,properties).
 *  	rename(uuid,oldName,newName)
 *  	create(uuid)
 *  This group of scripts must be defined 
 *  for every class that wants interact with external data.
 *  
 *  We maintain a script table to retain compiled versions of the scripts. Script 
 *  local variables are updated on each invocation.
 */
public class ClientScriptExtensionManager extends AbstractScriptExtensionManager  {
	private static String TAG = "ClientScriptExtensionManager";
	private final ApplicationRequestHandler handler;
	private static ClientScriptExtensionManager instance = null;

	/**
	 * The handler, make this private per Singleton pattern ...
	 * The initialization here is logically to a static initializer.
	 */
	private ClientScriptExtensionManager() {
		// Initialize map with entry points and call list
		handler = new ApplicationRequestHandler();
	}
	
	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static ClientScriptExtensionManager getInstance() {
		if( instance==null) {
			synchronized(ClientScriptExtensionManager.class) {
				instance = new ClientScriptExtensionManager();
			}
		}
		return instance;
	}
	

	
	/**
	 * Query the blocks and return a list of descriptors for classes that require
	 * external interface scripts. We re-query each time we're asked. The "embedded label"
	 * is a good display label. In this, the client version, it is important to 
	 * obtain the entire class list, including Python. The editor needs this
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
		
		BlockDescriptor blockDescriptor = null;
		List<PalettePrototype> prototypes = handler.getBlockPrototypes();
		for( PalettePrototype proto:prototypes) {
			// log.tracef("%s.createScriptPanel: block class = %s",TAG,proto.getBlockDescriptor().getBlockClass());
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
