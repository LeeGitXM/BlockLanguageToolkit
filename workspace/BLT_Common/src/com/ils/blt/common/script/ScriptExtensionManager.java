/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.python.core.PyDictionary;
import org.python.core.PyList;
import org.python.core.PyObject;

import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  The manger is a singleton used to compile and execute Python scripts. The
 *  scripts come in three flavors (PROPERTY_GET_SCRIPT, PROPERTY_RENAME_SCRIPT,
 *  and PROPERTY_SET_SCRIPT). The get/set have the same signature (uuid,properties).
 *  The rename is (uuid,oldName,newName).  This group of scripts must be defined 
 *  for every class that wants interact with external data.
 *  
 *  We maintain a script table to retain compiled versions of the scripts. Script 
 *  local variables are updated on each invocation. This feature applies only to 
 *  the "classic" version of the toolkit.
 */
public class ScriptExtensionManager {
	private static String TAG = "ScriptExtensionManager";
	private ToolkitRequestHandler handler = null;
	private final LoggerEx log;
	private static ScriptExtensionManager instance = null;
	private final JavaToPython j2p;
	private final PythonToJava p2j;
	private List<String> flavors;
	
	public final Map<String,Map<String,Object>> scriptMap;
	
	/**
	 * The handler, make this private per Singleton pattern ...
	 * The initialization here is logically to a static initializer.
	 */
	private ScriptExtensionManager() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		// Initialize map with entry points and call list
		scriptMap = new HashMap<>();
		j2p = new JavaToPython();
		p2j = new PythonToJava();
		flavors = new ArrayList<>();
		flavors.add(ScriptConstants.PROPERTY_GET_SCRIPT);
		flavors.add(ScriptConstants.PROPERTY_RENAME_SCRIPT);
		flavors.add(ScriptConstants.PROPERTY_SET_SCRIPT);
	}
	
	/**
	 * This class is used in a variety of scopes. Initialize the request handler in each of them.
	 * @param h
	 */
	public void setToolkitRequestHandler(ToolkitRequestHandler h) { this.handler = h; }
	
	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static ScriptExtensionManager getInstance() {
		if( instance==null) {
			synchronized(ScriptExtensionManager.class) {
				instance = new ScriptExtensionManager();
			}
		}
		return instance;
	}
	
	/**
	 * Adding a script has an advantage over an ad-hoc execution in that
	 * the compilation is cached.
	 * @param className
	 * @param flavor
	 * @param modulePath
	 */
	public void addScript(String className,String flavor,String modulePath) {
		String key = makeKey(className,flavor);
		String entry = "execute";
		String arglist = "";
		if( flavor.equals(ScriptConstants.PROPERTY_GET_SCRIPT))  {
			entry = "getAux";
			arglist = "uuid,properties";
		}
		else if( flavor.equals(ScriptConstants.PROPERTY_SET_SCRIPT))  {
			entry = "setAux";
			arglist = "uuid,properties";
		}
		else if( flavor.equals(ScriptConstants.PROPERTY_RENAME_SCRIPT))  {
			entry = "rename";
			arglist = "uuid,oldname,newname";
		}
		scriptMap.put(key,createMap(entry,arglist));
		setModulePath(key,modulePath);
		log.debugf("%s.addScript: %s-%s is %s",TAG,className,flavor,modulePath);
	}
	
	public List<String> getFlavors() {
		return flavors;
	}
	
	/**
	 * Query the blocks and return a list of classes that require
	 * external interface scripts. We re-query each time we're asked.
	 * @return
	 */
	public List<String> getClassNames() {
		List<String> classNames = new ArrayList<>();
		// These are givens
		List<BlockDescriptor> descriptors = getClassDescriptors();
		for( BlockDescriptor bd:descriptors ) {
			classNames.add(bd.getBlockClass());
		}
		return classNames;
	}
	
	/**
	 * Query the blocks and return a list of descriptors for classes that require
	 * external interface scripts. We re-query each time we're asked. The "embedded label"
	 * is a good display label.
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
	
	/**
	 * Execute the specified script with a new set of arguments.
	 * 
	 * @param mgr a script manager with proper context
	 * @param key to a pre-configured script. The configuration includes
	 *            module path, entry point and argument prototype.
	 * @param args
	 */
	@SuppressWarnings("unchecked")
	public void runScript(ScriptManager mgr,String className,String flavor,Object...args) {
		String key = makeKey(className,flavor);
		Map<String,Object> map = scriptMap.get(key);
		if( map!=null ) {
			Script script = (Script)map.get(ScriptConstants.SCRIPT_KEY);
			if( script.compileScript()) {
				try {
					script.initializeLocalsMap(mgr);
					List<PyObject> pyargs = new ArrayList<>();
					int index = 0;
					for(Object arg:args) {
						PyObject pyarg = j2p.objectToPy(arg);
						script.setLocalVariable(index, pyarg);
						pyargs.add(pyarg);
						index++;
					}
					script.execute(mgr);
					// For "complex" arguments, we update contents
					// as a mechanism to return info from the script
					index = 0;
					for(Object arg:args) {
						if( arg instanceof Map) {
							PyObject pyarg = pyargs.get(index);
							p2j.updateMapFromDictionary((Map<String,Object>)arg,(PyDictionary)pyarg);
							log.debugf("%s.runScript: Updating map on return: %s",TAG,pyarg.toString());
						}
						else if( arg instanceof GeneralPurposeDataContainer) {
							PyObject pyarg = pyargs.get(index);
							p2j.updateDataContainerFromPython((GeneralPurposeDataContainer)arg,(PyList)pyarg);
							log.debugf("%s.runScript: Updating container on return: %s",TAG,pyarg.toString());
						}
						index++;
					}
				}
				catch(Exception ex) {
					log.warnf("%s.runScript: Exception (%s)",TAG,ex.getMessage(),ex);
				}
			}
		}
		else {
			log.warnf("%s.runScript: Unknown python script type (%s)",TAG,key);
		}
	}
	
	/**
	 * This method is for use with scripts that are not globally specified,
	 * for example, those that are unique to a block or block class. The
	 * key specifies the arg list, but, in this case, not the module path.
	 * 
	 * @param mgr a script manager with proper context
	 * @param pythonPath the Python path to the module package, not including the entry point.
	 * @param key to a pre-configured script type (but not an individual script. The 
	 *            configuration includes entry point and argument prototype.
	 * @param args actual arguments to the script
	 */
	public void runOneTimeScript(ScriptManager mgr,String pythonPath,String className,String flavor,Object...args) {
		String key = makeKey(className,flavor);
		setModulePath(key,pythonPath);
		runScript(mgr,className,flavor,args);
	}
	
	public Set<String> scriptTypes() {return scriptMap.keySet();}
	
	/**
	 * Define the code that corresponds to the well-known key.
	 * Ignore keys that are not in ScriptConstants.
	 * @param key as found in ScriptConstants.
	 * @param pythonPath
	 */
	public void setModulePath(String key,String pythonPath) {
		Map<String,Object> map = scriptMap.get(key);
		if( map!=null ) {
			Script script = (Script)map.get(ScriptConstants.SCRIPT_KEY);
			script.resetModulePath(pythonPath);
		}
		else {
			log.tracef("%s.setModulePath: Unknown python script type (%s)",TAG,key);
		}
	}
	
	private Map<String,Object> createMap(String entry,String arglist) {
		Map<String,Object> result = new HashMap<>();
		result.put(ScriptConstants.ENTRY_KEY, entry);
		result.put(ScriptConstants.ARGS_KEY, arglist);
		result.put(ScriptConstants.SCRIPT_KEY, new Script(entry,arglist));
		return result;
	}
	
	public static String makeKey(String className,String flavor) {
		return String.format("%s(%s)",className,flavor);
	}
}
