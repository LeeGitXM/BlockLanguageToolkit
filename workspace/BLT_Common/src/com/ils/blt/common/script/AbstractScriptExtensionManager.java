/**
 *   (c) 2015-2016  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.python.core.PyDictionary;
import org.python.core.PyList;
import org.python.core.PyObject;

import com.ils.blt.common.block.BlockDescriptor;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  The manger is an abstract base class used to compile and execute Python scripts.
 *  The scripts come in 4 flavors (PROPERTY_GET_SCRIPT, PROPERTY_RENAME_SCRIPT,
 *  PROPERTY_SET_SCRIPT and NODE_CREATE_SCRIPT). The standard signatures are:
 *  	get/set(uuid,properties).
 *  	rename(uuid,oldName,newName)
 *  	create(uuid)
 *  This group of scripts must be defined 
 *  for every class that wants interact with external data.
 *  
 *  We maintain a script table to retain compiled versions of the scripts. Script 
 *  local variables are updated on each invocation.
 *  
 *  Concrete versions exist in both gateway and client/designer scopes.
 */
public abstract class AbstractScriptExtensionManager {
	private static String TAG = "AbstractScriptExtensionManager";
	protected final LoggerEx log;
	protected final JavaToPython j2p;
	protected final PythonToJava p2j;
	protected final List<String> flavors;
	protected final Map<String,Map<String,Object>> scriptMap;
	
	/**
	 * The handler.
	 */
	protected AbstractScriptExtensionManager() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		// Initialize map with entry points and call list
		scriptMap = new HashMap<>();
		j2p = new JavaToPython();
		p2j = new PythonToJava();
		flavors = new ArrayList<>();
		flavors.add(ScriptConstants.NODE_CREATE_SCRIPT);
		flavors.add(ScriptConstants.PROPERTY_GET_SCRIPT);
		flavors.add(ScriptConstants.PROPERTY_RENAME_SCRIPT);
		flavors.add(ScriptConstants.PROPERTY_SET_SCRIPT);
		
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
		else if( flavor.equals(ScriptConstants.NODE_CREATE_SCRIPT))  {
			entry = "create";
			arglist = "uuid";
		}
		scriptMap.put(key,createMap(entry,arglist));
		setModulePath(key,modulePath);
		log.infof("%s.addScript: %s is %s",TAG,key,modulePath);
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
	 * This method is abstract for the sole reason that interfaces 
	 * to get the class list differ in Client and Gateway scopes.
	 */
	public abstract  List<BlockDescriptor> getClassDescriptors();
	
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
