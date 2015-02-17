/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.python.core.PyObject;

import com.ils.common.JavaToPython;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  The manger is a singleton used to compile and execute Python scripts. There is a fixed
 *  vocabulary of these scripts, all derived from the Script base class. 
 *  
 *  We maintain a script table to retain compiled versions of the scripts. There is
 *  one script of each type. Script local variables are updated on each invocation.
 */
public class ScriptExtensionManager {
	private static String TAG = "ScriptExtensionManager";
	private final LoggerEx log;
	private static ScriptExtensionManager instance = null;
	private final JavaToPython j2p;
	
	public final Map<String,Map<String,Object>> scriptMap;
	
	/**
	 * The handler, make this private per Singleton pattern ...
	 * The initialization here is logically to a static initializer.
	 */
	private ScriptExtensionManager() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		// Initialize map with entry points and call list
		scriptMap = new HashMap<>();
		scriptMap.put(ScriptConstants.APP_ADD_SCRIPT,createMap("add","uuid"));
		scriptMap.put(ScriptConstants.APP_CLONE_SCRIPT,createMap("clone","uuid1,uuid2"));
		scriptMap.put(ScriptConstants.APP_DELETE_SCRIPT,createMap("delete","uuid"));
		scriptMap.put(ScriptConstants.APP_GET_AUX_SCRIPT,createMap("getAux","uuid,properties"));
		scriptMap.put(ScriptConstants.APP_SET_AUX_SCRIPT,createMap("setAux","uuid,properties"));
		scriptMap.put(ScriptConstants.APP_UPDATE_SCRIPT,createMap("update","name,uuid"));
		scriptMap.put(ScriptConstants.FAM_ADD_SCRIPT,createMap("add","uuid"));
		scriptMap.put(ScriptConstants.FAM_CLONE_SCRIPT,createMap("clone","uudid1,uuid2"));
		scriptMap.put(ScriptConstants.FAM_DELETE_SCRIPT,createMap("delete","uuid"));
		scriptMap.put(ScriptConstants.FAM_GET_AUX_SCRIPT,createMap("getAux","uuid,properties"));
		scriptMap.put(ScriptConstants.FAM_SET_AUX_SCRIPT,createMap("setAux","uuid,properties"));
		scriptMap.put(ScriptConstants.FAM_UPDATE_SCRIPT,createMap("update","name,uuid"));
		
		j2p = new JavaToPython();
	}
	
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
	 * Execute the specified script with a new set of arguments.
	 * 
	 * @param type
	 * @param args
	 */
	public void runScript(ScriptManager mgr,String key,Object...args) {
		Map<String,Object> map = scriptMap.get(key);
		if( map!=null ) {
			Script script = (Script)map.get(ScriptConstants.SCRIPT_KEY);
			if( script.compileScript()) {
				try {
					script.initializeLocalsMap(mgr);
					int index = 0;
					for(Object arg:args) {
						PyObject pyObject = j2p.objectToPy(arg);
						script.setLocalVariable(index, pyObject);
						index++;
					}
					script.execute(mgr);
				}
				catch(Exception ex) {
					log.warnf("%s.runScript: Exception (%s)",TAG,ex.getMessage(),ex);
				}
			}
		}
		else {
			log.warnf("%s.runScript: Unknown pythpn script type (%s)",TAG,key);
		}
	}
	
	public Set<String> scriptTypes() {return scriptMap.keySet();}
	
	/**
	 * Define the code that corresponds to the well-known key.
	 * If an empty path is specified, then the module will
	 * be deactivated.
	 * 
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
			log.warnf("%s.setModulePath: Unknown pythpn script type (%s)",TAG,key);
		}
	}
	
	private Map<String,Object> createMap(String entry,String arglist) {
		Map<String,Object> result = new HashMap<>();
		result.put(ScriptConstants.ENTRY_KEY, entry);
		result.put(ScriptConstants.ARGS_KEY, arglist);
		result.put(ScriptConstants.SCRIPT_KEY, new Script(entry,arglist));
		return result;
	}
}
