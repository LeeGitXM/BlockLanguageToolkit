/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;

import java.util.HashMap;
import java.util.Map;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  The manger is a singleton used to compile and execute Python scripts. There is a fixed
 *  vocabulary of these scripts, all derived from the Script base class.
 *  
 *  We maintain a script table to retain compiled versions of the scripts.
 *  
 */
public class ScriptExtensionManager {
	private static String TAG = "ScriptExtensionManager";
	private final LoggerEx log;
	private final Map<String,Script> scriptInstanceMap;    // Keyed by the script type.
	private static ScriptExtensionManager instance = null;
	
	/**
	 * The handler, make this private per Singleton pattern ...
	 */
	private ScriptExtensionManager() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		scriptInstanceMap = new HashMap<>();
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
	
	
}
