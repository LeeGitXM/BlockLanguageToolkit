/**
 *   (c) 2015-2022  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.script;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.python.core.PyDictionary;
import org.python.core.PyList;
import org.python.core.PyObject;
import org.python.core.PyString;

import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  The manager is used to compile and execute Python scripts.
 *  Scripts are compiled with every execution. Those that are empty or null are logged and ignored.
 *  
 *  Manager instances may be used in any scope.
 */
public class ScriptNotificationManager {
	private static String CLSS = "ScriptNotificationManager";
	protected final LoggerEx log;
	private static final boolean DEBUG = true;
	protected final JavaToPython j2p;
	protected final PythonToJava p2j;
	private static ScriptNotificationManager instance = null;
	
	
	/**
	 * The handler, make this private per Singleton pattern. The initialization
	 * here is logically to a static initializer.
	 */
	private ScriptNotificationManager() {
		// Initialize map with entry points and call list
		log = LogUtil.getLogger(getClass().getPackage().getName());
		j2p = new JavaToPython();
		p2j = new PythonToJava();
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static ScriptNotificationManager getInstance() {
		if (instance == null) {
			synchronized (ScriptNotificationManager.class) {
				instance = new ScriptNotificationManager();
			}
		}
		return instance;
	}
	
	/**
	 * Use hard-coded constants to generate a script path.
	 * If the tag is not found, the module path is empty or the compilation
	 * fails a un will be returned.
	 */
	public Script createScript(String operation) {
		Script script = null;
		try {
			String module = ScriptConstants.NOTIFICATION_MODULE_PATH+"."+operation;
			String args = argumenstForOperation(operation);
			if( !module.endsWith(".")) {
				script = new Script(module,args);
			}
			else {
				log.warnf("%s.createExtensionScript: Failed to create script %s", CLSS,module);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.createExtensionScript: Error creating script for %s", CLSS,operation); 
		}
		return script;
	}
	
	/**
	 * Execute the specified script with a new set of arguments.
	 * 
	 * @param mgr a script manager with proper context
	 * @param className class to which the script applies
	 * @param flavor script category
	 * @param args flavor-specific arguments
	 */
	@SuppressWarnings("unchecked")
	public void runScript(ScriptManager mgr,Script script,Object...args) {
		if( script==null) return;  // We should already have logged an error
		if( script.compileScript()) {
			try {
				script.initializeLocalsMap(mgr);
				List<PyObject> pyargs = new ArrayList<>();
				int index = 0;

				if( args!=null ) {
					for(Object arg:args) {
						if( arg==null ) {
							log.warnf("%s.runScript: null argument in %s",CLSS,script.toString());
							pyargs.add(new PyString(""));
						}
						else {
							PyObject pyarg = j2p.objectToPy(arg);
							log.tracef("%s.runScript: set local variable %d = %s",CLSS,index,pyarg);
							script.setLocalVariable(index, pyarg);
							pyargs.add(pyarg);
						}
						index++;
					}
				}
				else {
					log.warnf("%s.runScript: WARNING: null arguments in %s",CLSS,script.toString());
				}
				if (DEBUG) log.infof("%s.runScript: executing %s",CLSS,script.toString());
				script.execute(mgr);
				// For "complex" arguments, we update contents
				// as a mechanism to return info from the script
				index = 0;
				for(Object arg:args) {
					if( arg instanceof Map) {
						PyObject pyarg = pyargs.get(index);
						p2j.updateMapFromDictionary((Map<String,Object>)arg,(PyDictionary)pyarg);
						log.tracef("%s.runScript: Updating map on return: %s",CLSS,pyarg.toString());
					}
					else if( arg instanceof List) {
						PyObject pyarg = pyargs.get(index);
						List<String> list = p2j.pyListToStringList((PyList)pyarg);
						for(String item:list ) {
							((List)arg).add(item);
						}
						log.tracef("%s.runScript: Updating list on return: %s",CLSS,pyarg.toString());
					}
					else if( arg instanceof GeneralPurposeDataContainer) {
						PyObject pyarg = pyargs.get(index);
						p2j.updateDataContainerFromPython((GeneralPurposeDataContainer)arg,(PyList)pyarg);
						log.tracef("%s.runScript: Updating container on return: %s",CLSS,pyarg.toString());
					}
					index++;
				}
			}
			// NOTE: If the exception message consists of a single digit, it my be referring to an issue with 
			// the nth argument (e.g. wrong numbr of arguments).
			catch(Exception ex) {
				log.warnf("%s.runScript: Exception %s)",CLSS,ex.getMessage());
			}
		}
		else {
			log.warnf("%s.runScript: Failed to compile - %s",CLSS,script.toString());
		}
	}
	
	private String argumenstForOperation(String operation) {
		String args = null;
		if( ScriptConstants.DELETE_NOTIFICATION.equalsIgnoreCase(operation)) {
			args = ScriptConstants.DELETE_SCRIPT_ARGS;
		}
		else if( ScriptConstants.RENAME_NOTIFICATION.equalsIgnoreCase(operation)) {
			args = ScriptConstants.RENAME_SCRIPT_ARGS;
		}
		else if( ScriptConstants.SAVE_NOTIFICATION.equalsIgnoreCase(operation)) {
			args = ScriptConstants.SAVE_SCRIPT_ARGS;
		}
		else {
			throw new IllegalArgumentException("No extension argument list defined for operation "+operation);
		}
		return args;
	}
}
