/**
 *   (c) 2015-2021  ILS Automation. All rights reserved. 
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
import com.ils.common.tag.TagReader;
import com.inductiveautomation.ignition.common.model.CommonContext;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  The manager is used to compile and execute Python scripts.
 *  Scripts are compiled with every execution. Those that are empty or null are logged and ignored.
 *  
 *  Manager instances exist in all scopes.
 */
public class ScriptExtensionManager {
	private static String CLSS = "ScriptExtensionManager";
	protected final LoggerEx log;
	private static final boolean DEBUG = false;
	protected final JavaToPython j2p;
	protected final PythonToJava p2j;
	private CommonContext context = null;
	private static ScriptExtensionManager instance = null;
	
	
	/**
	 * The handler, make this private per Singleton pattern. The initialization
	 * here is logically to a static initializer.
	 */
	private ScriptExtensionManager() {
		// Initialize map with entry points and call list
		log = LogUtil.getLogger(getClass().getPackageName());
		j2p = new JavaToPython();
		p2j = new PythonToJava();
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static ScriptExtensionManager getInstance() {
		if (instance == null) {
			synchronized (ScriptExtensionManager.class) {
				instance = new ScriptExtensionManager();
			}
		}
		return instance;
	}
	
	/**
	 * Set the context in order to get the tag manager.
	 * @param ctx
	 */
	public void setContext(CommonContext ctx) { this.context = ctx; }
	/**
	 * Read a configuration tag to get a script string, then compile it.
	 * If the tag is not found, the module path is empty or the compilation
	 * fails a un will be returned.
	 */
	public Script createExtensionScript(String className, String operation, String provider) {
		Script script = null;
		try {
			String path = tagPathForClass(className);
			String moduleName = moduleFromTag(provider,path,operation);
			String args = argumentsForOperation(operation);
			if( moduleName != null) {
				script = new Script(moduleName,args);
			}
			else {
				log.warnf("%s.createExtensionScript: Failed to create script for %s.%s (provider=%s)", CLSS,className,operation,provider);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.createExtensionScript: Error creating script for %s.%s (provider=%s)", CLSS,className,operation,provider); 
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
			// the nth argument (e.g. wrong number of arguments).
			catch(Exception ex) {
				log.warnf("%s.runScript: Exception %s)",CLSS,ex.getMessage());
			}
		}
		else {
			log.warnf("%s.runScript: Failed to compile - %s",CLSS,script.toString());
		}
	}
	
	private String argumentsForOperation(String operation) {
		String args = null;
		if( ScriptConstants.DELETE_OPERATION.equalsIgnoreCase(operation)) {
			args = ScriptConstants.DELETE_SCRIPT_ARGS;
		}
		else if( ScriptConstants.GET_AUX_OPERATION.equalsIgnoreCase(operation)) {
			args = ScriptConstants.GET_AUX_SCRIPT_ARGS;
		}
		else if( ScriptConstants.GET_LIST_OPERATION.equalsIgnoreCase(operation)) {
			args = ScriptConstants.GET_LIST_SCRIPT_ARGS;
		}
		else if( ScriptConstants.RENAME_OPERATION.equalsIgnoreCase(operation)) {
			args = ScriptConstants.RENAME_SCRIPT_ARGS;
		}
		else if( ScriptConstants.SAVE_OPERATION.equalsIgnoreCase(operation)) {
			args = ScriptConstants.SAVE_SCRIPT_ARGS;
		}
		else if( ScriptConstants.SET_AUX_OPERATION.equalsIgnoreCase(operation)) {
			args = ScriptConstants.SET_AUX_SCRIPT_ARGS;
		}
		else {
			throw new IllegalArgumentException("No extension argument list defined for operation "+operation);
		}
		return args;
	}
	private String moduleFromTag(String provider,String path,String operation) {
		String module = "";
		if(path!=null && operation!=null && context!=null) {
			TagReader treader = new TagReader(context);
			path = String.format("[%s]%s/%s", provider,path,operation);
			QualifiedValue qv = treader.readTag(path);
			if(qv!=null && qv.getValue()!=null) module = qv.getValue().toString();
		}
		return module;
	}
	private String tagPathForClass(String className) {
		String path = null;
		if( ScriptConstants.APPLICATION_CLASS_NAME.equalsIgnoreCase(className)) {
			path = ScriptConstants.APPLICATION_TAG_PATH;
		}
		else if( ScriptConstants.FAMILY_CLASS_NAME.equalsIgnoreCase(className)) {
			path = ScriptConstants.FAMILY_TAG_PATH;
		}
		else if( ScriptConstants.DIAGRAM_CLASS_NAME.equalsIgnoreCase(className)) {
			path = ScriptConstants.DIAGRAM_TAG_PATH;
		}
		else {
			throw new IllegalArgumentException("No extension tag path defined for class "+className);
		}
		return path;
	}
	
}
