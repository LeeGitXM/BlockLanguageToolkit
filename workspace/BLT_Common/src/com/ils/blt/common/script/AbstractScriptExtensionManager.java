/**
 *   (c) 2015-2018  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.python.core.PyDictionary;
import org.python.core.PyList;
import org.python.core.PyObject;
import org.python.core.PyString;

import com.ils.blt.common.block.BlockDescriptor;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  The manger is an abstract base class used to compile and execute Python scripts.
 *  The scripts come in 4 flavors (PROPERTY_GET_SCRIPT,PROPERTY_SET_SCRIPT and 
 *  NODE_DELETE_SCRIPT, NODE_RENAME_SCRIPT, NODE_SAVE_SCRIPT). The standard signatures are:
 *  	get/set(uuid,properties,db)
 *  	delete(uuid)
 *  	rename(uuid,oldName,newName)
 *  	save(uuid)
 *  This group of scripts must be defined 
 *  for every class that wants interact with external data.
 *  
 *  We maintain a script table to retain compiled versions of the scripts. Script 
 *  local variables are updated on each invocation.
 *  
 *  Concrete versions exist in both gateway and client/designer scopes.
 */
public abstract class AbstractScriptExtensionManager {
	private static String CLSS = "AbstractScriptExtensionManager";
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
		flavors.add(ScriptConstants.NODE_DELETE_SCRIPT);
		flavors.add(ScriptConstants.NODE_RENAME_SCRIPT);
		flavors.add(ScriptConstants.NODE_SAVE_SCRIPT);
		flavors.add(ScriptConstants.PROPERTY_GET_SCRIPT);
		flavors.add(ScriptConstants.PROPERTY_SET_SCRIPT);
	}
	
	/**
	 * Adding a script has an advantage over an ad-hoc execution in that
	 * the compilation is cached.
	 * @param className class to which the script applies
	 * @param flavor script category
	 * @param modulePath path to module. we append the method
	 */
	public void addScript(String className,String flavor,String modulePath) {
		String key = makeKey(className,flavor);
		//log.infof("%s.addScript: %s, %s", CLSS,className,flavor);
		String entry = "execute";
		String arglist = "";
		if( flavor.equals(ScriptConstants.PROPERTY_GET_SCRIPT))  {
			entry = "getAux";
			arglist = "uuid,properties,db";
		}
		else if( flavor.equals(ScriptConstants.PROPERTY_SET_SCRIPT))  {
			entry = "setAux";
			arglist = "uuid,properties,db";
		}
		else if( flavor.equals(ScriptConstants.NODE_DELETE_SCRIPT))  {
			entry = "delete";
			arglist = "uuid";
//			arglist = "uuid,aux";
		}
		else if( flavor.equals(ScriptConstants.NODE_RENAME_SCRIPT))  {
			entry = "rename";
			arglist = "uuid,oldname,newname";
		}
		else if( flavor.equals(ScriptConstants.NODE_SAVE_SCRIPT))  {
			entry = "save";
			arglist = "uuid,aux";
		}
		scriptMap.put(key,createMap(entry,arglist));
		setModulePath(key,modulePath);
		log.debugf("%s.addScript: %s is %s",CLSS,key,modulePath);
	}
	
	public List<String> getFlavors() {
		return flavors;
	}
	
	/**
	 * Query the blocks and return a list of classes that require
	 * external interface scripts. We re-query each time we're asked.
	 * @return a list of class names
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
	 * @return a list of descriptors for this class
	 */
	public abstract  List<BlockDescriptor> getClassDescriptors();
	
	/**
	 * Execute the specified script with a new set of arguments.
	 * 
	 * @param mgr a script manager with proper context
	 * @param className class to which the script applies
	 * @param flavor script category
	 * @param args flavor-specific arguments
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
					
					if( args!=null ) {
						for(Object arg:args) {
							if( arg==null ) {
								log.infof("%s.runScript: null argument in %s",CLSS,script.toString());
								pyargs.add(new PyString(""));
							}
							else {
								PyObject pyarg = j2p.objectToPy(arg);
								script.setLocalVariable(index, pyarg);
								pyargs.add(pyarg);
							}
							index++;
						}
					}
					else {
						log.warnf("%s.runScript: WARNING: null arguments in %s",CLSS,script.toString());
					}
					log.infof("%s.runScript: %s",CLSS,script.toString());
					script.execute(mgr);
					// For "complex" arguments, we update contents
					// as a mechanism to return info from the script
					index = 0;
					for(Object arg:args) {
						if( arg instanceof Map) {
							PyObject pyarg = pyargs.get(index);
							p2j.updateMapFromDictionary((Map<String,Object>)arg,(PyDictionary)pyarg);
							log.debugf("%s.runScript: Updating map on return: %s",CLSS,pyarg.toString());
						}
						else if( arg instanceof GeneralPurposeDataContainer) {
							PyObject pyarg = pyargs.get(index);
							p2j.updateDataContainerFromPython((GeneralPurposeDataContainer)arg,(PyList)pyarg);
							log.debugf("%s.runScript: Updating container on return: %s",CLSS,pyarg.toString());
						}
						index++;
					}
				}
				catch(Exception ex) {
					log.warnf("%s.runScript: Exception (%s)",CLSS,ex.getMessage(),ex);
				}
			}
		}
		else {
			log.warnf("%s.runScript: Unknown python script type (%s)",CLSS,key);
		    System.out.println();
		}
	}
	
	/**
	 * This method is for use with scripts that are not globally specified,
	 * for example, those that are unique to a block or block class. The
	 * key specifies the arg list, but, in this case, not the module path.
	 * 
	 * @param mgr a script manager with proper context
	 * @param pythonPath the Python path to the module package, not including the entry point.
	 * @param className class to which the script applies
	 * @param flavor script category
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
	 * @param pythonPath to module
	 */
	public void setModulePath(String key,String pythonPath) {
		Map<String,Object> map = scriptMap.get(key);
		if( map!=null ) {
			Script script = (Script)map.get(ScriptConstants.SCRIPT_KEY);
			script.resetModulePath(pythonPath);
		}
		else {
			log.tracef("%s.setModulePath: Unknown python script type (%s)",CLSS,key);
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
	/**
	 * Test for existence of a external key
	 * @param className name of the node class
	 * @param flavor type of script
	 * @return true if the className/flavor combination points to a module
	 */
	public boolean hasKey(String className,String flavor) {
		String key = makeKey(className,flavor);
		return scriptMap.containsKey(key);
	}
}
