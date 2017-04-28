/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;

import org.python.core.CompileMode;
import org.python.core.CompilerFlags;
import org.python.core.Py;
import org.python.core.PyCode;
import org.python.core.PyObject;
import org.python.core.PyStringMap;

import com.inductiveautomation.ignition.common.script.JythonExecException;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * Scripts are initiated by a Java class in either Gateway or Client/Designer scopes.
 * Each script is independently compiled and cached (via the ScriptExtensionManager).
 * Different script managers can be used to execute the same script (not in parallel).
 * The standard pattern for execution is: 
 *    if( script.compileScript() ) {
 *			// There are 2 values to be specified - id, properties.
 *			script.initializeLocalsMap(mgr);
 *			script.setLocalVariable(0,a);
 *			script.setLocalVariable(1,b);
 *                 . . .
 *			script.execute(mgr);
 *	   }
 *   
 * Individual subclasses simply define the execution entry point and arg list. 
 */
public class Script {
	private final static String TAG = "Script";
	private final LoggerEx log;
	private PyCode code;
	private final String entry; 
	private String module = "";
	private String pythonPackage;
	private String[] localVariables;      // Derived from comma-separated
	private final String localVariableList;
	private PyStringMap localsMap = null;

	/**
	 * Create a new executable script
	 * @param ep nominal entry point into script (unused)
	 * @param args the script arguments
	 */
	public Script(String ep,String args) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.entry = ep;
		this.pythonPackage = ""; 
		this.module = "";
		this.localVariables = args.split(",");
		this.localVariableList=args;
		this.code = null;
	}
	
	public void resetModulePath(String pythonPath) { 
		if( pythonPath!=null && pythonPath.length()>0 ) {
			this.module = moduleNameFromPath(pythonPath);
			this.pythonPackage = packageNameFromPath(pythonPath);
			//log.debugf("%s.resetModulePath: %s:%s from %s",TAG,pythonPackage,module,pythonPath);
		}
		this.code = null;   // Needs compiling
	}
	
	/**
	 * Compile a simple script that does nothing but call the specified script. 
	 * We assume that the script name includes a module, plus script name, plus method.
	 * We want the module for our import statement.
	 * @return true if the script compiled
	 */
	public boolean compileScript() {
		if( module.length()==0 ) return false;     // Module is unset
		if( code !=null  )       return true;      // Already compiled               
		String script = String.format("import %s;%s.%s(%s)",pythonPackage,pythonPackage,module,localVariableList);
		try {
			code = Py.compile_flags(script,pythonPackage,CompileMode.exec,CompilerFlags.getCompilerFlags());
	     }
		catch(Exception ex) {
			log.errorf("%s.compileScript: Failed to compile script \n%s",TAG,script,ex);
		}
		return code!=null;
	}
	/**
	 *  Run the script in line. On completion return the contents of the shared variable.
	 *  @param scriptManager the current script manager. This should be refreshed with every use
	 */
	public void execute(ScriptManager scriptManager) {
		if( module.length()==0 ) return;   // Do nothing
		if( localsMap == null ) throw new IllegalArgumentException("Attempt to execute with uninitialized locals map.");
		String script = pythonPackage+"."+module;
		log.debugf("%s.execute: Running callback script (%s)",TAG,script);
		try {
			scriptManager.runCode(code,localsMap);
		}
		catch( JythonExecException jee) {
			log.error(String.format("%s.execute: JythonException executing python %s (%s) ",TAG,script,jee.getMessage()),jee);
		}
		catch(Exception ex) {
			log.error(String.format("%s.execute: Error executing python %s (%s)",TAG,script,ex.getMessage()+")"),ex);
		}
		log.tracef("%s: Completed callback script.",TAG);
		localsMap = null;
	}


	/**
	 * Clear the locals map. This must be called before any local variables are 
	 * defined.
	 * 
	 * @param scriptManager script runner appropriate to the block
	 *        upon which this is executed.
	 */
	public void initializeLocalsMap(ScriptManager scriptManager) {
		localsMap = scriptManager.createLocalsMap();
	}
	/**
	 * Setting a variable value creates a locals map. We need to set the
	 * variables in the order that their names appear in the list.
	 * @param index is the variable position in the argument list.
	 * @param value the single local argument
	 */
	public void setLocalVariable(int index,PyObject value) {
		if( localsMap == null ) throw new IllegalArgumentException("Locals map must be initialized before variables can be added.");
		if( localVariables.length<=index ) throw new IllegalArgumentException(
										String.format("%s.setLocalVariable %d, but defined list is: %s",entry,index,localVariableList));

		localsMap.__setitem__(localVariables[index],value);
		log.debugf("%s.setLocalVariable: %s to %s",TAG,localVariables[index],value.toString());
	}
	
	// Strip off the last segment and return the rest.
	private String packageNameFromPath(String path) {
		int index = path.lastIndexOf(".");
		if( index>0 ) path = path.substring(0, index);
		return path;
	}
	
	// Strip off the last segment and return the rest.
	private String moduleNameFromPath(String path) {
		int index = path.lastIndexOf(".");
		if( index>0 ) path = path.substring(index+1);
		return path;
	}
}

