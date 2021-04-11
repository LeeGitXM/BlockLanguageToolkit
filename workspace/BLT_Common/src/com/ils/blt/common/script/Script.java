/**
 *   (c) 2015,2018  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.script;

import org.python.core.CompileMode;
import org.python.core.CompilerFlags;
import org.python.core.Py;
import org.python.core.PyCode;
import org.python.core.PyObject;
import org.python.core.PyStringMap;

import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.script.JythonExecException;
import com.inductiveautomation.ignition.common.script.ScriptManager;


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
	private final static String CLSS = "Script";
	private final boolean DEBUG = false;
	private final ILSLogger log;
	private PyCode code;
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
	public Script(String pythonPath,String args) {
		this.log = LogMaker.getLogger(this);
		setModulePath(pythonPath);
		this.localVariables = args.split(",");
		this.localVariableList=args;
		this.code = null;
	}
	
	public void setModulePath(String pythonPath) { 
		if( pythonPath!=null && pythonPath.length()>0 ) {
			this.module = moduleNameFromPath(pythonPath);
			this.pythonPackage = packageNameFromPath(pythonPath);
			if(DEBUG) log.infof("%s.resetModulePath: %s:%s from %s",CLSS,pythonPackage,module,pythonPath);
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
		if(DEBUG) log.infof("%s.compileScript: %s",CLSS,script);
		try {
			code = Py.compile_flags(script,pythonPackage,CompileMode.exec,CompilerFlags.getCompilerFlags());
	     }
		catch(Exception ex) {
			log.errorf("%s.compileScript: Failed to compile script \n%s",CLSS,script,ex);
		}
		return code!=null;
	}
	/**
	 *  Run the script in line. On completion return the contents of the shared variable.
	 *  Note: A stacktrace in the exception clause is not helpful as it simply goes back to the script manager.
	 *  @param scriptManager the current script manager. This should be refreshed with every use
	 */
	public void execute(ScriptManager scriptManager) {
		if( module.length()==0 ) return;   // Do nothing
		if( localsMap == null ) throw new IllegalArgumentException("Attempt to execute with uninitialized locals map.");
		String script = pythonPackage+"."+module;
		if(DEBUG) log.infof("%s.execute: Running callback script (%s)",CLSS,script);
		try {
			scriptManager.runCode(code,localsMap);
		}
		catch( JythonExecException jee) {
			log.error(String.format("%s.execute: JythonException executing python %s (%s) ",CLSS,script,jee.getMessage()));
		}
		catch(Exception ex) {
			log.error(String.format("%s.execute: Error executing python %s (%s)",CLSS,script,ex.getMessage()+")"),ex);
		}
		log.tracef("%s: Completed callback script.",CLSS);
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
		if(DEBUG) log.infof("%s.setLocalVariable: setting %s to %s",CLSS,localVariables[index],value.toString());
		if( localVariables.length<=index ) throw new IllegalArgumentException(
				String.format("%s.setLocalVariable %d, but defined list is: %s",module,index,localVariableList));

		localsMap.__setitem__(localVariables[index],value);
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
	
	@Override
	public String toString() {
		return (pythonPackage==null?"module path unset":this.pythonPackage+"."+this.module);
	}
}

