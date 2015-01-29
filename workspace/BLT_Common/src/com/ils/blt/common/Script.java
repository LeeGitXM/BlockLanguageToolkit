/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

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
 * A script is a holder for an executable python module. Each script to
 * be executed is a separate instance of the class. The script isn't 
 * compiled until it is executed. 
 */
public class Script {
	private final static String TAG = "Script";
	protected final LoggerEx log;
	private PyCode code;
	protected String module;
	protected String pythonPackage;
	private String[] localVariables;      // Derived from comma-separated
	private String localVariableList;
	private ScriptManager scriptManager = null;
	private PyStringMap localsMap = null;


	public Script() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		pythonPackage = "ils.blt.util";      // Default
		module = "";
		scriptManager = null;
		localVariables = new String[0];
		localVariableList="";
		code = null;
	}
	
	/**
	 * Compile a simple script that does nothing but call the specified script. 
	 * We assume that the script name includes a module, plus script name, plus method.
	 * We want the module for our import statement.
	 * @param callbackIndex index of the script we wish to execute.
	 * @return true if the script compiled
	 */
	public boolean compileScript() {
		if( code !=null ) return true;   // Already compiled               
		String script = String.format("import %s;%s.%s(%s)",pythonPackage,pythonPackage,module,localVariableList);
		log.infof("%s.compileScript: Compiling ... %s",TAG,script);
		try {
			code = Py.compile_flags(script,pythonPackage,CompileMode.exec,CompilerFlags.getCompilerFlags());
	     }
		catch(Exception ex) {
			log.errorf("%s.compileScript: Failed to compile script \n%s",TAG,script);
		}
		return code!=null;
	}
	/**
	 *  Run the script in line. On completion return the contents of the shared variable.
	 */
	public void execute() {
		String script = pythonPackage+"."+module;
		log.infof("%s.execute: Running callback script ...(%s)",TAG,script);
		try {
			scriptManager.runCode(code,getLocalsMap());
		}
		catch( JythonExecException jee) {
			log.error(String.format("%s.execute: JythonException executing python %s (%s) ",TAG,script,jee.getMessage()),jee);
		}
		catch(Exception ex) {
			log.error(String.format("%s.execute: Error executing python %s (%s)",TAG,script,ex.getMessage()+")"),ex);
		}
		log.infof("%s: Completed callback script.",TAG);
	}
	
	public void setScriptManager(ScriptManager mgr) {
		this.scriptManager = mgr;
	}
	/**
	 * Convert the comma-separated variable string into an array of strings.
	 */
	protected void setLocalVariableList(String varlist) {
		localVariableList = varlist;
		localVariables = varlist.split(",");
	}

	/**
	 * Setting a variable value creates a locals map. We need to set the
	 * variables in the order that their names appear in the list.
	 * @param index is the variable position in the argument list.
	 * @param value the single local argument
	 */
	public void setLocalVariable(int index,PyObject value) {
		if( localsMap == null ) localsMap = scriptManager.createLocalsMap();

		localsMap.__setitem__(localVariables[index],value);
		log.debugf("%s.setLocalVariable: %s to %s",TAG,localVariables[index],value.toString());
	}

	public PyStringMap getLocalsMap() { return this.localsMap; }
}

