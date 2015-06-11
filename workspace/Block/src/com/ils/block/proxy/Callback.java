/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.block.proxy;

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
 * Callbacks are initiated by the Proxy block and made into the Python code for 
 * which it is a proxy. These are standard commands, global for all blocks.
 * However, the ScriptManager which executes them is project-dependent. The
 * standard usage is to create a new locals map with each execution.
 * 
 * The standard execution pattern is:
 *    if( callback.compileScript() ) {
 *			// There are 4 values to be specified - block,port,value,quality.
 *			callback.initializeLocalsMap(mgr);
 *			callback.setLocalVariable(0,a);
 *			callback.setLocalVariable(1,b);
 *               . . .
 *			callback.execute(mgr);
 *	   }
 *    
 * 
 * A callback is a holder for parameters that define specific
 *  python modules. This is the base class for the set. Each
 *  callback corresponds to a specific module that is expected
 *  to have been loaded into Ignition.
 *  
 * Note that the callback class exposes public members instead of methods. 
 */
public class Callback {
	private final static String TAG = "Callback";
	protected final LoggerEx log;
	private PyCode code;
	protected String module;
	protected String pythonPackage;
	private String[] localVariables  = new String[0];;      // Derived from comma-separated
	private String localVariableList = "";
	private PyStringMap localsMap = null;


	public Callback() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		pythonPackage = "ils.blt.util";      // Default
		module = "";
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
	 *  Run the script in-line. On completion return the contents of the shared variable.
	 */
	public void execute(ScriptManager scriptManager) {
		if( localsMap == null ) throw new IllegalArgumentException("Attempt to execute with uninitialized locals map.");
		String script = pythonPackage+"."+module;
		log.tracef("%s.execute: Running callback script ...(%s)",TAG,script);
		try {
			scriptManager.runCode(code,localsMap);
		}
		catch( JythonExecException jee) {
			log.error(String.format("%s.execute: JythonExecException executing python %s(%s)",TAG,script,localVariableList),jee);
		}
		catch(Exception ex) {
			log.error(String.format("%s.execute: Error executing python %s(%s) (%s)",TAG,script,localVariableList,ex.getMessage()+")"),ex);
		}
		log.tracef("%s: Completed callback script.",TAG);
		localsMap = null;
	}
	

	/**
	 * Convert the comma-separated variable string into an array of strings.
	 */
	protected void setLocalVariableList(String varlist) {
		localVariableList = varlist;
		localVariables = varlist.split(",");
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

		localsMap.__setitem__(localVariables[index],value);
		log.debugf("%s.setLocalVariable: %s to %s",TAG,localVariables[index],value.toString());
	}
}

