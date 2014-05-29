/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

import org.python.core.PyCode;
import org.python.core.PyObject;
import org.python.core.PyStringMap;

import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * A callback is a holder for parameters that define specific
 *  python modules. This is the base class for the set. Each
 *  callback corresponds to a specific module that is expected
 *  to have been loaded into Ignition.
 *  
 * Note that the callback class exposes public members instead of methods. 
 */
public class Callback {
	private final static String TAG = "Callback";
	private final LoggerEx log;
	public PyCode code;
	public String module;
	public String pythonPackage;
	protected String[] localVariables;      // Derived from comma-separated
	protected String localVariableList;
	protected ScriptManager scriptManager = null;
	protected PyStringMap localsMap = null;


	public Callback() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		pythonPackage = "ils.blt.util";      // Default
		module = "";
		scriptManager = null;
		localVariables = new String[0];
		localVariableList="";
		code = null;
	}

	public void setScriptManager(ScriptManager mgr) {
		this.scriptManager = mgr;
	}
	public ScriptManager getScriptManager() { return this.scriptManager; }
	public String getLocalVariableList() { return localVariableList; }
	/**
	 * Convert the comma-separated variable string into an array of strings.
	 */
	public void setLocalVariableList(String varlist) {
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
		log.infof("%s.Callback.setLocalVariable: %s to %s",TAG,localVariables[index],value.toString());
	}
	/**
	 * Strip off any parentheses that might be supplied.
	 * @param truthState the single local argument
	 */
	public void setModule(String mod) {
		if( mod==null) return;
		module = mod;
		int index = mod.lastIndexOf("(");
		if( index>0 ) module = mod.substring(0,index);
	}
	public PyStringMap getLocalsMap() { return this.localsMap; }
}

