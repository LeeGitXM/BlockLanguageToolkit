/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.diagnostics.gateway;

import java.util.Hashtable;

import org.python.core.CompileMode;
import org.python.core.CompilerFlags;
import org.python.core.Py;
import org.python.core.PyCode;
import org.python.core.PyDictionary;
import org.python.core.PyList;
import org.python.core.PyObject;

import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.ils.jgx.common.JGXProperties;
import com.inductiveautomation.ignition.common.script.JythonExecException;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;



/**
 *  This handler provides for a specific collection of calls to the Python
 *  layer from the Gateway. In general, the calls are made to update properties 
 *  in the Python objects that represent a block and to trigger their evaluation.
 *  
 *  In addition to the direct updates to Python classes via script execution,
 *  this class posts update notifications regarding those same attribute
 *  changes, expecting that that will be picked up by listeners associated with the UI.
 *  
 *  This class is a singleton for easy access throughout the application.
 */
public class PropertiesUpdateHandler   {
	private final static String TAG = "PropertiesUpdateHandler: ";
	private final LoggerEx log;
	private GatewayContext context = null;
	private static PropertiesUpdateHandler instance = null;
	private final String shareName = "shared";
	private ScriptManager scriptManager = null;
	protected long projectId = 0;
	
	// These are well-known Python methods and their code.
	private final String createInstanceScript = "app.diagnostics.util.createInstance()";
	private PyCode createInstanceCode = null;
	private final String getAttributesScript = "app.diagnostics.util.getAttributes()";
	private PyCode getAttributesCode = null;
	private final String instanceExistsScript = "app.diagnostics.util.instanceExists()";
	private PyCode instanceExistsCode = null;
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private PropertiesUpdateHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static PropertiesUpdateHandler getInstance() {
		if( instance==null) {
			synchronized(PropertiesUpdateHandler.class) {
				instance = new PropertiesUpdateHandler();
			}
		}
		return instance;
	}
	/**
	 * The gateway context must be specified before the instance is useful.
	 * @param cntx the GatewayContext
	 */
	public void setContext(GatewayContext cntx) {
		this.context = cntx;
	}
	
	/**
	 * The script manager is the manager for the Gateway, not projects (?).
	 * It is set in the hook class.
	 * @param mge the script manager
	 */
	public void setScriptManager(ScriptManager mgr) {
		this.scriptManager = mgr;
	}
	
	
	public boolean createInstance(String key,String className) {
		boolean result = false;
		if( createInstanceCode==null) {
			createInstanceCode = compileScript(createInstanceScript);
		}
		if( createInstanceCode!=null ) {
			Hashtable<String,String> arg = new Hashtable<String,String>();
			arg.put(JGXProperties.NAME_KEY, key);
			arg.put(JGXProperties.NAME_CLASS, className);
			PyObject pyDict = new JavaToPython().tableToPyDictionary(arg);
			scriptManager.addGlobalVariable(shareName,pyDict);
			execute(createInstanceCode,createInstanceScript);
			log.debug(TAG+"createInstance returned "+ pyDict);   // Should be updated
		}
		
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public Hashtable<String,?> getAttributes(String key,Hashtable<String,?> attributes) {
		if( getAttributesCode==null) {
			getAttributesCode = compileScript(getAttributesScript);
		}
		Hashtable<String,Object> temp = (Hashtable<String,Object>)attributes;
		if( getAttributesCode!=null ) {
			Hashtable<String,String> att = new Hashtable<String,String>();
			att.put(JGXProperties.ATTRIBUTE_VALUE, key);
			att.put(JGXProperties.ATTRIBUTE_EDITABLE, "False");
			temp.put(JGXProperties.NAME_KEY, att);
			PyDictionary pyDict = new JavaToPython().tableToPyDictionary(temp);
			scriptManager.addGlobalVariable(shareName,pyDict);
			execute(getAttributesCode,getAttributesScript);
			log.debug(TAG+"getAttributes returned "+ pyDict);   // Should now be updated
			attributes = (Hashtable<String,?>)(new PythonToJava().pyDictionaryToTable(pyDict));
			log.debug(TAG+"getAttributes result "+ attributes);  
		}
		return attributes;
	}
	
	public boolean instanceExists(String key) {
		boolean result = false;
		if( instanceExistsCode==null) {
			instanceExistsCode = compileScript(instanceExistsScript);
		}
		if( instanceExistsCode!=null ) {
			Hashtable<String,String> arg = new Hashtable<String,String>();
			arg.put(JGXProperties.NAME_KEY, key);
			PyObject pyDict = new JavaToPython().tableToPyDictionary(arg);
			scriptManager.addGlobalVariable(shareName,pyDict);
			execute(instanceExistsCode,instanceExistsScript);
			log.debug(TAG+"instanceExists returned "+ pyDict);   // Should be updated
		}
		
		return result;
	}
	
	/**
	 * Query Python classes to obtain a list of attributes appropriate for the indicated
	 * block class. If the block is already instantiated, then return the actual attribute
	 * values, as opposed to defaults.
	 * 
	 * @param key
	 * @param attributes
	 * @return the attribute table appropriately enhanced.
	 */
	@SuppressWarnings("unchecked")
	public Hashtable<String,?> getBlockAttributes(String key,Hashtable<String,?> attributes) {
		// If the instance doesn't exist, create one
		if(!ClassRepository.getInstance().getRepository().containsKey(key)) {
			Hashtable<String,String> classAttribute = (Hashtable<String,String>)attributes.get(JGXProperties.NAME_CLASS);
			String className = classAttribute.get(JGXProperties.ATTRIBUTE_VALUE);
			if( className!=null ) {
				createInstance(key,className);
			}
			else {
				log.warn(TAG+"getBlockAttributes: No class in supplied attributes ("+attributes+")");
			}
		}
		Hashtable<String,?> result = getAttributes(key,attributes);
		return result;
	}
	
	/**
	 * Query the Python classes connected at the beginning and end of the connection for a list
	 * of permissible port names. If the connection instance already exists in the Gateway model,
	 * then return the actual port connections.
	 * 
	 * @param key
	 * @param attributes
	 * @return
	 */
	public Hashtable<String,?> getConnectionAttributes(String key,Hashtable<String,?> attributes) {
		return attributes;
	}
	
	
	/**
	 * Notification received when one of the models has completed 
	 * its computation. If all models are complete, then the set
	 * is complete and do the model set completion processing. 
	 */
	public void processCompletion(PyList list) {
		
		/*
		//----- call a gateway script -----
		log.info(String.format("%sprocessModelComplete - calling %s (results in variable %s)",TAG,scriptName,resultsName));
		if( scriptCode!=null && scriptManager!=null) {  
			// All models are complete. Report back to the Gateway. We've acquired the results.
			// The initial dictionary contains processing directives.
			scriptManager.addGlobalVariable(resultsName, list);
			ScriptRunner srunner = new ScriptRunner(scriptManager,scriptCode);
			scriptExecutor.execute(srunner);
		}
		else {
			log.warn(String.format("%sprocessModelComplete  - failed to execute call back script (it didn't compile)",TAG));
		}
	   */
	}
	
	
	/**
	 * Compile a simple script that does nothing but call the specified script. 
	 * We assume that the script name includes a module, plus script name, plus method.
	 * We want the module for our import statement.
	 * @param scriptToCall the script we wish to execute.
	 * @return the compiled python script
	 */
	private PyCode compileScript(String scriptToCall) {
		PyCode code = null;
		String module = "app.diagnostics";
		int index = scriptToCall.lastIndexOf(".");
		if( index>0 ) module = scriptToCall.substring(0,index);
		index = module.lastIndexOf(".");
		if( index>0 ) module = module.substring(0,index);
		String script = String.format("import %s;%s",module,scriptToCall);
		log.debug(TAG+"Compiling callback script \n"+script);
		try {
	         code = Py.compile_flags(script,module,CompileMode.exec,CompilerFlags.getCompilerFlags());
	         
	     }
		catch(Exception ex) {
			log.error(TAG+"Failed to compile script "+script);
		}
		return code;
	}
	
	/**
	 *  Run the call-back script in line. On completion return the contents of the shared variable.
	 */
	public void execute(PyCode code,String scriptName) {
		log.trace(TAG+"Running callback script ...");
		try {
			scriptManager.runCode(code,scriptManager.createLocalsMap());
		}
		catch( JythonExecException jee) {
			log.error(TAG+"JythonException executing python "+scriptName+ " ("+jee.getMessage()+")",jee);
		}
		catch(Exception ex) {
			log.error(TAG+"Error executing python "+scriptName+ " ("+ex.getMessage()+")",ex);
		}
		log.trace(TAG+"Completed callback script.");
	}

}
