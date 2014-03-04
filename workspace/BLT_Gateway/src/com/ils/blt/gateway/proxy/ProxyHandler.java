/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.UUID;

import org.python.core.CompileMode;
import org.python.core.CompilerFlags;
import org.python.core.Py;
import org.python.core.PyCode;
import org.python.core.PyDictionary;
import org.python.core.PyObject;

import com.ils.block.common.BlockConstants;
import com.ils.block.common.BlockProperty;
import com.ils.blt.common.BLTProperties;
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
public class ProxyHandler   {
	private final static String TAG = "ProxyHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private static ProxyHandler instance = null;
	
	// Maintain a map of callbacks keyed by class name
	private final Hashtable<String,Callback[]> callbackMap;
	// These are the indices of specific callback functions within the array
	private static int CREATE_INSTANCE = 0;
	private static int EVALUATE = 1;
	private static int GET_CLASSES = 2;
	private static int GET_PROPERTIES = 3;
	private static int GET_PROPERTY = 4;
	private static int SET_VALUE = 5;
	private static int CALLBACK_COUNT = 6;
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private ProxyHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		callbackMap = new Hashtable<String,Callback[]>();
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static ProxyHandler getInstance() {
		if( instance==null) {
			synchronized(ProxyHandler.class) {
				instance = new ProxyHandler();
			}
		}
		return instance;
	}
	
	/**
	 * Set parameters for a callback. We only support a single project as the server 
	 * of each package of callbacks.
	 * @param type a string denoting the callback kind. Valid values are found in BlockProperties.
	 * @param pkg the package of classes which this project handles
	 * @param project name of the project that is the block code repository
	 * @param module the python code module. Must be in package app.block.
	 * @param variable the name of the global variable that will hold both function arguments and results.e
	 */
	public void register(String type, String pkg, String project, String module,String variable) {
		Callback[] callbacks = callbackMap.get(pkg);
		if( callbacks==null) {
			callbacks = new Callback[CALLBACK_COUNT];
			callbackMap.put(pkg, callbacks);
		}
		long projectId = context.getProjectManager().getProjectId(project);
		if( projectId<0) {
			log.warnf("%s: register: Unknown project (%s)",TAG,project);
			return;
		}
		int index = 0;
		
		if( type.equalsIgnoreCase(BLTProperties.CREATE_INSTANCE_CALLBACK)) {
			index = CREATE_INSTANCE;
		}
		else if( type.equalsIgnoreCase(BLTProperties.EVALUATE_CALLBACK)) {
			index = EVALUATE;
		}
		else if( type.equalsIgnoreCase(BLTProperties.GET_BLOCK_CLASSES_CALLBACK)) {
			index = GET_CLASSES;
		}
		else if( type.equalsIgnoreCase(BLTProperties.GET_PROPERTIES_CALLBACK)) {
			index = GET_PROPERTIES;
		}
		else if( type.equalsIgnoreCase(BLTProperties.GET_PROPERTY_CALLBACK)) {
			index = GET_PROPERTY;
		}
		else if( type.equalsIgnoreCase(BLTProperties.SET_VALUE_CALLBACK)) {
			index = SET_VALUE;
		}
		else {
			log.warnf("%s: register: unknown registration method (%s)",TAG,type);
			return;
		}
		// Now that we have the callback type, add to the array
		Callback callback = callbacks[index];
		if( callback == null ) {
			callback = new Callback(type);
			callbacks[index] = callback;	
		}
		callback.projectId = projectId;
		callback.scriptManager = context.getProjectManager().getProjectScriptManager(projectId);
		callback.module = module;
		callback.variable = variable;
		
	}
	/**
	 * The gateway context must be specified before the instance is useful.
	 * @param cntx the GatewayContext
	 */
	public void setContext(GatewayContext cntx) {
		this.context = cntx;
	}



	@SuppressWarnings("unchecked")
	public List<String> getClassNames() {

		List<String> temp = new ArrayList<String>();
		/*
		if( compileScript(getAttributesCallback) ) {
			Hashtable<String,String> att = new Hashtable<String,String>();
			att.put(BlockConstants.BLOCK_ATTRIBUTE_VALUE, key);
			att.put(BlockConstants.BLOCK_ATTRIBUTE_EDITABLE, "False");
			//temp.put(BlockProperties.NAME_KEY, att);
			PyDictionary pyDict = new JavaToPython().tableToPyDictionary(temp);
			getAttributesCallback.scriptManager.addGlobalVariable(getAttributesCallback.variable,pyDict);
			execute(getAttributesCallback);
			log.debug(TAG+"getAttributes returned "+ pyDict);   // Should now be updated
			attributes = (Hashtable<String,?>)(new PythonToJava().pyDictionaryToTable(pyDict));
			log.debug(TAG+"getAttributes result "+ attributes);  
		}
		*/
		return temp;
	}


	/**
	 * Query a Python class to obtain a list of its properties. The block is expected
	 * to exist.
	 * 
	 * @param block the python object
	 * @return the property table with current values
	 */
	public BlockProperty[] getProperties(PyObject block) {
		 
		// Place the instance in the shared dictionary
		PyDictionary pyDict = new PyDictionary();
		pyDict.put(BlockConstants.BLOCK_PROPERTY_INSTANCE, block);
/*
		if(!ClassRepository.getInstance().getRepository().containsKey(key)) {
			Hashtable<String,String> classAttribute = (Hashtable<String,String>)attributes.get(BlockConstants.BLOCK_PROPERTY_CLASS);
			String className = classAttribute.get(BlockConstants.BLOCK_ATTRIBUTE_VALUE);
			if( className!=null ) {
				createInstance(key,className);
			}
			else {
				log.warn(TAG+"getBlockAttributes: No class in supplied attributes ("+attributes+")");
			}
		}
		Hashtable<String,?> result = getAttributes(key,attributes);
		return result;
		*/
		return null;
	}

	/**
	 * Tell the block to do whatever it is supposed to do.
	 * 
	 * @param projectId
	 * @param diagramId
	 * @param blockId
	 */
	public void evaluate(long projectId,long diagramId,UUID blockId) {
		
	}
	
	/**
	 * Inform the block that it has a new value on one of its inputs
	 * 
	 * @param projectId
	 * @param diagramId
	 * @param blockId
	 * @param stub
	 * @param value one of a QualifiedValue, Signal, Truth-value or String
	 */
	public void setValue(long projectId,long diagramId,UUID blockId,String stub,Object value) {
		
	}
	

	/**
	 * Create an instance of the specified class
	 * @param className
	 * @param callback
	 * @return the new instance, or null
	 */
	private PyObject createInstance(String className,Callback callback) {
		PyObject result = null;
		/*
		if( compileScript(callback) ) {
			Hashtable<String,String> arg = new Hashtable<String,String>();
			arg.put(BlockConstants.BLOCK_PROPERTY_CLASS, className);
			PyDictionary pyDict = new JavaToPython().tableToPyDictionary(arg);
			callback.scriptManager.addGlobalVariable(callback.variable,pyDict);
			execute(callback);
			log.debug(TAG+"createInstance returned "+ pyDict);   // Should be updated
			PyString key = new PyString(BlockConstants.BLOCK_PROPERTY_INSTANCE);
			result = pyDict.get(key);
		}
		*/
		return result;
	}
	/**
	 * Compile a simple script that does nothing but call the specified script. 
	 * We assume that the script name includes a module, plus script name, plus method.
	 * We want the module for our import statement.
	 * @param scriptToCall the script we wish to execute.
	 * @return true if the script compiled
	 */
	private boolean compileScript(Callback scriptToCall) {
		if( scriptToCall.code !=null ) return true;   // Already compiled
		String module = "app";
		int index = scriptToCall.module.lastIndexOf(".");
		if( index>0 ) module = scriptToCall.module.substring(0,index);
		index = module.lastIndexOf(".");
		if( index>0 ) module = module.substring(0,index);
		String script = String.format("import %s;%s",module,scriptToCall.module);
		log.debug(TAG+"Compiling callback script \n"+script);
		try {
	         scriptToCall.code = Py.compile_flags(script,module,CompileMode.exec,CompilerFlags.getCompilerFlags());
	         
	     }
		catch(Exception ex) {
			log.errorf("%s: Failed to compile script \n%s",TAG,script);
		}
		return scriptToCall.code!=null;
	}
	
	/**
	 *  Run the call-back script in line. On completion return the contents of the shared variable.
	 */
	public void execute(Callback callback) {
		log.tracef("%s Running callback script ...(%s)",TAG,callback.module);
		try {
			callback.scriptManager.runCode(callback.code,callback.scriptManager.createLocalsMap());
		}
		catch( JythonExecException jee) {
			log.error(TAG+": JythonException executing python "+callback.module+ " ("+jee.getMessage()+")",jee);
		}
		catch(Exception ex) {
			log.error(TAG+": Error executing python "+callback.module+ " ("+ex.getMessage()+")",ex);
		}
		log.tracef("%s: Completed callback script.",TAG);
	}
	
	/**
	 * Search the callback map for a suitable provider given a class name
	 */
	private Callback[] getCallbacks(String className) {
		Callback[] callbacks = null;
		for( String key:callbackMap.keySet() ) {
			if(className.startsWith(key)) {
				callbacks = callbackMap.get(key);
			}
		}
		return callbacks;
	}
	
	/**
	 * Create a class with public members to hold attributes of a callback. 
	 * Note that the callback class exposes public members instead of methods.
	 * The callbacks are held in an indexed list, so there is no need to make them
	 * comparable..
	 */
	private class Callback {
		public final String type;
		public PyCode code;
		public long projectId;
		public String module;
		public String variable;
		public ScriptManager scriptManager;
		private int hashcode;
		
		public Callback(String type) {
			this.type = type;
			module = "";
			projectId = BlockConstants.UNKNOWN;
			scriptManager = null;
			variable = "";
			code = null;
			hashcode = (int)(System.nanoTime()%1000000000);
		}
	}
}
