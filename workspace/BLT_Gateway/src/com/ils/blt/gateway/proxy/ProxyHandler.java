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
import org.python.core.PyList;
import org.python.core.PyObject;
import org.python.core.PyString;
import org.python.core.PyStringMap;

import com.ils.block.common.AnchorDirection;
import com.ils.block.common.AnchorPrototype;
import com.ils.block.common.BindingType;
import com.ils.block.common.BlockConstants;
import com.ils.block.common.BlockDescriptor;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.BlockStyle;
import com.ils.block.common.PalettePrototype;
import com.ils.block.common.PropertyType;
import com.ils.block.common.TruthValue;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.UtilityFunctions;
import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
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
	private final PythonToJava toJavaTranslator;
	private final JavaToPython toPythonTranslator;
	private static ProxyHandler instance = null;
	// These are the indices of specific callback functions within the array
	private static int CREATE_BLOCK_INSTANCE = 0;
	private static int EVALUATE = 1;
	private static int GET_CLASSES = 2;
	private static int GET_BLOCK_PROPERTIES = 3;
	private static int GET_BLOCK_PROTOTYPES = 4;
	private static int SET_BLOCK_PROPERTY = 5;
	private static int SET_VALUE = 6;
	private static int CALLBACK_COUNT = 7;
	
	// This is the array of callbacks ...
	Callback[] callbacks = new Callback[CALLBACK_COUNT];
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private ProxyHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		toJavaTranslator = new PythonToJava();
		toPythonTranslator = new JavaToPython();
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
	 * Set parameters for a callback. Practically speaking, there should be only one project that serves
	 * as a "library" of custom blocks.
	 * @param key a string denoting the callback kind. Valid values are found in BlockProperties.
	 * @param project name of the project that is the block code repository
	 * @param module the python code module which handles the callback. Must be in package "project" (hard-coded import).
	 * @param arglist name of the local variable, if any, supplied when the module is called.
	 */
	public void register(String key, String project, String module,String arg) {

		long projectId = -1;
		try {
			projectId = context.getProjectManager().getProjectId(project);
		}
		catch(Exception ex) {
			// Presumably we get this exception when the block project is changed (?)
			log.warnf("%s: register: Exception getting Id for project %s (%s)",TAG,project,ex.getMessage());
		}
		if( projectId<0) {
			log.warnf("%s: register: Unknown project (%s)",TAG,project);
			return;
		}
		int index = 0;
		
		if( key.equalsIgnoreCase(BLTProperties.CREATE_BLOCK_INSTANCE_CALLBACK)) {
			index = CREATE_BLOCK_INSTANCE;
		}
		else if( key.equalsIgnoreCase(BLTProperties.EVALUATE_CALLBACK)) {
			index = EVALUATE;
		}
		else if( key.equalsIgnoreCase(BLTProperties.GET_BLOCK_CLASSES_CALLBACK)) {
			index = GET_CLASSES;
		}
		else if( key.equalsIgnoreCase(BLTProperties.GET_BLOCK_PROPERTIES_CALLBACK)) {
			index = GET_BLOCK_PROPERTIES;
		}
		else if( key.equalsIgnoreCase(BLTProperties.GET_BLOCK_PROTOTYPES_CALLBACK)) {
			index = GET_BLOCK_PROTOTYPES;
		}
		else if( key.equalsIgnoreCase(BLTProperties.SET_BLOCK_PROPERTY_CALLBACK)) {
			index = SET_BLOCK_PROPERTY;
		}
		else if( key.equalsIgnoreCase(BLTProperties.SET_VALUE_CALLBACK)) {
			index = SET_VALUE;
		}
		else {
			log.warnf("%s: register: unknown registration method (%s)",TAG,key);
			return;
		}
		// Now that we have the callback type, add to the array
		Callback callback = callbacks[index];
		if( callback == null ) {
			callback = new Callback(key);
			callbacks[index] = callback;	
		}
	
		callback.setScriptManager(context.getProjectManager().getProjectScriptManager(projectId));
		callback.setModule(module);
		callback.setLocalVariableList((arg==null?"":arg));
	}
	
	/**
	 * Remove callback capabilities.
	 */
	public void deregister(String key) {
		int index = 0;
		
		if( key.equalsIgnoreCase(BLTProperties.CREATE_BLOCK_INSTANCE_CALLBACK)) {
			index = CREATE_BLOCK_INSTANCE;
		}
		else if( key.equalsIgnoreCase(BLTProperties.EVALUATE_CALLBACK)) {
			index = EVALUATE;
		}
		else if( key.equalsIgnoreCase(BLTProperties.GET_BLOCK_CLASSES_CALLBACK)) {
			index = GET_CLASSES;
		}
		else if( key.equalsIgnoreCase(BLTProperties.GET_BLOCK_PROPERTIES_CALLBACK)) {
			index = GET_BLOCK_PROPERTIES;
		}
		else if( key.equalsIgnoreCase(BLTProperties.GET_BLOCK_PROTOTYPES_CALLBACK)) {
			index = GET_BLOCK_PROTOTYPES;
		}
		else if( key.equalsIgnoreCase(BLTProperties.SET_BLOCK_PROPERTY_CALLBACK)) {
			index = SET_BLOCK_PROPERTY;
		}
		else if( key.equalsIgnoreCase(BLTProperties.SET_VALUE_CALLBACK)) {
			index = SET_VALUE;
		}
		else {
			log.warnf("%s: register: unknown registration method (%s)",TAG,key);
			return;
		}
		// Now that we have the callback type, add to the array
		callbacks[index] = null;
	}
	
	
	/**
	 * The gateway context must be specified before the instance is useful.
	 * @param cntx the GatewayContext
	 */
	public void setContext(GatewayContext cntx) {
		this.context = cntx;
	}
	
	public ProxyBlock createBlockInstance(String className,UUID parentId,UUID blockId) {
		ProxyBlock block = new ProxyBlock(className,parentId,blockId);
		log.infof("%s.createInstance --- calling",TAG); 
		if( callbacks[CREATE_BLOCK_INSTANCE]!=null && compileScript(CREATE_BLOCK_INSTANCE) ) {
			Callback cb = callbacks[CREATE_BLOCK_INSTANCE];
			PyDictionary pyDictionary = new PyDictionary();  // Empty
			cb.setLocalVariable(0,new PyString(className));
			cb.setLocalVariable(1,new PyString(parentId.toString()));
			cb.setLocalVariable(2,new PyString(blockId.toString()));
			cb.setLocalVariable(3,pyDictionary);
			
			execute(cb);
			log.info(TAG+".createInstance: returned "+ pyDictionary);   // Should now be updated
			// Contents of list are Hashtable<String,?>
			PyObject pyBlock = (PyObject)pyDictionary.get("instance");
			if( pyBlock!=null ) {
				block.setPythonBlock(pyBlock);
				BlockProperty[] props = getBlockProperties(pyBlock);
				for(BlockProperty prop:props) {
					block.addProperty(prop);
				}
			}
			else {
				log.warnf("%s.createInstance: Failed to create instance of %s",TAG,className);
				block = null;
			}
		}
		return block;
	}
	
	/**
	 * Tell the block to do whatever it is supposed to do. The block is the only
	 * argument passed.
	 *
	 * @param block the saved Py block
	 */
	public void evaluate(PyObject block) {
		log.infof("%s.evaluate --- %s",TAG,block.toString());
		if( callbacks[EVALUATE]!=null && compileScript(EVALUATE) ) {
			Callback cb = callbacks[EVALUATE];
			cb.setLocalVariable(0,block);
			execute(cb);
		}
	}
	
	/**
	 * Query a Python block to obtain a list of its properties. The block is expected
	 * to exist.
	 * 
	 * @param block the python block
	 * @return an array of block properties.
	 */
	public BlockProperty[] getBlockProperties(PyObject block) {
		BlockProperty[] properties = null;
		
		if( callbacks[GET_BLOCK_PROPERTIES]!=null && compileScript(GET_BLOCK_PROPERTIES) ) {
			Object val = null;
			UtilityFunctions fns = new UtilityFunctions();
			Callback cb = callbacks[GET_BLOCK_PROPERTIES];
			PyList pyList = new PyList();  // Empty
			cb.setLocalVariable(0,block);
			cb.setLocalVariable(1,pyList);
			execute(cb);
			log.info(TAG+".getBlockProperties returned "+ pyList);   // Should now be updated
			// Contents of list are Hashtable<String,?>
			List<?> list = toJavaTranslator.pyListToArrayList(pyList);
			
			int index = 0;
			properties = new BlockProperty[list.size()];
			for( Object obj:list ) { 
				try {
					if( obj instanceof Hashtable ) {
						@SuppressWarnings("unchecked")
						Hashtable<String,?> tbl = (Hashtable<String,?>)obj;
						log.info(TAG+": getProperties property = "+ tbl);  
						BlockProperty prop = new BlockProperty();
						prop.setName(nullCheck(tbl.get(BLTProperties.BLOCK_ATTRIBUTE_NAME),"unnamed"));
						prop.setBinding(nullCheck(tbl.get(BLTProperties.BLOCK_ATTRIBUTE_BINDING),""));
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_BINDING_TYPE);
						if( val!=null) {
							try {
								prop.setBindingType(BindingType.valueOf(val.toString().toUpperCase()));
							}
							catch(IllegalArgumentException iae ) {
								log.warnf("%s: getProperties: Illegal binding type (%) (%s)" , TAG,val,iae.getMessage());
							}
							catch(Exception ex ) {
								log.warnf("%s: getProperties: Illegal binding type (%) (%s)" , TAG,val,ex.getMessage());
							}
						}
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_EDITABLE);
						if( val!=null) prop.setEditable(fns.coerceToBoolean(val));
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_MAX);
						if( val!=null) prop.setMaximum(fns.coerceToDouble(val));
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_MIN	);
						if( val!=null) prop.setMinimum(fns.coerceToDouble(val));
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_QUALITY);
						prop.setQuality(nullCheck(tbl.get(BLTProperties.BLOCK_ATTRIBUTE_NAME),"good"));
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_DATA_TYPE);
						if( val!=null) {
							try {
								prop.setType(PropertyType.valueOf(val.toString().toUpperCase()));
							}
							catch(IllegalArgumentException iae ) {
								log.warnf("%s: getProperties: Illegal data type (%) (%s)" , TAG,val,iae.getMessage());
							}
							catch(Exception ex ) {
								log.warnf("%s: getProperties: Illegal data type (%) (%s)" , TAG,val,ex.getMessage());
							}
						}
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_VALUE);
						if( val!=null ) prop.setValue(val);
						properties[index] = prop;
					}
					
				}
				catch( Exception ex ) {
					log.warnf("%s: getProperties: Exception processing prototype (%)" , TAG,ex.getMessage());
				}
				index++;
			}
		}
		else {
			// Callback does not compile ...
			properties = new BlockProperty[0];
		}

		return properties;
	}

	/**
	 * Query the python layer for a list of palette prototypes, one for
	 * each block definition. The prototypes are returned as a list of dictionaries
	 * and converted to PalettePrototype object here.
	 * @return
	 */
	public List<PalettePrototype> getPalettePrototypes() {
		List<PalettePrototype> prototypes = new ArrayList<PalettePrototype>();
	
		if( callbacks[GET_BLOCK_PROTOTYPES]!=null && compileScript(GET_BLOCK_PROTOTYPES) ) {
			Object val = null;
			UtilityFunctions fns = new UtilityFunctions();
			Callback cb = callbacks[GET_BLOCK_PROTOTYPES];
			PyList pyList = new PyList();  // Empty
			List<?> list = null;
			cb.setLocalVariable(0,pyList);
			execute(cb);
			log.info(TAG+".getBlockPrototypes: returned "+ pyList);   // Should now be updated
			// Contents of list are Hashtable<String,?>
			list = toJavaTranslator.pyListToArrayList(pyList);

			for( Object obj:list ) { 
				try {
					if( obj instanceof Hashtable ) {
						@SuppressWarnings("unchecked")
						Hashtable<String,?> tbl = (Hashtable<String,?>)obj;
						log.info(TAG+": getPalettePrototypes first table "+ tbl);  
						PalettePrototype proto = new PalettePrototype();
						proto.setPaletteIconPath(nullCheck(tbl.get(BLTProperties.PALETTE_ICON_PATH),"Block/icons/large/transmitter.png"));
						proto.setPaletteLabel(nullCheck(tbl.get(BLTProperties.PALETTE_LABEL),"From Python"));
						proto.setTooltipText(nullCheck(tbl.get(BLTProperties.PALETTE_TOOLTIP),""));
						proto.setTabName(nullCheck(tbl.get(BLTProperties.PALETTE_TAB_NAME),BlockConstants.PALETTE_TAB_CONTROL));
						
						BlockDescriptor view = proto.getBlockDescriptor();
						val = tbl.get(BLTProperties.PALETTE_RECEIVE_ENABLED);
						if( val!=null ) view.setReceiveEnabled(fns.coerceToBoolean(val.toString()));
						val = tbl.get(BLTProperties.PALETTE_TRANSMIT_ENABLED);
						if( val!=null ) view.setTransmitEnabled(fns.coerceToBoolean(val.toString()));
						val = tbl.get(BLTProperties.PALETTE_VIEW_LABEL);
						if( val!=null ) view.setEmbeddedLabel(val.toString());
						val = tbl.get(BLTProperties.PALETTE_VIEW_ICON);
						if( val!=null ) view.setEmbeddedIcon(val.toString());
						val = tbl.get(BLTProperties.PALETTE_VIEW_BLOCK_ICON);
						if( val!=null ) view.setIconPath(val.toString());
						val = tbl.get(BLTProperties.PALETTE_VIEW_HEIGHT);
						if( val!=null ) view.setPreferredHeight(fns.coerceToInteger(val));
						val = tbl.get(BLTProperties.PALETTE_VIEW_WIDTH);
						if( val!=null ) view.setPreferredWidth(fns.coerceToInteger(val));
						view.setBlockClass(nullCheck(tbl.get(BLTProperties.PALETTE_BLOCK_CLASS),"project.block.BasicBlock.BasicBlock"));
						val = tbl.get(BLTProperties.PALETTE_BLOCK_STYLE);
						if( val!=null) {
							try {
								view.setStyle(BlockStyle.valueOf(val.toString().toUpperCase()));
							}
							catch(IllegalArgumentException iae ) {
								log.warnf("%s: getPalettePrototypes: Illegal block style (%) (%s)" , TAG,val,iae.getMessage());
							}
							catch(Exception ex ) {
								log.warnf("%s: getPalettePrototypes: Illegal block style (%) (%s)" , TAG,val,ex.getMessage());
							}
						}
						// Now handle the anchors
						val = tbl.get(BLTProperties.PALETTE_ANCHOR_IN);
						if( val!=null ) addAnchorsToDescriptor(view,val,AnchorDirection.INCOMING);
						val = tbl.get(BLTProperties.PALETTE_ANCHOR_OUT);
						if( val!=null ) addAnchorsToDescriptor(view,val,AnchorDirection.OUTGOING);
						prototypes.add(proto); 
					}
				}
				catch( Exception ex ) {
					log.warnf("%s: getPalettePrototypes: Exception processing prototype (%)" , TAG,ex.getMessage());
				}
			}
		}
		log.infof("%s: getPalettePrototypes returning %d protos from Python",TAG,prototypes.size()); 
		return prototypes;
	}
	
	public void setBlockProperty(ProxyBlock block,BlockProperty prop) {
		if( block==null || prop==null ) return;
		log.infof("%s.setProperty --- %s:%s",TAG,block.getClass(),prop.getName()); 
		if( callbacks[SET_BLOCK_PROPERTY]!=null && compileScript(SET_BLOCK_PROPERTY) ) {
			Callback cb = callbacks[SET_BLOCK_PROPERTY];
			PyDictionary pyDictionary = new PyDictionary();  // Empty
			cb.setLocalVariable(0,block.getPythonBlock());
			cb.setLocalVariable(1,pyDictionary);
			// Convert the property object into a table to send to Python.
			if( prop.getName()==null ) {
				log.errorf("%s.setProperty: Property name cannot be null",TAG); 
				return;
			}
			Hashtable<String,Object> tbl = new Hashtable<String,Object>();  
			tbl.put(BLTProperties.BLOCK_ATTRIBUTE_NAME,prop.getName());
			if(prop.getBinding()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_BINDING,prop.getBinding());
			if(prop.getBindingType()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_BINDING_TYPE,prop.getBindingType().toString());
			if(prop.isEditable())tbl.put(BLTProperties.BLOCK_ATTRIBUTE_EDITABLE,TruthValue.TRUE.toString());
			else tbl.put(BLTProperties.BLOCK_ATTRIBUTE_EDITABLE,TruthValue.FALSE.toString());
			tbl.put(BLTProperties.BLOCK_ATTRIBUTE_MAX, String.valueOf(prop.getMaximum()));
			tbl.put(BLTProperties.BLOCK_ATTRIBUTE_MIN, String.valueOf(prop.getMinimum()));
			if(prop.getQuality()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_QUALITY,prop.getQuality());
			if( prop.getType()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_DATA_TYPE,prop.getType().toString());
			if( prop.getValue()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_VALUE,prop.getValue().toString());
			PyDictionary dict = toPythonTranslator.tableToPyDictionary(tbl);
			pyDictionary.__set__(new PyString("property"), dict);
			execute(cb);
		}
	}
	
	/**
	 * Inform the block that it has a new value on one of its inputs. There is no shared dictionary.
	 * 
	 * @param block
	 * @param stub
	 * @param value one of a QualifiedValue, Signal, Truth-value or String
	 */
	public void setValue(PyObject block,String stub,QualifiedValue value) {
		if(block==null || value==null || value.getValue()==null ) return;
		log.infof("%s.setValue --- %s %s on %s",TAG,block.toString(),value.getValue().toString(),stub); 
		if( callbacks[SET_VALUE]!=null && compileScript(SET_VALUE) ) {
			Callback cb = callbacks[SET_VALUE];
			// There are 4 values to be specified - block,port,value,quality.
			cb.setLocalVariable(0,block);
			cb.setLocalVariable(1,new PyString(stub));
			cb.setLocalVariable(2,new PyString(value.getValue().toString()));
			cb.setLocalVariable(3,new PyString(value.getQuality().toString()));
			execute(cb);
		}
	}
	
	//================================ Unimplemented/Tested ==================================


	/**
	 * Compile a simple script that does nothing but call the specified script. 
	 * We assume that the script name includes a module, plus script name, plus method.
	 * We want the module for our import statement.
	 * @param callbackIndex index of the script we wish to execute.
	 * @return true if the script compiled
	 */
	private boolean compileScript(int callbackIndex) {
		Callback callback = callbacks[callbackIndex];
		if( callback.code !=null ) return true;   // Already compiled
		String module = "project";                
		int index = callback.module.lastIndexOf(".");
		if( index>0 ) module = callback.module.substring(0,index);
		index = module.lastIndexOf(".");
		if( index>0 ) module = module.substring(0,index);
		String script = String.format("import %s;%s(%s)",module,callback.module,callback.getLocalVariableList());
		log.infof("%s: Compiling callback script \n%s",TAG,script);
		try {
			callback.code = Py.compile_flags(script,module,CompileMode.exec,CompilerFlags.getCompilerFlags());
	         
	     }
		catch(Exception ex) {
			log.errorf("%s: Failed to compile script \n%s",TAG,script);
		}
		return callback.code!=null;
	}
	
	/**
	 *  Run the call-back script in line. On completion return the contents of the shared variable.
	 */
	public void execute(Callback callback) {
		log.tracef("%s Running callback script ...(%s)",TAG,callback.module);
		try {
			callback.getScriptManager().runCode(callback.code,callback.getLocalsMap());
		}
		catch( JythonExecException jee) {
			log.error(TAG+": JythonException executing python "+callback.module+ " ("+jee.getMessage()+")",jee);
		}
		catch(Exception ex) {
			log.error(TAG+": Error executing python "+callback.module+ " ("+ex.getMessage()+")",ex);
		}
		log.tracef("%s: Completed callback script.",TAG);
	}
	//========================================= Helper Methods ============================================
	/**
	 * Create a list of AnchorPrototypes and add to the PalettePrototype
	 * @param bd
	 * @param l a list, hopefully
	 * @param direction
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void addAnchorsToDescriptor(BlockDescriptor bd,Object l,AnchorDirection direction) {
		log.info(TAG+": addAnchorsToPrototype "+l);
		if( l instanceof List ) {
			for( Object t: (List)l ) {
				if( t instanceof Hashtable ) {
					Hashtable tbl = (Hashtable<String,?>)t;
					Object name = tbl.get("name");
					Object type = tbl.get("type");
					if( name!=null && type!=null ) {
						try {
							AnchorPrototype ap = new AnchorPrototype();
							ap.setName(name.toString());
							ap.setConnectionType(ConnectionType.valueOf(type.toString().toUpperCase()));
							ap.setAnchorDirection(direction);
							bd.addAnchor(ap);
						}
						catch(IllegalArgumentException iae) {
							log.warnf("%s: addAnchorsToPrototype: Illegal connection type %s (%s)",TAG,type,iae.getMessage());
						}
					}
							
				}
			}
		}
	}
	/**
	 * @param obj
	 * @param def
	 * @return either the object converted to a string, or, if null, the default
	 */
	private String nullCheck(Object obj,String def) {
		log.trace(TAG+": nullCheck "+obj);
		if( obj!=null ) return obj.toString();
		else return def;
	}
	
	/**
	 * Create a class with public members to hold attributes of a callback. 
	 * Note that the callback class exposes public members instead of methods.
	 * The callbacks are held in an indexed list, so there is no need to make them
	 * comparable..
	 */
	private class Callback {
		public PyCode code;
		public String module;
		private String[] localVariables;      // Derived from comma-separated
		private String localVariableList;
		private ScriptManager scriptManager = null;
		private PyStringMap localsMap = null;

		
		public Callback(String type) {
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
		 * @param value the single local argument
		 */
		public void setModule(String mod) {
			if( mod==null) return;
			module = mod;
			int index = mod.lastIndexOf("(");
			if( index>0 ) module = mod.substring(0,index);
		}
		public PyStringMap getLocalsMap() { return this.localsMap; }
	}
}
