/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.UUID;

import org.python.core.PyDictionary;
import org.python.core.PyList;
import org.python.core.PyObject;
import org.python.core.PyString;

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
import com.ils.blt.gateway.PythonRequestHandler;
import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
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
	private final PythonRequestHandler prh;
	// These are the indices of specific callback functions within the array
	private final Callback createBlockCallback;
	private final Callback evaluateCallback;
	private final Callback getBlockPropertiesCallback;
	private final Callback getBlockPrototypesCallback;
	private final Callback setBlockPropertyCallback;
	private final Callback setValueCallback;


	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private ProxyHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		toJavaTranslator = new PythonToJava();
		toPythonTranslator = new JavaToPython();
		prh = new PythonRequestHandler();
		// Create an instance of each callback method
		createBlockCallback = new CreateBlock();
		evaluateCallback = new Evaluate();
		getBlockPropertiesCallback = new GetBlockProperties();
		getBlockPrototypesCallback = new GetBlockPrototypes();
		setBlockPropertyCallback = new SetBlockProperty();
		setValueCallback = new SetValue();
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
	 * The gateway context must be specified before the instance is useful.
	 * @param cntx the GatewayContext
	 */
	public void setContext(GatewayContext cntx) {
		this.context = cntx;
		initializeCallbacks();
	}
	
	/**
	 * Once we have the context, we can initialize all of the callbacks. 
	 */
	private void initializeCallbacks() {
		createBlockCallback.setScriptManager(context.getScriptManager());
		evaluateCallback.setScriptManager(context.getScriptManager());
		getBlockPropertiesCallback.setScriptManager(context.getScriptManager());
		getBlockPrototypesCallback.setScriptManager(context.getScriptManager());
		setBlockPropertyCallback.setScriptManager(context.getScriptManager());
		setValueCallback.setScriptManager(context.getScriptManager());
	}
	

	public ProxyBlock createBlockInstance(String className,UUID parentId,UUID blockId) {
		ProxyBlock block = new ProxyBlock(className,parentId,blockId);
		log.infof("%s.createInstance --- calling",TAG); 
		if( createBlockCallback.compileScript() ) {
			PyDictionary pyDictionary = new PyDictionary();  // Empty
			createBlockCallback.setLocalVariable(0,new PyString(className));
			createBlockCallback.setLocalVariable(1,new PyString(parentId.toString()));
			createBlockCallback.setLocalVariable(2,new PyString(blockId.toString()));
			createBlockCallback.setLocalVariable(3,prh);
			createBlockCallback.setLocalVariable(4,pyDictionary);
			
			createBlockCallback.execute();
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
		if( evaluateCallback.compileScript() ) {
			evaluateCallback.setLocalVariable(0,block);
			evaluateCallback.execute();
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
		
		if( getBlockPropertiesCallback.compileScript() ) {
			Object val = null;
			UtilityFunctions fns = new UtilityFunctions();
			PyList pyList = new PyList();  // Empty
			getBlockPropertiesCallback.setLocalVariable(0,block);
			getBlockPropertiesCallback.setLocalVariable(1,pyList);
			getBlockPropertiesCallback.execute();
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
	
		if( getBlockPrototypesCallback.compileScript())  {
			Object val = null;
			UtilityFunctions fns = new UtilityFunctions();
			PyList pyList = new PyList();  // Empty
			List<?> list = null;
			getBlockPrototypesCallback.setLocalVariable(0,pyList);
			getBlockPrototypesCallback.execute();
			log.info(TAG+".getBlockPrototypes: returned "+ pyList);   // Should now be updated
			// Contents of list are Hashtable<String,?>
			list = toJavaTranslator.pyListToArrayList(pyList);

			for( Object obj:list ) { 
				try {
					if( obj instanceof Hashtable ) {
						@SuppressWarnings("unchecked")
						Hashtable<String,?> tbl = (Hashtable<String,?>)obj;
						log.info(TAG+".getPalettePrototypes first table "+ tbl);  
						PalettePrototype proto = new PalettePrototype();
						proto.setPaletteIconPath(nullCheck(tbl.get(BLTProperties.PALETTE_ICON_PATH),"Block/icons/embedded/transmitter.png"));
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
						val = tbl.get(BLTProperties.PALETTE_VIEW_BACKGROUND);
						if( val!=null ) {
							int background = 0xffffff; // White
							try {
								background = Long.decode(val.toString()).intValue();
							}
							catch(NumberFormatException nfe) {
								log.infof("%s.getPalettePrototypes: Illegal background specification: %s (%s) ",TAG,val.toString(),nfe.getLocalizedMessage());  
							}
							view.setBackground(background);
						}
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
								log.warnf("%s.getPalettePrototypes: Illegal block style parameter (%) (%s)" , TAG,val,iae.getMessage());
							}
							catch(Exception ex ) {
								log.warnf("%s.getPalettePrototypes: Illegal block style (%) (%s)" , TAG,val,ex.getMessage());
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
		if( setBlockPropertyCallback.compileScript() ) {
			PyDictionary pyDictionary = new PyDictionary();  // Empty
			setBlockPropertyCallback.setLocalVariable(0,block.getPythonBlock());
			setBlockPropertyCallback.setLocalVariable(1,pyDictionary);
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
			if(prop.getQuality()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_QUALITY,prop.getQuality());
			if( prop.getType()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_DATA_TYPE,prop.getType().toString());
			if( prop.getValue()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_VALUE,prop.getValue().toString());
			PyDictionary dict = toPythonTranslator.tableToPyDictionary(tbl);
			pyDictionary.__set__(new PyString("property"), dict);
			setBlockPropertyCallback.execute();
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
		if( setValueCallback.compileScript() ) {
			// There are 4 values to be specified - block,port,value,quality.
			setValueCallback.setLocalVariable(0,block);
			setValueCallback.setLocalVariable(1,new PyString(stub));
			setValueCallback.setLocalVariable(2,new PyString(value.getValue().toString()));
			setValueCallback.setLocalVariable(3,new PyString(value.getQuality().toString()));
			setValueCallback.execute();
		}
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
	
}
