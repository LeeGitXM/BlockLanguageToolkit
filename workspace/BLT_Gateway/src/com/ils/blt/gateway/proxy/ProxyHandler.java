/**
 *   (c) 2014-2017  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.python.core.PyDictionary;
import org.python.core.PyList;
import org.python.core.PyLong;
import org.python.core.PyObject;
import org.python.core.PyString;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
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
 *  This class is a singleton for easy access throughout the application. Methods are
 *  synchronized to insure that the script arguments are correctly grouped. 
 */
public class ProxyHandler   {
	private final static String TAG = "ProxyHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private final PythonToJava toJavaTranslator;
	private final JavaToPython toPythonTranslator;
	private static ProxyHandler instance = null;
	// These are the instances of specific callback functions
	private final Callback acceptValueCallback;
	private final Callback createBlockCallback;
	private final Callback evaluateCallback;
	private final Callback getBlockPropertiesCallback;
	private final Callback getBlockStateCallback;
	private final Callback getBlockPrototypesCallback;
	private final Callback notifyOfStatusCallback;
	private final Callback resetCallback;
	private final Callback setBlockPropertyCallback;
	private final Callback setBlockStateCallback;

	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	private ProxyHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		toJavaTranslator = new PythonToJava();
		toPythonTranslator = new JavaToPython();
		// Create an instance of each callback method
		acceptValueCallback = new AcceptValue();
		createBlockCallback = new CreateBlock();
		evaluateCallback = new Evaluate();
		getBlockPropertiesCallback = new GetBlockProperties();
		getBlockStateCallback = new GetBlockState();
		getBlockPrototypesCallback = new GetBlockPrototypes();
		notifyOfStatusCallback = new NotifyOfStatus();
		resetCallback = new Reset();
		setBlockPropertyCallback = new SetBlockProperty();
		setBlockStateCallback = new SetBlockState();
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
	}
	
	
	/**
	 * Called from the proxy into its Python implementation to inform the block that 
	 * it has a new value on one of its inputs. There is no shared dictionary.
	 * 
	 * @param mgr the appropriate project-specific script manager
	 * @param block
	 * @param stub the input port of the block on which the new value has arrived
	 * @param value one of a QualifiedValue, Signal, Truth-value or String
	 */
	public void acceptValue(ScriptManager mgr,PyObject block,String stub,QualifiedValue value) {
		
		if(block==null || stub==null || value==null || value.getValue()==null ) return;
		String qualityName = BLTProperties.QUALITY_GOOD;
		if(!value.getQuality().isGood() ) qualityName = value.getQuality().getName();
		log.debugf("%s.acceptValue --- %s %s (%s) on %s",TAG,block.toString(),value.getValue().toString(),qualityName,stub); 
		if( acceptValueCallback.compileScript() ) {
			synchronized(acceptValueCallback) {
			// There are 4 values to be specified - block,port,value,quality.
				acceptValueCallback.initializeLocalsMap(mgr);
				acceptValueCallback.setLocalVariable(0,block);
				acceptValueCallback.setLocalVariable(1,new PyString(stub));
				acceptValueCallback.setLocalVariable(2,new PyString(value.getValue().toString()));
				acceptValueCallback.setLocalVariable(3,new PyString(qualityName));
				acceptValueCallback.setLocalVariable(4,new PyLong(value.getTimestamp().getTime()));
				acceptValueCallback.execute(mgr);
			}
		}
	}

	public ProxyBlock createBlockInstance(String className,UUID parentId,UUID blockId,long projectId) {
		ProxyBlock block = new ProxyBlock(context,className,parentId,blockId);
		log.debugf("%s.createBlockInstance --- python proxy for %s, project %d",TAG,className,projectId); 
		if( createBlockCallback.compileScript() ) {
			synchronized(createBlockCallback) {
				PyDictionary pyDictionary = new PyDictionary();  // Empty
				createBlockCallback.initializeLocalsMap(context.getProjectManager().getProjectScriptManager(projectId));
				createBlockCallback.setLocalVariable(0,new PyString(className));
				createBlockCallback.setLocalVariable(1,new PyString(parentId.toString()));
				createBlockCallback.setLocalVariable(2,new PyString(blockId.toString()));
				createBlockCallback.setLocalVariable(3,pyDictionary);
				log.debugf("%s.createBlockInstance --- executing create script for %s",TAG,className); 
				createBlockCallback.execute(context.getProjectManager().getProjectScriptManager(projectId));

				// Contents of list are Hashtable<String,?>
				PyObject pyBlock = (PyObject)pyDictionary.get("instance");
				if( pyBlock!=null ) {
					block.setPythonBlock(pyBlock);
					BlockProperty[] props = getBlockProperties(context.getProjectManager().getProjectScriptManager(projectId),pyBlock);
					for(BlockProperty prop:props) {
						if(prop!=null) block.addProperty(prop);
					}
				}
				else {
					log.warnf("%s.createBlockInstance: Failed to create instance of %s",TAG,className);
					block = null;
				}
			}
		}
		else {
			log.warnf("%s.createBlockInstance --- failed to compile create script %s",TAG,className);
		}

		return block;
	}
	
	/**
	 * Tell the block to do whatever it is supposed to do. The block is the only
	 * argument passed.
	 *
	 * @param mgr the appropriate project-specific script manager
	 * @param block the saved Py block
	 */
	public synchronized void evaluate(ScriptManager mgr,PyObject block) {
		log.debugf("%s.evaluate --- %s",TAG,block.toString());
		if( evaluateCallback.compileScript() ) {
			evaluateCallback.initializeLocalsMap(mgr);
			evaluateCallback.setLocalVariable(0,block);
			evaluateCallback.execute(mgr);
		}
	}

	
	/**
	 * Query a Python block to obtain a list of its properties. The block is expected
	 * to exist.
	 * 
	 * @param mgr the appropriate project-specific script manager
	 * @param block the python block
	 * @return a new array of block properties.
	 */
	public synchronized BlockProperty[] getBlockProperties(ScriptManager mgr,PyObject block) {
		BlockProperty[] properties = null;
		log.debugf("%s.getBlockProperties ... ",TAG);
		if( getBlockPropertiesCallback.compileScript() ) {
			Object val = null;
			UtilityFunctions fns = new UtilityFunctions();
			PyList pyList = new PyList();  // Empty
			getBlockPropertiesCallback.initializeLocalsMap(mgr);
			getBlockPropertiesCallback.setLocalVariable(0,block);
			getBlockPropertiesCallback.setLocalVariable(1,pyList);
			getBlockPropertiesCallback.execute(mgr);
			log.debug(TAG+".getBlockProperties returned "+ pyList);   // Should now be updated
			// Contents of list are Map<String,?>
			List<?> list = toJavaTranslator.pyListToArrayList(pyList);
			
			int index = 0;
			properties = new BlockProperty[list.size()];
			for( Object obj:list ) { 
				try {
					if( obj instanceof Map ) {
						@SuppressWarnings("unchecked")
						Map<String,?> tbl = (Map<String,?>)obj;
						log.debug(TAG+".getBlockProperties property = "+ tbl);  
						BlockProperty prop = new BlockProperty();
						prop.setName(nullCheck(tbl.get(BLTProperties.BLOCK_ATTRIBUTE_NAME),"unnamed"));
						prop.setBinding(nullCheck(tbl.get(BLTProperties.BLOCK_ATTRIBUTE_BINDING),""));
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_BINDING_TYPE);
						if( val!=null) {
							try {
								prop.setBindingType(BindingType.valueOf(val.toString().toUpperCase()));
							}
							catch(IllegalArgumentException iae ) {
								log.warnf("%s.getBlockProperties: Illegal binding type (%s) (%s)" , TAG,val,iae.getMessage());
							}
							catch(Exception ex ) {
								log.warnf("%s.getBlockProperties: Illegal binding type (%s) (%s)" , TAG,val,ex.getMessage());
							}
						}
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_EDITABLE);
						if( val!=null) prop.setEditable(fns.coerceToBoolean(val));
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_DATA_TYPE);
						if( val!=null) {
							try {
								prop.setType(PropertyType.valueOf(val.toString().toUpperCase()));
							}
							catch(IllegalArgumentException iae ) {
								log.warnf("%s.getBlockProperties: Illegal data type (%s) (%s)" , TAG,val,iae.getMessage());
							}
							catch(Exception ex ) {
								log.warnf("%s.getBlockProperties: Illegal data type (%s) (%s)" , TAG,val,ex.getMessage());
							}
						}
						val = tbl.get(BLTProperties.BLOCK_ATTRIBUTE_VALUE);
						if( val!=null ) {
							prop.setValue(val);
						}
						properties[index] = prop;
					}
					
				}
				catch( Exception ex ) {
					log.warnf("%s.getBlockProperties: Exception processing prototype (%s)" , TAG,ex.getMessage());
				}
				index++;
			}
		}
		else {
			// Callback does not compile ...
			log.warnf("%s.getBlockProperties ... Error compiling script",TAG);
			properties = new BlockProperty[0];
		}

		return properties;
	}

	/**
	 * Query a Python block to get its current state. The block is expected
	 * to exist.
	 * 
	 * @param mgr the appropriate project-specific script manager
	 * @param block the python block
	 * @return a new array of block properties.
	 */
	public synchronized TruthValue getBlockState(ScriptManager mgr,PyObject block) {
		TruthValue state = TruthValue.UNSET;
		log.debugf("%s.getBlockState ... ",TAG);
		if( getBlockStateCallback.compileScript() ) {
			PyList pyList = new PyList();  // Empty
			getBlockStateCallback.initializeLocalsMap(mgr);
			getBlockStateCallback.setLocalVariable(0,block);
			getBlockStateCallback.setLocalVariable(1,pyList);
			getBlockStateCallback.execute(mgr);
			log.debug(TAG+".getBlockState returned "+ pyList);   // Should now be updated
			// Contents of list are Hashtable<String,?>
			// We're looking for a single string entry in the list
			List<?> list = toJavaTranslator.pyListToArrayList(pyList);
			
			for( Object obj:list ) { 
				try {
					state = TruthValue.valueOf(obj.toString().toUpperCase());	
				}
				catch( Exception ex ) {
					log.warnf("%s.getBlockState: Exception converting %s into a state (%s)" , TAG,obj.toString(),ex.getMessage());	
					state = TruthValue.UNKNOWN;
				}
			}
		}

		return state;
	}
	/**
	 * Query the python layer for a list of palette prototypes, one for
	 * each block definition. The prototypes are returned as a list of dictionaries
	 * and converted to PalettePrototype object here. We use the generic script manager.
	 * 
	 * @return
	 */
	public synchronized List<PalettePrototype> getPalettePrototypes() {
		List<PalettePrototype> prototypes = new ArrayList<PalettePrototype>();
		log.debugf("%s.getPalettePrototypes (python) ... ",TAG);
		if( getBlockPrototypesCallback.compileScript())  {
			Object val = null;
			UtilityFunctions fns = new UtilityFunctions();
			PyList pyList = new PyList();  // Empty
			List<?> list = null;
			getBlockPrototypesCallback.initializeLocalsMap(context.getScriptManager());
			getBlockPrototypesCallback.setLocalVariable(0,pyList);
			getBlockPrototypesCallback.execute(context.getScriptManager());
			log.debug(TAG+".getPalettePrototypes: returned "+ pyList);   // Should now be updated
			// Contents of list are Hashtable<String,?>
			list = toJavaTranslator.pyListToArrayList(pyList);

			for( Object obj:list ) { 
				try {
					if( obj instanceof Map ) {   // Note: Both Hashtable and HashMap implement Map
						@SuppressWarnings("unchecked")
						Map<String,?> tbl = (Map<String,?>)obj;
						log.debug(TAG+".getPalettePrototypes first table "+ tbl);  
						PalettePrototype proto = new PalettePrototype();
						proto.setPaletteIconPath(nullCheck(tbl.get(BLTProperties.PALETTE_ICON_PATH),"Block/icons/embedded/transmitter.png"));
						proto.setPaletteLabel(nullCheck(tbl.get(BLTProperties.PALETTE_LABEL),"From Python"));
						proto.setTooltipText(nullCheck(tbl.get(BLTProperties.PALETTE_TOOLTIP),""));
						proto.setTabName(nullCheck(tbl.get(BLTProperties.PALETTE_TAB_NAME),BlockConstants.PALETTE_TAB_CONTROL));
						
						// The table that we get from Python contains attributes for the BlockDescriptor
						// as well as the PalettePrototype
						BlockDescriptor desc = proto.getBlockDescriptor();
						val = tbl.get(BLTProperties.PALETTE_AUX_DATA);
						if( val!=null ) desc.setExternallyAugmented(fns.coerceToBoolean(val.toString()));
						val = tbl.get(BLTProperties.PALETTE_RECEIVE_ENABLED);
						if( val!=null ) desc.setReceiveEnabled(fns.coerceToBoolean(val.toString()));
						val = tbl.get(BLTProperties.PALETTE_TRANSMIT_ENABLED);
						if( val!=null ) desc.setTransmitEnabled(fns.coerceToBoolean(val.toString()));
						val = tbl.get(BLTProperties.PALETTE_VIEW_LABEL);
						if( val!=null ) desc.setEmbeddedLabel(val.toString());
						val = tbl.get(BLTProperties.PALETTE_VIEW_BACKGROUND);
						if( val!=null ) {
							int background = 0xffffff; // White
							try {
								background = Long.decode(val.toString()).intValue();
							}
							catch(NumberFormatException nfe) {
								log.infof("%s.getPalettePrototypes: Illegal background specification: %s (%s) ",TAG,val.toString(),nfe.getLocalizedMessage());  
							}
							desc.setBackground(background);
						}
						val = tbl.get(BLTProperties.PALETTE_VIEW_ICON);
						if( val!=null ) desc.setEmbeddedIcon(val.toString());
						val = tbl.get(BLTProperties.PALETTE_VIEW_BLOCK_ICON);
						if( val!=null ) desc.setIconPath(val.toString());
						val = tbl.get(BLTProperties.PALETTE_VIEW_FONT_SIZE);
						if( val!=null ) desc.setEmbeddedFontSize(fns.coerceToInteger(val));
						val = tbl.get(BLTProperties.PALETTE_VIEW_HEIGHT);
						if( val!=null ) desc.setPreferredHeight(fns.coerceToInteger(val));
						val = tbl.get(BLTProperties.PALETTE_VIEW_WIDTH);
						if( val!=null ) desc.setPreferredWidth(fns.coerceToInteger(val));
						desc.setBlockClass(nullCheck(tbl.get(BLTProperties.PALETTE_BLOCK_CLASS),"project.block.BasicBlock.BasicBlock"));
						val = tbl.get(BLTProperties.PALETTE_EDITOR_CLASS);
						if( val!=null ) desc.setEditorClass(val.toString());
						val = tbl.get(BLTProperties.PALETTE_BLOCK_STYLE);
						if( val!=null) {
							try {
								desc.setStyle(BlockStyle.valueOf(val.toString().toUpperCase()));
							}
							catch(IllegalArgumentException iae ) {
								log.warnf("%s.getPalettePrototypes: Illegal block style parameter (%s) (%s)" , TAG,val,iae.getMessage());
							}
							catch(Exception ex ) {
								log.warnf("%s.getPalettePrototypes: Illegal block style (%s) (%s)" , TAG,val,ex.getMessage());
							}
						}
						// Now handle the anchors
						val = tbl.get(BLTProperties.PALETTE_ANCHOR_IN);
						if( val!=null ) addAnchorsToDescriptor(desc,val,AnchorDirection.INCOMING);
						val = tbl.get(BLTProperties.PALETTE_ANCHOR_OUT);
						if( val!=null ) addAnchorsToDescriptor(desc,val,AnchorDirection.OUTGOING);
						prototypes.add(proto); 
					}
				}
				catch( Exception ex ) {
					log.warnf("%s: getPalettePrototypes: Exception processing prototype (%s)" ,TAG,ex.getMessage());
				}
			}
		}
		else {
			log.infof("%s: getPalettePrototypes: script compilation error (%s)",TAG,getBlockPropertiesCallback.module);
		}
		log.infof("%s: getPalettePrototypes returning %d protos from Python",TAG,prototypes.size()); 
		return prototypes;
	}
	/**
	 * Tell the block to issue status notifications. The block is the only
	 * argument passed.
	 *
	 * @param mgr the appropriate project-specific script manager
	 * @param block the saved Py block
	 */
	public synchronized void notifyOfStatus(ScriptManager mgr,PyObject block) {
		log.debugf("%s.notifyOfStatus --- %s",TAG,block.toString());
		if( notifyOfStatusCallback.compileScript() ) {
			notifyOfStatusCallback.initializeLocalsMap(mgr);
			notifyOfStatusCallback.setLocalVariable(0,block);
			notifyOfStatusCallback.execute(mgr);
		}
	}
	/**
	 * Tell the block to reset itself. The block is the only
	 * argument passed. Note that the python is responsible
	 * for "clear" notifications on its outputs
	 *
	 * @param mgr the appropriate project-specific script manager
	 * @param block the saved Py block
	 */
	public synchronized void reset(ScriptManager mgr,PyObject block) {
		log.debugf("%s.reset --- %s",TAG,block.toString());
		if( resetCallback.compileScript() ) {
			resetCallback.initializeLocalsMap(mgr);
			resetCallback.setLocalVariable(0,block);
			resetCallback.execute(mgr);
		}
	}
	public synchronized void setBlockProperty(ScriptManager mgr,ProxyBlock block,BlockProperty prop) {
		if( block==null || prop==null ) return;
		log.debugf("%s.setBlockProperty --- %s:%s",TAG,block.getClass(),prop.getName()); 
		if( setBlockPropertyCallback.compileScript() ) {
			// Convert the property object into a table to send to Python.
			if( prop.getName()==null ) {
				log.errorf("%s.setBlockProperty: Property name cannot be null",TAG); 
				return;
			}
			Map<String,Object> tbl = new HashMap<String,Object>();  
			tbl.put(BLTProperties.BLOCK_ATTRIBUTE_NAME,prop.getName());
			if(prop.getBinding()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_BINDING,prop.getBinding());
			if(prop.getBindingType()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_BINDING_TYPE,prop.getBindingType().toString());
			if(prop.isEditable())tbl.put(BLTProperties.BLOCK_ATTRIBUTE_EDITABLE,TruthValue.TRUE.toString());
			else tbl.put(BLTProperties.BLOCK_ATTRIBUTE_EDITABLE,TruthValue.FALSE.toString());
			if( prop.getType()!=null) tbl.put(BLTProperties.BLOCK_ATTRIBUTE_DATA_TYPE,prop.getType().toString());
			if( prop.getValue()!=null) {
				tbl.put(BLTProperties.BLOCK_ATTRIBUTE_VALUE,prop.getValue().toString());
			}
			PyDictionary pyDictionary = toPythonTranslator.tableToPyDictionary(tbl);
			setBlockPropertyCallback.initializeLocalsMap(mgr);
			setBlockPropertyCallback.setLocalVariable(0,block.getPythonBlock());
			setBlockPropertyCallback.setLocalVariable(1,pyDictionary);
			setBlockPropertyCallback.execute(mgr);
		}
	}
	
	public synchronized void setBlockState(ScriptManager mgr,ProxyBlock block,TruthValue newState) {
		if( block==null || newState==null ) return;
		log.debugf("%s.setBlockState --- %s:%s",TAG,block.getClass(),newState.name()); 
		if( setBlockStateCallback.compileScript() ) {
			setBlockStateCallback.initializeLocalsMap(mgr);
			setBlockStateCallback.setLocalVariable(0,block.getPythonBlock());
			setBlockStateCallback.setLocalVariable(1,new PyString(newState.name()));
			setBlockStateCallback.execute(mgr);
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
		log.debugf(TAG+": addAnchorsToPrototype "+l);
		if( l instanceof List ) {
			for( Object t: (List)l ) {
				if( t instanceof Map ) {
					Map tbl = (Map<String,?>)t;
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
