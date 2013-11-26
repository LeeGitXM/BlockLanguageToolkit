/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.Hashtable;

import com.ils.block.BasicBlock;
import com.ils.jgx.common.JGXProperties;
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
	private final static String TAG = "PropertiesUpdateHandler";
	private final LoggerEx log;
	private GatewayContext context = null;
	private static PropertiesUpdateHandler instance = null;
	protected long projectId = 0;
	
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
	 * Create an instance of a named class. Place it into the instance repository
	 * keyed by the specified value. 
	 * @param key
	 * @param className
	 * @return the instance created, else null
	 */
	public BasicBlock createInstance(String key,String className) {
		log.debugf("%s: createInstance of %s, keyed by %s",TAG,className,key);   // Should be updated
		BasicBlock block = null;
		try {
			Class<?> clss = Class.forName(className);
			Object obj = clss.newInstance();
			if( obj instanceof BasicBlock ) {
				block = (BasicBlock)obj;
				// The block already has the class ... add the key.
				Hashtable<String,String> att = new Hashtable<String,String>();
				att.put(JGXProperties.ATTRIBUTE_VALUE, key);
				att.put(JGXProperties.ATTRIBUTE_EDITABLE, "False");
				block.getProperties().put(JGXProperties.NAME_KEY, att);
				ClassRepository repo = ClassRepository.getInstance();
				repo.put(key, block);
			}
			else {
				log.warnf("%s: createInstance: Class %s not a BasicBlock",TAG,className);
			}
		}
		catch( ClassNotFoundException cnf ) {
			log.warnf("%s: createInstance: Error creating %s (%s)",TAG,className,cnf.getMessage()); 
		}
		catch( InstantiationException ie ) {
			log.warnf("%s: createInstance: Error instantiating %s (%s)",TAG,className,ie.getLocalizedMessage()); 
		}
		catch( IllegalAccessException iae ) {
			log.warnf("%s: createInstance: Security exception creating %s (%s)",TAG,className,iae.getLocalizedMessage()); 
		}
		return block;
	}
	/**
	 * Extract 
	 * @param key
	 * @param attributes
	 * @return
	 */
	public Hashtable<String,Hashtable<String,String>> getAttributes(BasicBlock block,Hashtable<String,Hashtable<String,String>> attributes) {
		if( block==null) return attributes;

		Hashtable<String,Hashtable<String,String>> properties = block.getProperties();
		for( String key:attributes.keySet()) {
			if( !properties.containsKey(key) ) {
				properties.put(key, attributes.get(key));
			}
		}
		return properties;
	}
	
	public boolean instanceExists(String key) {
		ClassRepository repo = ClassRepository.getInstance();
		return repo.containsKey(key);
	}
	
	/**
	 * Query the block repository for a block specified by the key. If the block
	 * does not exist, create it.  If the block is already instantiated, then 
	 * return the actual attribute values (as opposed to defaults for a newly created block).
	 * 
	 * @param key
	 * @param attributes already known
	 * @return the attribute table appropriately enhanced.
	 */
	public Hashtable<String,Hashtable<String,String>> getBlockAttributes(String key,Hashtable<String,Hashtable<String,String>> attributes) {
		// If the instance doesn't exist, create one
		BasicBlock block = ClassRepository.getInstance().get(key);
		Hashtable<String,Hashtable<String,String>> results = null;
		if(block==null) {
			Hashtable<String,String> classAttribute = (Hashtable<String,String>)attributes.get(JGXProperties.NAME_CLASS);
			String className = classAttribute.get(JGXProperties.ATTRIBUTE_VALUE);
			if( className!=null ) {
				block = createInstance(key,className);
			}
			else {
				log.warnf(TAG+"getBlockAttributes: No class in supplied attributes ("+attributes+")");
			}
		}
		results = getAttributes(block,attributes);
		return results;
	}
	
	/**
	 * Query DiagramModel for classes connected at the beginning and end of the connection to obtain a list
	 * of permissible port names. If the connection instance already exists in the Gateway model,
	 * then return the actual port connections.
	 * 
	 * @param key
	 * @param attributes
	 * @return
	 */
	public Hashtable<String,Hashtable<String,String>> getConnectionAttributes(String key,Hashtable<String,Hashtable<String,String>> attributes) {
		return attributes;
	}
}
