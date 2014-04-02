/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common.serializable;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;



/**
 *  This class provides the utility function of resetting the UUIDs
 *  everywhere in an Application hierarchy. This is required for a tree that is 
 *  cloned or imported.
 */
public class ApplicationUUIDResetHandler   {
	private final static String TAG = "ApplicationUUIDResetHandler";
	private final LoggerEx log;
	private final SerializableApplication application;
	
	/**
	 * Initialize with instances of the classes to be controlled.
	 */
	public ApplicationUUIDResetHandler(SerializableApplication sa) {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.application = sa;
	}

	/**
	 * Do it.
	 */
	public boolean convertUUIDs() {
		boolean success = false;
		
		return success;
	}
	
}
