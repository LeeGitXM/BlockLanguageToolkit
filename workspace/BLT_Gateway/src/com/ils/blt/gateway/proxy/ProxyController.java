/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.proxy;

import java.util.UUID;

import org.python.core.PyObject;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * A proxy controller is a stand-in for an output controller written in python. It's methods are all
 * delegated to the python layer, but callable from Java. It also holds the Python 
 * object to provide persistence. 
 *  
 */
public class ProxyController   {
	private static final String TAG = "ProxyController";
	private final String className;
	private PyObject pythonController= null;
	private final LoggerEx log;
	private final ProxyHandler delegate = ProxyHandler.getInstance();
	

	/**
	 * Constructor
	 * @param clss the Python module to instantiate
	 * @param proj ID of the project to which the diagram belongs
	 * @param diag ID of the diagram of which the block is a part
	 * @param block identifier
	 */
	public ProxyController(String clss,long proj,long diag,UUID block) {
		this.className = clss;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * @return the Python object for which this class is a proxy
	 */
	public PyObject getPythonController() {
		return pythonController;
	}
	public void setPythonController(Object obj) {
		if( obj==null ) {
			log.warnf("%s: setPythonController attempt to set null",TAG);
		}
		else if( obj instanceof PyObject ) {
			this.pythonController = (PyObject)obj; 
		}
		else {
			log.warnf("%s: setPythonController Unexpected object class %s, ignored",TAG,obj.getClass().getName());
		}
	}
	
	
}