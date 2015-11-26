/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.proxy;

import java.util.UUID;

import org.python.core.PyObject;

import com.ils.block.AbstractProcessBlock;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.inductiveautomation.ignition.common.script.ScriptManager;


/**
 * A proxy block is a stand-in for a block written in python. It's methods are all
 * delegated to the python layer, but callable from Java. It also holds the Python 
 * object to provide persistence. 
 *  
 */
public class ProxyBlock extends AbstractProcessBlock  {
	private static final String TAG = "ProxyBlock";
	private final String className;
	private PyObject pythonBlock = null;
	private final ProxyHandler delegate = ProxyHandler.getInstance();
	private final ScriptManager scriptManager;
	

	/**
	 * Constructor
	 * @param clss the Python module to instantiate
	 * @param proj ID of the project to which the diagram belongs
	 * @param diag ID of the diagram of which the block is a part
	 * @param block identifier
	 */
	public ProxyBlock(String clss,UUID parent,UUID block,ScriptManager mgr) {
		super(null,parent,block);
		this.className = clss;
		this.controller = BlockExecutionController.getInstance();
		this.scriptManager = mgr;
	}

	/**
	 * Add a new property. Do this as the block is first instantiated and
	 * we query python.
	 */
	public void addProperty(BlockProperty prop) {
		setProperty(prop.getName(),prop);
	}
	
	@Override
	public String getClassName() { return className; }
	
	/**
	 * @return the Python object for which this class is a proxy
	 */
	public PyObject getPythonBlock() {
		return pythonBlock;
	}
	public void setPythonBlock(Object obj) {
		if( obj==null ) {
			log.warnf("%s.setPythonBlock: attempt to set null",TAG);
		}
		else if( obj instanceof PyObject ) {
			this.pythonBlock = (PyObject)obj; 
		}
		else {
			log.warnf("%s.setPythonBlock: Unexpected object class %s, ignored",TAG,obj.getClass().getName());
		}
	}
	
	
	/**
	 * On a get, we return the locally cached property.
	 * @param name the property (attribute) name.
	 * @return a particular property given its name.
	 */
	@Override
	public BlockProperty getProperty(String name) {
		return super.getProperty(name);
	}
	
	/**
	 * Unimplemented. We assume this is never called. Instead
	 * the method to list all block prototypes calls this in
	 * the Python world.
	 */
	@Override
	public PalettePrototype getBlockPrototype() {
		return null;
	}
	/**
	 * Get the state from the python
	 */
	@Override
	public TruthValue getState() {
		return delegate.getBlockState(scriptManager, pythonBlock);
	}
	/**
	 * Send status update notifications for any properties
	 * or output connections known to the designer. 
	 * 
	 * In particular, this is called on startup to trigger
	 * status notifications.
	 */
	@Override
	public void notifyOfStatus() {
		delegate.notifyOfStatus(scriptManager,getPythonBlock());
	}
	/**
	 * Accept a new value for a block property. Push through to the
	 * Python layer. It is up to the block to determine whether or not
	 * this triggers block evaluation.
	 * @param name the name of one of the block's properties.
	 * @param obj the new value.
	 */
	@Override
	public void setProperty(String name,Object obj) {
		BlockProperty prop = getProperty(name);
		if( prop!=null ) {
			prop.setValue(obj);
			delegate.setBlockProperty(scriptManager,this,prop);
		}
	}

	
	/**
	 * Notify the block that a new value has appeared on one of its input anchors. If the block has
	 * an embedded tag, it is possible that there is no port.
	 * @param port name of the incoming anchor point
	 * @param value to accept. A qualified value has a timestamp, quality,
	 *        and simple value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		String port = null;
		if(vcn.getConnection()!=null  ) {
			port = vcn.getConnection().getDownstreamPortName();
		}
		delegate.acceptValue( scriptManager,getPythonBlock(),port,vcn.getValue());
	}
	
	/**
	 * In the case where the block has specified a coalescing time, this method
	 * will be called by the engine after receipt of input once the coalescing 
	 * "quiet" time has passed without further input.
	 */
	@Override
	public void evaluate() { 
		delegate.evaluate(scriptManager,getPythonBlock()); 
	}
	/**
	 * Reset the block.
	 */
	@Override
	public void reset() { 
		delegate.reset(scriptManager,getPythonBlock()); 
	}
}