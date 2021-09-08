/**
 *   (c) 2013-2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.proxy;

import java.util.UUID;

import org.python.core.PyObject;

import com.ils.block.AbstractProcessBlock;
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.gateway.PythonRequestHandler;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;


/**
 * A proxy block is a stand-in for a block written in python. It's methods are all
 * delegated to the python layer, but callable from Java. It also holds the Python 
 * object to provide persistence. Nothing referenced in Python is permanent, nothing
 * is serialized.
 */
public class ProxyBlock extends AbstractProcessBlock  {
	private static final String TAG = "ProxyBlock";
	private String className;
	private PyObject pythonBlock = null;
	private final ProxyHandler delegate = ProxyHandler.getInstance();
	private final PythonRequestHandler requestHandler;
	private final GatewayContext context;
	

	/**
	 * Constructor
	 * @param clss the Python module to instantiate
	 * @param parent ID of the diagram of which the block is a part
	 * @param block identifier
	 */
	public ProxyBlock(GatewayContext ctx,String clss,ProjectResourceId parent,UUID block) {
		super(null,parent,block);
		this.context = ctx;
		this.className = clss;
		this.requestHandler = new PythonRequestHandler();
	}

	/**
	 * Add a new property. Do this as the block is first instantiated and
	 * we query python.
	 */
	public void addProperty(BlockProperty prop) {
		setProperty(prop.getName(),prop);
	}
	
	@Override
	public String getClassName() {
		log.debugf("%s.getClassName: className is %s",TAG,className);
		String ret = new String(className);  // changing className causes a deadlock
		if (ret.toLowerCase().startsWith("xom.block.")) {
			ret = "ils" + className.substring(3);
		}
		return ret; 
	}
	
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
	 * @param propName the property (attribute) name.
	 * @return a particular property given its name
	 */
	@Override
	public BlockProperty getProperty(String propName) {
		return super.getProperty(propName);
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
		return delegate.getBlockState(context.getProjectManager().getProjectScriptManager(getProjectName()), pythonBlock);
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
		delegate.notifyOfStatus(context.getProjectManager().getProjectScriptManager(getProjectName()),getPythonBlock());
	}
	/**
	 * Notify the python instance that it needs to propagate a value.
	 */
	@Override
	public void propagate() {
		delegate.propagate(context.getProjectManager().getProjectScriptManager(getProjectName()), pythonBlock);
	}
	/**
	 * Accept a new value for a block property. Push through to the
	 * Python layer. It is up to the block to determine whether or not
	 * this triggers block evaluation.
	 * @param name the name of one of the block's properties.
	 * @param obj the new value.
	 */
	@Override
	public synchronized void setProperty(String name,Object obj) {
		BlockProperty prop = getProperty(name);
		if( prop!=null ) {
			prop.setValue(obj);
			delegate.setBlockProperty(context.getProjectManager().getProjectScriptManager(getProjectName()),this,prop);
		}
	}
	/**
	 * Set the state of a block directly. Notify of alerting status.
	 * @param newState the new state.
	 */
	@Override
	public synchronized void setState(TruthValue newState) {
		super.setState(newState);   // Records activity
		delegate.setBlockState(context.getProjectManager().getProjectScriptManager(getProjectName()),this,newState);
		requestHandler.postAlertingStatus(this);
	}
	
	/**
	 * Notify the block that a new value has appeared on one of its input anchors. If the block has
	 * an embedded tag, it is possible that there is no port.
	 * @param vcn incoming notification
	 */
	@Override
	public synchronized void acceptValue(IncomingNotification vcn) {
		String port = null;
		if(vcn.getConnection()!=null  ) {
			port = vcn.getConnection().getDownstreamPortName();
		}
		recordActivity(Activity.ACTIVITY_RECEIVE,port,vcn.getValue().toString());
		delegate.acceptValue( context.getProjectManager().getProjectScriptManager(getProjectName()),getPythonBlock(),port,vcn.getValue());
	}
	
	/**
	 * In the case where the block has specified a coalescing time, this method
	 * will be called by the engine after receipt of input once the coalescing 
	 * "quiet" time has passed without further input.
	 */
	@Override
	public synchronized void evaluate() { 
		delegate.evaluate(context.getProjectManager().getProjectScriptManager(getProjectName()),getPythonBlock()); 
	}

	/**
	 * Return the custom internal data owned by the block.
	 */
	@Override
	public synchronized  void getAuxData(GeneralPurposeDataContainer container) { 
		delegate.getAuxData(context.getProjectManager().getProjectScriptManager(getProjectName()),getPythonBlock(),container);
		requestHandler.postAlertingStatus(this);
	}
	
	/**
	 * Start the block. 
	 * This is a handy place to do updates.
	 */
	@Override
	public synchronized void start() {
	}

	/**
	 * 
	 * Removed references to this because it causes a deadlock
	 * 
	 */
	public synchronized String removeXomFromClassname(String ret) {
		// This first section updates old blocks that start with "xom.block" to "ils.block" since that code was moved.
		if (ret.toLowerCase().startsWith("xom.block.")) {
			ret = "ils" + className.substring(3);
		}
		return ret;
	}
	
	/**
	 * Perform whatever is necessary prior to deleting the block.
	 */
	@Override
	public synchronized void onDelete() { 
		delegate.onDelete(context.getProjectManager().getProjectScriptManager(getProjectName()),getPythonBlock()); 
		requestHandler.postAlertingStatus(this);
	}
	/**
	 * Perform whatever is necessary prior to creating or saving the block.
	 */
	@Override
	public synchronized void onSave() { 
		delegate.onSave(context.getProjectManager().getProjectScriptManager(getProjectName()),getPythonBlock()); 
		requestHandler.postAlertingStatus(this);
	}
	/**
	 * Reset the block. Resetting  python block may change the diagram alert
	 * status.
	 */
	@Override
	public synchronized void reset() { 
		delegate.reset(context.getProjectManager().getProjectScriptManager(getProjectName()),getPythonBlock()); 
		requestHandler.postAlertingStatus(this);
		recordActivity(Activity.ACTIVITY_RESET,"");
	}
	/**
	 * Allow the block to write its custom auxiliary to an external source.
	 */
	@Override
	public synchronized void setAuxData(GeneralPurposeDataContainer container) { 
		delegate.setAuxData(context.getProjectManager().getProjectScriptManager(getProjectName()),getPythonBlock(),container); 
		setAuxiliaryData(container);
		requestHandler.postAlertingStatus(this);
	}
}