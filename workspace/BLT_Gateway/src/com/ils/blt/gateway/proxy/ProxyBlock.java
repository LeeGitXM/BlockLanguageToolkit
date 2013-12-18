/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.proxy;

import java.util.Hashtable;
import java.util.Set;

import org.python.core.PyObject;

import com.ils.block.ProcessBlock;
import com.ils.block.common.PalettePrototype;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * A proxy block is a stand-in for a block written in python. It's methods are all
 * delegated to the pythn layer, but callable from Java. It also holds the Python 
 * object to provide persistence. 
 *  
 * The subclasses depend on the "ExecutableBlock" class annotation
 * as the signal to group a particular subclass into the list of 
 * available executable block types.
 */
public class ProxyBlock implements ProcessBlock {
	
	private final String className;
	private final long projectId;
	private final long diagramId;
	private final String blockId;
	private PyObject code = null;
	private final ProxyHandler delegate = ProxyHandler.getInstance();
	
	protected final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());

	
	/**
	 * Constructor
	 * @param clss the Python module to instantiate
	 * @param proj ID of the project to which the diagram belongs
	 * @param diag ID of the diagram of which the block is a part
	 * @param block number of the block (vertex) within the diagram
	 */
	public ProxyBlock(String clss,long proj,long diag,String block) {
		this.className = clss;
		this.projectId = proj;
		this.diagramId = diag;
		this.blockId = block;
	}

	/**
	 * Do it.
	 */
	@Override
	public void evaluate() {
		delegate.evaluate(projectId,diagramId,blockId);
	}
	/**
	 * @return the Python object for which this class is a proxy
	 */
	public PyObject getObject() {
		return code;
	}
	
	/**
	 * @param name the property (attribute) name.
	 * @return a particular property given its name.
	 */
	@Override
	public Hashtable<String,String> getProperty(String name) {
		//return delegate.getProperty(projectId,diagramId,blockId,name);
		return null;
	}
	
	@Override
	public long getProjectId() { return projectId; }
	@Override
	public long getDiagramId() { return diagramId; }
	@Override
	public String getBlockId() { return blockId; }
	/**
	 * @return all properties. These properties are modifiable. Each
	 *         property/attribute is a hashtable of Strings keyed by Strings.
	 */
	@Override
	public Hashtable<String,Hashtable<String,String>> getProperties() {
		return delegate.getProperties(getObject());
	}
	
	/**
	 * @return a list of the property names required by this class.
	 */
	@Override
	public Set<String> getPropertyNames() {
		Hashtable<String,Hashtable<String,String>> properties = getProperties();
		return properties.keySet();
	}
	
	/**
	 * Accept notification that a value has arrived on an input
	 * @param port name of the input port
	 * @param value to accept
	 */
	@Override
	public  void setValue(String name,QualifiedValue value) {
		delegate.setValue(projectId,diagramId,blockId,name,value);
	}

	@Override
	public PalettePrototype getBlockPrototype() {
		// TODO Auto-generated method stub
		return null;
	}
	

}