/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.proxy;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import org.python.core.PyObject;

import com.ils.block.ProcessBlock;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.BlockState;
import com.ils.block.common.PalettePrototype;
import com.ils.block.control.BlockPropertyChangeEvent;
import com.ils.block.control.IncomingNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * A proxy block is a stand-in for a block written in python. It's methods are all
 * delegated to the python layer, but callable from Java. It also holds the Python 
 * object to provide persistence. 
 *  
 */
public class ProxyBlock implements ProcessBlock {
	
	private final String className;
	private final long projectId;
	private final long diagramId;
	private final UUID blockId;
	private String label;
	private String statusText;
	private BlockState state;
	private PyObject code = null;
	private final ProxyHandler delegate = ProxyHandler.getInstance();
	
	protected final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());

	
	/**
	 * Constructor
	 * @param clss the Python module to instantiate
	 * @param proj ID of the project to which the diagram belongs
	 * @param diag ID of the diagram of which the block is a part
	 * @param block identifier
	 */
	public ProxyBlock(String clss,long proj,long diag,UUID block) {
		this.className = clss;
		this.projectId = proj;
		this.diagramId = diag;
		this.blockId = block;
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
	public BlockProperty getProperty(String name) {
		//return delegate.getProperty(projectId,diagramId,blockId,name);
		return null;
	}
	
	@Override
	public long getProjectId() { return projectId; }
	@Override
	public long getDiagramId() { return diagramId; }
	@Override
	public UUID getBlockId() { return blockId; }
	/**
	 * @return all properties. These properties are modifiable. Each
	 *         property/attribute is a hashtable of Strings keyed by Strings.
	 */
	@Override
	public BlockProperty[] getProperties() {
		return delegate.getProperties(getObject());
	}
	
	/**
	 * @return a list of the property names required by this class.
	 */
	@Override
	public Set<String> getPropertyNames() {
		BlockProperty[] properties = getProperties();
		Set<String>result = new HashSet<String>();
		for(BlockProperty bp:properties) {
			result.add(bp.getName());
		}
		return result;
	}
	


	@Override
	public PalettePrototype getBlockPrototype() {
		// TODO Auto-generated method stub
		return null;
	}
	
	/**
	 * Accept a new value for a block property. It is up to the block to
	 * determine whether or not this triggers block evaluation.
	 * @param property the new value of one of the block's properties.
	 */
	@Override
	public void setProperty(String name,QualifiedValue qv) {
		
	}
	/**
	 * Notify the block that a new value has appeared on one of its input anchors.
	 * @param port name of the incoming anchor point
	 * @param value to accept. A qualified value has a timestamp, quality,
	 *        and simple value.
	 */
	public void setValue(IncomingNotification vcn) {
		delegate.setValue(projectId, diagramId, blockId, vcn.getConnection().getUpstreamPortName(), vcn.getValue());
	}
	
	/**
	 * In the case where the block has specified a coalescing time, this method
	 * will be called by the engine after receipt of input once the coalescing 
	 * "quiet" time has passed without further input.
	 */
	public void evaluate() { delegate.evaluate(projectId, diagramId, blockId); }

	@Override
	public String getLabel() { return this.label; }
	@Override
	public void setLabel(String label) { this.label = label; }

	@Override
	public BlockState getState() { return this.state; }
	@Override
	public void setState(BlockState state) { this.state = state; }

	@Override
	public String getStatusText() { return this.statusText; }

	@Override
	public void setStatusText(String text) { this.statusText = text; }

	@Override
	public void propertyChange(BlockPropertyChangeEvent evt) {
		
	}

}