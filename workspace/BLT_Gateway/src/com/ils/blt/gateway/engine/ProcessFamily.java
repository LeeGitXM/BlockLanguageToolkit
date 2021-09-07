/**
 *   (c) 2014-2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;

/**
* A family is a specialized process node.
*/
public class ProcessFamily extends ProcessNode {
	private static final long serialVersionUID = 6466659606436686263L;
	private DiagramState state = DiagramState.ACTIVE;

	/**
	 * Constructor: Create a family node from the NavTree structure of an diagram.
	 *
	 * @param name of the node
	 * @param parent UUID of the parent of this node.
	 * @param self UUID of this node 
	 */
	public ProcessFamily(String name,ResourcePath parent,ProjectResourceId self) { 
		super(name,parent,self);
	}
	
	/**
	 * Constructor: Create a Gateway object that encapsulates attributes of a Family.
	 * @param app the serialized object that represents the family.
	 * @param parent 
	 */
	public ProcessFamily(SerializableFamily fam,ResourcePath parent,String projectName) { 
		super(fam.getName(),parent,projectName,BLTProperties.FAMILY_RESOURCE_TYPE.getTypeId());
		setState(fam.getState());
	}
	
	public DiagramState getState() {return state;}
	public void setState(DiagramState state) {this.state = state;}
	@Override
	public SerializableResourceDescriptor toResourceDescriptor() {
		SerializableResourceDescriptor descriptor = super.toResourceDescriptor();
		descriptor.setType(BLTProperties.FAMILY_RESOURCE_TYPE.getTypeId());
		return descriptor;
	}
}

