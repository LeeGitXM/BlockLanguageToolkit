/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;

/**
* A family is a specialized process node.
*/
public class ProcessFamily extends ProcessNode {
	private static final long serialVersionUID = 6466659606436686263L;
	private ActiveState state = ActiveState.ACTIVE;

	/**
	 * Constructor: Create a family node from the NavTree structure of an diagram.
	 *
	 * @param name of the node
	 * @param parent UUID of the parent of this node.
	 * @param me UUID of this node 
	 */
	public ProcessFamily(String name,UUID parent,UUID self) { 
		super(name,parent,self);
	}
	
	/**
	 * Constructor: Create a Gateway object that encapsulates attributes of an Application.
	 * @param fam the serialized object that represents the family.
	 * @param parent 
	 */
	public ProcessFamily(SerializableFamily fam,UUID parent) { 
		super(fam.getName(),parent,fam.getId());
	}
	
	public ActiveState getState() {return state;}
	public void setState(ActiveState state) {this.state = state;}
	@Override
	public SerializableResourceDescriptor toResourceDescriptor() {
		SerializableResourceDescriptor descriptor = super.toResourceDescriptor();
		descriptor.setType(BLTProperties.FAMILY_RESOURCE_TYPE);
		return descriptor;
	}
}

