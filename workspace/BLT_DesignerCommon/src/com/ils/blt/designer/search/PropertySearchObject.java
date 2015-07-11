package com.ils.blt.designer.search;

import javax.swing.Icon;

import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.findreplace.SearchObject;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Return either the binding or the value string.
 * @author chuckc
 *
 */
public class PropertySearchObject implements SearchObject {
	
	private final DesignerContext context;
	private final ProcessBlockView block;
	private final BlockProperty property;
	
	public PropertySearchObject(DesignerContext ctx,ProcessBlockView parent,BlockProperty prop) {
		this.context = ctx;
		this.block = parent;
		this.property = prop;
	}
	@Override
	public Icon getIcon() {
		return null;
	}

	@Override
	public String getName() {
		return property.getName();
	}

	@Override
	public String getOwnerName() {
		return block.getName();
	}

	@Override
	public String getText() {
		BindingType type = property.getBindingType();
		if( (type==BindingType.TAG_MONITOR) ||
		    (type==BindingType.TAG_READ) ||
		    (type==BindingType.TAG_WRITE) ||
			(type==BindingType.TAG_READWRITE) ) {
			return property.getBinding();
		}
		else {
			return property.getValue().toString();
		}
	}

	@Override
	public void locate() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setText(String arg0) throws IllegalArgumentException {
		// TODO Auto-generated method stub
		
	}

}
