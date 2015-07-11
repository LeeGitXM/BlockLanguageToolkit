package com.ils.blt.designer.search;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
public class PropertySearchCursor extends SearchObjectCursor {
	private final String TAG = "PropertySearchCursor";
	private final LoggerEx log;
	private final DesignerContext context;
	private final ProcessBlockView block;
	private final BlockProperty property;
	private int index = 0;

	public PropertySearchCursor(DesignerContext ctx,ProcessBlockView blk,BlockProperty prop) {
		this.context = ctx;
		this.block = blk;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.property = prop;
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null; // SearchObject
		// Depending on the binding, return either the value or binding string
		//log.infof("%s.next %d %s:%s",TAG,index,block.getName(),property.getName());
		if( index==0 ) {
			so = new PropertySearchObject(context,block,property);
		}
		index++;
		return so;
	}

}

