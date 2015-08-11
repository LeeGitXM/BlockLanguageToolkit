package com.ils.blt.designer.search;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
public class PropertySearchCursor extends SearchObjectCursor {
	private final String TAG = "PropertySearchCursor";
	private final LoggerEx log;
	private final DesignerContext context;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final BlockProperty property;
	private int index = 0;

	public PropertySearchCursor(DesignerContext ctx,ProcessDiagramView dia,ProcessBlockView blk,BlockProperty prop) {
		this.context = ctx;
		this.diagram = dia;
		this.block = blk;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.property = prop;
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null; // SearchObject
		if( index==0 ) {
			so = new PropertyNameSearchObject(context,diagram,block,property);
		}
		// Depending on the binding, return either the value or binding string
		//log.infof("%s.next %d %s:%s",TAG,index,block.getName(),property.getName());
		else if( index==1 ) {
			so = new PropertyValueSearchObject(context,diagram,block,property);
		}
		
		index++;
		return so;
	}

}

