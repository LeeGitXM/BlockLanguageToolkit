package com.ils.blt.designer.search;

import java.util.Iterator;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
public class BlockSearchCursor extends SearchObjectCursor {
	private final String TAG = "BlockSearchCursor";
	private final LoggerEx log;
	private final DesignerContext context;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private int index = 0;

	public BlockSearchCursor(DesignerContext ctx,ProcessDiagramView dia,ProcessBlockView blk) {
		this.context = ctx;
		this.diagram = dia;
		this.block = blk;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null;   // SearchObject
		// First time through only - scan the name
		if( index==0 ) {
			so = new BlockNameSearchObject(context,diagram,block);
			//log.infof("%s.next %s:%s",TAG,diagram.getName(),block.getName());
		}
		else {
			int jndex = 1;
			Iterator<BlockProperty> propertyWalker = block.getProperties().iterator();
			while( propertyWalker.hasNext() ) {
				Object temp = new PropertySearchCursor(context,block,(BlockProperty)(propertyWalker.next()));
				if( jndex==index ) {
					so = temp;
					break;
				}
				jndex++;
			}
		}
		index++;
		return so;
	}

}
