package com.ils.blt.designer.search;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

// Search a GeneralPurposeDataContainer's list of lists.
public class BlockAuxListSearchCursor extends SearchObjectCursor {
	private final String CLSS = "BlockAuxListSearchCursor";
	private final DesignerContext context;
	private Map<String,List<String>> lists;
	private Iterator<List<String>> iterator;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final LoggerEx log;
	
	public BlockAuxListSearchCursor(DesignerContext ctx,Map<String,List<String>> stringLists,ProcessDiagramView dia,ProcessBlockView blk) {
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.lists = stringLists;
		this.iterator = null;
		if( lists!=null ) {
			iterator = lists.values().iterator();
		}
		this.diagram = dia;
		this.block = blk;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if(lists!=null && lists.size()>0 ) {
			if( iterator.hasNext() ) {
				List<String> list = iterator.next();
				so = new BlockAuxListSearchObject(context,list,diagram,block);
				log.infof("%s.next %s",CLSS,diagram.getName(),block.getName());
			}
		}
		return so;
	}
}
