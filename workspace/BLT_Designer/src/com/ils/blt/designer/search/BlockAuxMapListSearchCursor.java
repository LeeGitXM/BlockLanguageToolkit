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
public class BlockAuxMapListSearchCursor extends SearchObjectCursor {
	private final String CLSS = "BlockAuxMapListSearchCursor";
	private final DesignerContext context;
	private Map<String,List<Map<String,String>>> maplists;
	private Iterator<List<Map<String,String>>> iterator;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final LoggerEx log;
	
	public BlockAuxMapListSearchCursor(DesignerContext ctx,Map<String,List<Map<String,String>>> stringMapLists,ProcessDiagramView dia,ProcessBlockView blk) {
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.maplists = stringMapLists;
		this.iterator = null;
		if( maplists!=null ) {
			iterator = maplists.values().iterator();
		}
		this.diagram = dia;
		this.block = blk;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if(maplists!=null && maplists.size()>0 ) {
			if( iterator.hasNext() ) {
				List<Map<String,String>> list = iterator.next();
				so = new BlockAuxMapListSearchObject(context,list,diagram,block);
				log.infof("%s.next %s",CLSS,diagram.getName(),block.getName());
			}
		}
		return so;
	}
}
