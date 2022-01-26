package com.ils.blt.designer.search;

import java.util.Iterator;
import java.util.Map;

import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

// Search a GeneralPurposeDataContainer's list of maps.
public class BlockAuxPropertySearchCursor extends SearchObjectCursor {
	private final String CLSS = "BlockAuxPropertySearchCursor";
	private final DesignerContext context;
	Map<String,String> properties;
	private Iterator<String> iterator;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final LoggerEx log;
	
	public BlockAuxPropertySearchCursor(DesignerContext ctx,Map<String,String> props,ProcessDiagramView dia,ProcessBlockView blk) {
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.properties = props;
		this.iterator = null;
		if( properties!=null ) {
			iterator = properties.keySet().iterator();
		}
		this.diagram = dia;
		this.block = blk;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if(properties!=null && properties.size()>0 ) {
			if( iterator.hasNext() ) {
				String name = iterator.next();
				so = new BlockAuxPropertySearchObject(context,name,properties.get(name),diagram,block);
				//log.infof("%s.next %s",CLSS,parentName,nodeName);
			}
		}
		return so;
	}
}
