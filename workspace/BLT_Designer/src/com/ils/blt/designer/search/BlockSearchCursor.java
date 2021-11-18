package com.ils.blt.designer.search;

import java.util.Iterator;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
public class BlockSearchCursor extends SearchObjectCursor {
	private final String CLSS = "BlockSearchCursor";
	private final LoggerEx log;
	private final DesignerContext context;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final boolean searchBlock;
	private final boolean searchProperties;
	private int index = 0;
	private Iterator<BlockProperty> propertyWalker;


	public BlockSearchCursor(DesignerContext ctx,ProcessDiagramView dia,ProcessBlockView blk,int key) {
		this.context = ctx;
		this.diagram = dia;
		this.block = blk;
		this.searchBlock = (key & BLTSearchProvider.SEARCH_BLOCK) != 0;
		this.searchProperties = (key & BLTSearchProvider.SEARCH_PROPERTY) != 0;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.index = 0;
		this.propertyWalker = null;
	}
	@Override
	public Object next() {
		Object so = null;   // SearchObject
		// First time through only - scan the name
		if( index==0 && searchBlock ) {
			so = new BlockNameSearchObject(context,diagram,block);
			//log.infof("%s.next %s:%s",TAG,diagram.getName(),block.getName());
		}
		else if( index==1 && searchBlock) {
			so = new BlockClassSearchObject(context,diagram,block);
			//log.infof("%s.next %s:%s",TAG,diagram.getName(),block.getName());
		}
		else if( searchProperties ) {
			GeneralPurposeDataContainer aux = block.getAuxiliaryData();
			if( propertyWalker == null) {
				so = new BlockAuxSearchCursor(context,aux,diagram,block);
				propertyWalker = block.getProperties().iterator();
			}
			else { 
				if( propertyWalker.hasNext() ) {
					so = new PropertySearchCursor(context,diagram,block,(BlockProperty)(propertyWalker.next()));	
				}
			}
		}
		index++;
		return so;
	}

}
