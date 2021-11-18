package com.ils.blt.designer.search;



import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Search the auxiliary data of a block. The auxx data has potentially 3 sections.
 *
 */
public class BlockAuxSearchCursor extends SearchObjectCursor {
	private final String CLSS = "BlockAuxSearchCursor";
	private final LoggerEx log;
	private final DesignerContext context;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private int index = 0;

	public BlockAuxSearchCursor(DesignerContext ctx,GeneralPurposeDataContainer aux,ProcessDiagramView dia,ProcessBlockView blk) {
		this.context = ctx;
		this.diagram = dia;
		this.block = blk;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null; // SearchObject
		GeneralPurposeDataContainer aux = block.getAuxiliaryData();
		if( aux!=null && aux.containsData() ) {
			if( index==0 ) {
				so = new BlockAuxPropertySearchCursor(context,aux.getProperties(),diagram,block);
			}
			// Depending on the binding, return either the value or binding string
			//log.infof("%s.next %d %s:%s",TAG,index,block.getName(),property.getName());
			else if( index==1 ) {
				so = new BlockAuxListSearchCursor(context,aux.getLists(),diagram,block);
			}
			else if( index==2 ) {
				so = new BlockAuxMapListSearchCursor(context,aux.getMapLists(),diagram,block);
			}
		}
		index++;
		return so;
	}

}

