package com.ils.blt.designer.search;



import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Search the auxiliary data of a block. The auxx data has potentially 3 sections.
 *
 */
public class NavAuxSearchCursor extends SearchObjectCursor {
	private final String CLSS = "NavAuxSearchCursor";
	private final LoggerEx log;
	private final DesignerContext context;
	private final GeneralPurposeDataContainer aux;
	private final String parentName;
	private final ProjectResourceId parentId;
	private final String nodeName;
	private int index = 0;

	public NavAuxSearchCursor(DesignerContext ctx,GeneralPurposeDataContainer data,ProjectResourceId parent,String node) {
		this.context = ctx;
		this.aux = data;
		this.parentName = parent.getResourcePath().getName();
		this.parentId = parent;
		this.nodeName = node;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null; // SearchObject
		if( aux!=null && aux.containsData() ) {
			if( index==0 ) {
				so = new NavAuxPropertySearchCursor(context,aux.getProperties(),parentId,nodeName);
			}
			// Depending on the binding, return either the value or binding string
			//log.infof("%s.next %d %s:%s",TAG,index,block.getName(),property.getName());
			else if( index==1 ) {
				so = new NavAuxListSearchCursor(context,aux.getLists(),parentId,nodeName);
			}
			else if( index==2 ) {
				so = new NavAuxMapListSearchCursor(context,aux.getMapLists(),parentId,nodeName);
			}
		}
		index++;
		return so;
	}

}

