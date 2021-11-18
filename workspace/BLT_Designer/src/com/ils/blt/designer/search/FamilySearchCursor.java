package com.ils.blt.designer.search;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

// The only thing we search for with families is the name.
// We can get that from the resource directly.
public class FamilySearchCursor extends SearchObjectCursor {
	private final String CLSS = "FamilySearchCursor";
	private final DesignerContext context;
	private ProjectResource family; 
	private final LoggerEx log;
	private final long resId;
	private int index = 0;
	
	public FamilySearchCursor(DesignerContext ctx,long res) {
		this.context = ctx;
		this.resId = res;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.family = context.getProject().getResource(resId);
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		// Deserialize here - first time
		if( index==0 ) {
			ApplicationRequestHandler appHandler = new ApplicationRequestHandler();
			String appName = appHandler.getApplicationName(family.getParentUuid().toString());
			so = new FamilyNameSearchObject(context,appName,family.getName(),family.getParentUuid().toString());
			log.infof("%s.next %s",CLSS,family.getName());
		}
		else if(index==1 ) {
			GeneralPurposeDataContainer aux = GeneralPurposeTreeNode.deserializeFamily(family).getAuxiliaryData();
			if( aux!=null && aux.containsData() ) {
				ApplicationRequestHandler appHandler = new ApplicationRequestHandler();
				String appName = appHandler.getApplicationName(family.getParentUuid().toString());
				so = new NavAuxSearchCursor(context,aux,appName,family.getName(),family.getParentUuid().toString());
				log.infof("%s.next %s",CLSS,family.getName());
			}
		}
		index++;
		return so;
	}
}
