package com.ils.blt.designer.search;

import com.ils.blt.common.ApplicationRequestHandler;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
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

	private int index = 0;
	
	public FamilySearchCursor(DesignerContext ctx,ProjectResource res) {
		this.context = ctx;
		this.family = res;
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if( index==0 ) {
			ApplicationRequestHandler appHandler = new ApplicationRequestHandler();
			String appName = appHandler.getApplicationName(family.getResourceId());
			so = new FamilyNameSearchObject(context,appName,family.getResourceName(),family.getResourceId());
			log.infof("%s.next %s",CLSS,family.getResourceName());
		}
		index++;
		return so;
	}
}
