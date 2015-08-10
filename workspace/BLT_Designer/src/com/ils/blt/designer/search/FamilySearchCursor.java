package com.ils.blt.designer.search;

import com.ils.blt.common.ApplicationRequestHandler;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

// The only thing we search for with families is the name.
// We can get that from the resource directly.
public class FamilySearchCursor extends SearchObjectCursor {
	private final String TAG = "FamilySearchCursor";
	private final DesignerContext context;
	private ProjectResource family; 
	private final LoggerEx log;
	private final long resId;
	private int index = 0;
	
	public FamilySearchCursor(DesignerContext ctx,long res) {
		this.context = ctx;
		this.resId = res;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		// Deserialize here - first time through only - return next block cursor
		if( index==0 ) {
			family = context.getProject().getResource(resId);
			ApplicationRequestHandler appHandler = new ApplicationRequestHandler();
			String appName = appHandler.getApplicationName(family.getParentUuid().toString());
			so = new FamilyNameSearchObject(context,appName,family.getName());
			log.infof("%s.next %s",TAG,family.getName());
		}
		index++;
		return so;
	}
}
