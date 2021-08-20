package com.ils.blt.designer.search;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

// The only thing we search for with families is the name.
// We can get that from the resource directly.
public class FamilySearchCursor extends SearchObjectCursor {
	private final String TAG = "FamilySearchCursor";
	private final DesignerContext context;
	private ProjectResource family; 
	private final ILSLogger log;

	private int index = 0;
	
	public FamilySearchCursor(DesignerContext ctx,ProjectResource res) {
		this.context = ctx;
		this.family = res;
		this.log = LogMaker.getLogger(this);
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		// Deserialize here - first time through only - return next block cursor
		if( index==0 ) {
			ApplicationRequestHandler appHandler = new ApplicationRequestHandler();
			String appName = appHandler.getApplicationName(family.getParentUuid().toString());
			so = new FamilyNameSearchObject(context,appName,family.getName(),family.getParentUuid().toString());
			log.infof("%s.next %s",TAG,family.getName());
		}
		index++;
		return so;
	}
}
