package com.ils.blt.designer.search;

import java.util.Iterator;
import java.util.Map;

import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

// Search a GeneralPurposeDataContainer's list of maps.
public class NavAuxPropertySearchCursor extends SearchObjectCursor {
	private final String CLSS = "NavAuxPropertySearchCursor";
	private final DesignerContext context;
	Map<String,String> properties;
	private Iterator<String> iterator;
	private final String parentName;
	private final ProjectResourceId parentId;
	private final String nodeName;
	private final LoggerEx log;
	
	public NavAuxPropertySearchCursor(DesignerContext ctx,Map<String,String> props,ProjectResourceId parent,String node) {
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.properties = props;
		this.iterator = null;
		if( properties!=null ) {
			iterator = properties.keySet().iterator();
		}
		this.parentName = parent.getResourcePath().getName();;
		this.nodeName = node;
		this.parentId = parent;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if(properties!=null && properties.size()>0 ) {
			if( iterator.hasNext() ) {
				String name = iterator.next();
				so = new NavAuxPropertySearchObject(context,name,properties.get(name),parentId,nodeName);
				//log.infof("%s.next %s",CLSS,parentName,nodeName);
			}
		}
		return so;
	}
}
