package com.ils.blt.designer.search;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

// Search a GeneralPurposeDataContainer's list of lists.
public class NavAuxListSearchCursor extends SearchObjectCursor {
	private final String CLSS = "NavAuxListSearchCursor";
	private final DesignerContext context;
	private Map<String,List<String>> lists;
	private Iterator<String> iterator;
	private final String parentName;
	private final ProjectResourceId parentId;
	private final String nodeName;
	private final LoggerEx log;
	
	public NavAuxListSearchCursor(DesignerContext ctx,Map<String,List<String>> stringLists,ProjectResourceId parent,String node) {
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.lists = stringLists;
		this.iterator = null;
		if( lists!=null ) {
			iterator = lists.keySet().iterator();
		}
		this.parentId = parent;
		this.parentName = parent.getResourcePath().getName();
		this.nodeName = node;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if(lists!=null && lists.size()>0 ) {
		if( iterator.hasNext() ) {
			String key = iterator.next();
			List<String> list = lists.get(key);
			so = new NavAuxListSearchObject(context,key,list,parentId,nodeName);
			log.infof("%s.next %s",CLSS,parentName,nodeName);
		}
		}
		return so;
	}
}
