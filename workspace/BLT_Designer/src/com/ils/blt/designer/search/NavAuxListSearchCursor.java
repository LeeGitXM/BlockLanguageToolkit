package com.ils.blt.designer.search;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

// Search a GeneralPurposeDataContainer's list of lists.
public class NavAuxListSearchCursor extends SearchObjectCursor {
	private final String CLSS = "NavAuxListSearchCursor";
	private final DesignerContext context;
	private Map<String,List<String>> lists;
	private Iterator<List<String>> iterator;
	private final String parentName;
	private final String parentId;
	private final String nodeName;
	private final LoggerEx log;
	
	public NavAuxListSearchCursor(DesignerContext ctx,Map<String,List<String>> stringLists,String parent,String node,String uuid) {
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.lists = stringLists;
		this.iterator = null;
		if( lists!=null ) {
			iterator = lists.values().iterator();
		}
		this.parentName = parent;
		this.nodeName = node;
		this.parentId = uuid;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if(lists!=null && lists.size()>0 ) {
		if( iterator.hasNext() ) {
			List<String> list = iterator.next();
			so = new NavAuxListSearchObject(context,list,parentName,nodeName,parentId);
			log.infof("%s.next %s",CLSS,parentName,nodeName);
		}
		}
		return so;
	}
}
