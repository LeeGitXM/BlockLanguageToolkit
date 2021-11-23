package com.ils.blt.designer.search;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

// Search a GeneralPurposeDataContainer's list of lists.
public class NavAuxMapListSearchCursor extends SearchObjectCursor {
	private final String CLSS = "NavAuxMapListSearchCursor";
	private final DesignerContext context;
	private Map<String,List<Map<String,String>>> maplists;
	private Iterator<String> iterator;
	private final String parentName;
	private final String parentId;
	private final String nodeName;
	private final LoggerEx log;
	
	public NavAuxMapListSearchCursor(DesignerContext ctx,Map<String,List<Map<String,String>>> stringMapLists,String parent,String node,String uuid) {
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.maplists = stringMapLists;
		this.iterator = null;
		if( maplists!=null ) {
			iterator = maplists.keySet().iterator();
		}
		this.parentName = parent;
		this.nodeName = node;
		this.parentId = uuid;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if(maplists!=null && maplists.size()>0 ) {
			if( iterator.hasNext() ) {
				 String key = iterator.next();
				 List<Map<String,String>> list = maplists.get(key);
				so = new NavAuxMapListSearchObject(context,key,list,parentName,nodeName,parentId);
				log.infof("%s.next %s",CLSS,parentName,nodeName);
			}
		}
		return so;
	}
}
