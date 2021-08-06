package com.ils.blt.designer.search;

import java.util.Enumeration;

import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.findreplace.SearchObjectCursor;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.ProjectBrowserRoot;

public class ApplicationSearchCursor extends SearchObjectCursor {
	private final String TAG = "DiagramSearchCursor";
	private final DesignerContext context;
	private ProjectResource application; 
	private final LoggerEx log;
	private final long resId;
	private int index = 0;
	
	public ApplicationSearchCursor(DesignerContext ctx,long res) {
		this.context = ctx;
		this.resId = res;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if( index==0 ) {
			application = context.getProject().getLocalResource(resId);
			String rootName = getRootName();
			so = new ApplicationNameSearchObject(context,rootName,application.getName());
			log.infof("%s.next %s",TAG,application.getName());
		}
		index++;
		return so;
	}

	/**
	 * Select an item in the navigation tree given its path. Often we have only 
	 * the parentId, so we use that and append the node name. The path is colon-separated
	 * with a leading colon for the root.
	 */
	public String getRootName() {
		ProjectBrowserRoot project = context.getProjectBrowserRoot();
		String name = "";
		// Get the "ROOT" node and return its text
		AbstractNavTreeNode root = project.findChild("Project");   // Start at the Project node
		@SuppressWarnings("unchecked")
		Enumeration<AbstractNavTreeNode> nodeWalker = root.children();
		AbstractNavTreeNode child = null;
		while( nodeWalker.hasMoreElements() ) {
			child = nodeWalker.nextElement();
			if( child.getName().equalsIgnoreCase("ROOT")) {
				root = child;
				break;
			}
		}
		if( root!=null ) name = root.getText();
		return name;
	}
}
