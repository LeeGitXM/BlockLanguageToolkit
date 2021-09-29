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
	private final String CLSS = "DiagramSearchCursor";
	private final DesignerContext context;
	private ProjectResource application; 
	private final LoggerEx log;
	private int index = 0;
	
	public ApplicationSearchCursor(DesignerContext ctx,ProjectResource res) {
		this.context = ctx;
		this.application = res;
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.index = 0;
	}
	@Override
	public Object next() {
		Object so = null;   // Search Object
		if( index==0 ) {
			String rootName = getRootName();
			so = new ApplicationNameSearchObject(context,rootName,application.getResourceName());
			log.infof("%s.next %s",CLSS,application.getResourceName());
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
