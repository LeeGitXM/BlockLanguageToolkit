/**
 *   (c) 2013-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.search;

import java.util.Enumeration;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.designer.navtree.NavTreeFolder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.ProjectBrowserRoot;


/**
 *  The locator is designed to navigate to the named path in the navigation tree.
 *  This is as a consequence of double-clicking the icon in the find/replace window.
 */
public class NavTreeLocator {
	private static String CLSS = "NavTreeLocator";
	private final LoggerEx log;
	private final DesignerContext context;

	/**
	 * The handler
	 */
	public NavTreeLocator(DesignerContext ctx) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = ctx;
	}

	/**
	 * Select an item in the navigation tree given its name and the Id of its immediate parent.  
	 * We use the parentId, append the node name to construct the path. It is colon-separated
	 * with a leading colon for the root.
	 */
	public void locate(ProjectResourceId parentId,String name) {
		ApplicationRequestHandler handler = new ApplicationRequestHandler();
		String path = parentId.getFolderPath();
		if( path!=null ) {
			ProjectBrowserRoot project = context.getProjectBrowserRoot();
			AbstractNavTreeNode root = null;
			AbstractNavTreeNode node = null;
			// Get the "ROOT" node before we traverse the hierarchy.
			root = project.findChild("Project");
			if( root!=null ) node = findChildInTree(root,"ROOT");

			// The specified path is colon-delimited.
			if( name!=null && !name.isEmpty() ) path = path+":"+name;
			String[] pathArray = path.toString().split(":");

			int index = 1;   // Skip the empty root
			while( index<pathArray.length ) {
				node = findChildInTree(node,pathArray[index]);
				if( node!=null ) {
					node.expand();
					try {
						Thread.sleep(100); 
					}
					catch(InterruptedException ignore) {}
				}
				else{
					log.warnf("%s.locate: Unable to find node (%s) on browser path",CLSS,pathArray[index]);
					break;
				}
				index++;
			}

			if( node!=null ) {
				node.onDoubleClick();    // Opens the diagram
			}
			else {
				log.warnf("%s.locate: Unable to open browser path (%s)",CLSS,path.toString());
			}
		}
	}
	
	/**
	 * Select an item in the navigation tree given its id.
	 */
	public void locate(ProjectResourceId resourceId) {
		String path = resourceId.getResourcePath().getFolderPath();
		locate(path);
	}
	/**
	 * Open an item immediately under the root node. There is no parent.
	 */
	public void locate(String path) {
		if( path!=null ) {
			ProjectBrowserRoot projBrowser = context.getProjectBrowserRoot();
			AbstractNavTreeNode node = null;
			// The root node is of class NavTreeFOlder
			NavTreeFolder root = null;
			Enumeration<AbstractNavTreeNode> enumeration = projBrowser.children();
			while( enumeration.hasMoreElements()) {
				node = enumeration.nextElement();
				if( node instanceof NavTreeFolder ) {
					root = (NavTreeFolder) node;
					break;
				}
			}

			// The specified path is slash-delimited.
			String[] pathArray = path.toString().split("/");

			int index = 0;
			while( index<pathArray.length ) {
				node = findChildInTree(node,pathArray[index]);
				if( node!=null ) {
					node.expand();
					try {
						Thread.sleep(100); 
					}
					catch(InterruptedException ignore) {}
				}
				else{
					log.warnf("%s.locate: Unable to find node (%s) on nav tree path",CLSS,pathArray[index]);
					break;
				}
				index++;
			}

			if( node!=null ) {
				node.onDoubleClick();    // Opens the diagram
			}
			else {
				log.warnf("%s.locate: Unable to open nav tree path (%s)",CLSS,path.toString());
			}
		}
	}
	
	/**
	 * We have not been successful with the findChild method .. so we've taken it on ourselves.
	 * @param root
	 * @param name
	 * @return
	 */
	private AbstractNavTreeNode findChildInTree(AbstractNavTreeNode root,String name) {
		AbstractNavTreeNode match = null;
		if( root!=null ) {
			@SuppressWarnings("unchecked")
			Enumeration<AbstractNavTreeNode> nodeWalker = root.children();
			AbstractNavTreeNode child = null;

			while( nodeWalker.hasMoreElements() ) {
				child = nodeWalker.nextElement();
				//log.tracef("%s.findChildInTree: testing %s vs %s",TAG,name,child.getName());
				if( child.getName().equalsIgnoreCase(name)) {
					match = child;
					break;
				}
			}
		}
		return match;
	}

}
