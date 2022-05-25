/**
 *   (c) 2013-2022	  ILS Automation. All rights reserved. 
 */
package com.ils.blt.designer.navtree;

import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * This interface defines a NavTreeNode with methods used by the NodeStatusManager. 
 */
public interface NavTreeNodeInterface  {
	/**
	 * @return the node name as it appears in the NavTree
	 */
	public String getName();
	/**
	 * There are times when the project resource has been deleted 
	 * and all we have to go on is the resourceId
	 * @return the resourceId identified with the node when it was created.
	 */
	public ProjectResourceId getResourceId();
	/**
	 * Refresh the UI
	 */
	public void reload();
	/**
	 * Handle a node rename
	 */
	public void setName(String name);
	/**
	 * Paint the name with italics, or not, to indicate if the node needs saving.
	 * @param flag true if the node is in a saved state
	 */
	public void updateUI(boolean flag);
}