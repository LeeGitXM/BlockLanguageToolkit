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
	 * There are times when the project resource has been deleted 
	 * and all we have to go on is the resourceId
	 * @return the resourceId identified with the node when it was created.
	 */
	public ProjectResourceId getResourceId();
	/**
	 * Clean up any linkages in preparation for the node being deleted.
	 */
	public void prepareForDeletion();
	/**
	 * Handle a node rename
	 */
	public void setName(String name);
	/**
	 * Notify the node if it is in/out of sync with the gateway 
	 * @param flag true if the node is out-of-sync with the gateway 
	 */
	public void updateUI(boolean flag);
}