/**
 *   (c) 2013-2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.designer.navtree;



/**
 * This interface defines a NavTreeNode with methods used by the NodeStatusManager. 
 */
public interface BLTNavTreeNode  {
	/**
	 * Clean up any linkages in preparation for the node being deleted.
	 */
	public void prepareForDeletion();
	/**
	 * Notify the node if it is in/out of sync with the gateway 
	 * @param flag true if the node is out-of-sync with the gateway 
	 */
	public void setDirty(boolean flag);
}