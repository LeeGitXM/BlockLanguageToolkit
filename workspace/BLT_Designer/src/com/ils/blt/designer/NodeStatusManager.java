/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer;

import java.util.HashMap;
import java.util.Map;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

import com.ils.blt.common.serializable.DiagramState;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  This class retains a map of nav tree node indexed by resource number.
 *  It is a central reporting point for the current status of each node.
 *  
 *  The resourceId is known both the the view code and the nav tree.
 */
public class NodeStatusManager  {
	private static String TAG = "NodeStatusManager";
	private final LoggerEx log;
	private final Map<Long,StatusEntry> statusMap;
	private final EventListenerList listenerList;
	private final ChangeEvent changeEvent;
	/**
	 * The handler, make this private per Singleton pattern ...
	 */
	public NodeStatusManager() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		this.listenerList = new EventListenerList();
		this.changeEvent  = new ChangeEvent(this);
		statusMap = new HashMap<>();
	}
	
	/**
	 * Define a new resource. The default should work for newly discovered resources
	 * @param resourceId
	 */
	public void defineResource(long parentResource,long resourceId) {
		log.infof("%s.defineResource(%d:%d)",TAG,parentResource,resourceId);
		statusMap.put(resourceId,new StatusEntry(parentResource));
	}
	/**
	 * Delete a resource.
	 * @param resourceId
	 */
	public void removeResource(long resourceId ) {
		log.infof("%s.removeResource(%d)",TAG,resourceId);
		statusMap.remove(resourceId);
	}
	public DiagramState getResourceState(long resourceId) {
		DiagramState result = DiagramState.ACTIVE;  
		StatusEntry se = statusMap.get(resourceId);
		if( se!=null ) result = se.getState();
		return result;
	}
	/**
	 * For a diagram, specify that all child blocks have been saved.
	 * @param resourceId
	 */
	public void clearDirtyBlockCount(long resourceId) {
		log.infof("%s.clearDirtyBlockCount(%d)",TAG,resourceId);
		StatusEntry se = statusMap.get(resourceId);
		if( se!=null ) se.clearDirtyChildCount();
	}
	/**
	 * For a diagram, note that a child block has been saved.
	 * @param resourceId
	 */
	public void decrementDirtyBlockCount(long resourceId) {
		log.infof("%s.decrementDirtyBlockCount(%d)",TAG,resourceId);
		StatusEntry se = statusMap.get(resourceId);
		if( se!=null ) {
			int prior = se.getDirtyChildCount();
			se.decrementDirtyChildCount();
			if( prior==1 && !se.isDirty()) {
				// Is newly clean because of children
				recursivelyDecrementDirtyCount(se.parentId);
				fireStateChanged();
			}
		}
	}
	/**
	 * For a diagram, specify that all child blocks have been saved.
	 * If this changes the state of the diagram, propagate up the tree.
	 * @param resourceId
	 */
	public void incrementDirtyBlockCount(long resourceId) {
		log.infof("%s.incrementDirtyBlockCount(%d)",TAG,resourceId);
		StatusEntry se = statusMap.get(resourceId);
		if( se!=null ) {
			int prior = se.getDirtyChildCount();
			se.incrementDirtyChildCount();
			if( (prior==0) && (!se.isDirty()) ) {
				// Newly dirty because of children
				recursivelyIncrementDirtyCount(se.parentId);
				fireStateChanged();
			}
		}
		else {
			log.warnf("%s.incrementDirtyBlockCount(%d) - status entry does not exist",TAG,resourceId);
		}
	}
	
	public boolean isResourceDirty(long resourceId) {
		boolean result = true;        // If we haven't seen it yet, it's dirty
		StatusEntry se = statusMap.get(resourceId);
		if( se!=null ) result = se.isDirty()||(se.getDirtyChildCount()>0);
		else log.infof("%s.isResourceDirty(%d) - resource UNDEFINED",TAG,resourceId);
		return result;
	}
	
	/**
	 * If the dirtyBlock count > 0, then setting the resource will have no effect.
	 * @param resourceId
	 * @param dirtFlag
	 */
	public void setResourceDirty(long resourceId,boolean dirtFlag) {
		log.infof("%s.setResourceDirty(%d) %s",TAG,resourceId,(dirtFlag?"DIRTY":"CLEAN"));
		StatusEntry se = statusMap.get(resourceId);
		if( se!=null && se.isDirty()!= dirtFlag && se.getDirtyChildCount()==0) {
			se.setDirty(dirtFlag);
			// Propagate the change up the tree - if this represents a change
			if( dirtFlag  ) recursivelyIncrementDirtyCount(se.parentId);
			else            recursivelyDecrementDirtyCount(se.parentId);
		}
		fireStateChanged();
	}
	
	public void setResourceState(long resourceId,DiagramState bs) {
		StatusEntry se = statusMap.get(resourceId);
		if( se!=null ) {
			se.setState(bs);
		}
	}
	private void recursivelyDecrementDirtyCount(long resId) {
		log.infof("%s.recursivelyDecrementDirtyCount(%d)",TAG,resId);
		StatusEntry se = statusMap.get(resId);
		if( se!=null ) {
			se.decrementDirtyChildCount();
			recursivelyDecrementDirtyCount(se.parentId);
		}
	}
	private void recursivelyIncrementDirtyCount(long resId) {
		log.infof("%s.recursivelyIncrementDirtyCount(%d)",TAG,resId);
		StatusEntry se = statusMap.get(resId);
		if( se!=null ) {
			se.incrementDirtyChildCount();
			recursivelyIncrementDirtyCount(se.parentId);
		}
	}
	
	/**
	 * For a diagram, the dirty child count represents dirty blocks.
	 * For folders, the children are other folders, including applications
	 * and families.
	 * 
	 * @author chuckc
	 */
	private class StatusEntry {
		private int dirtyChildren = 0;
		private boolean dirty = false;
		private final long parentId; 
		private DiagramState state = DiagramState.ACTIVE;
		
		public StatusEntry(long parent)  {
			this.parentId = parent;
		}
		public void clearDirtyChildCount() {dirtyChildren=0;}
		public void decrementDirtyChildCount() {dirtyChildren-=1;}
		public int getDirtyChildCount() {return dirtyChildren;}
		public DiagramState getState() {return state;}
		public void incrementDirtyChildCount() {dirtyChildren+=1;}
		// Note: isDirty refers to the node of interest alone, excluding children
		public boolean isDirty() {return dirty;}
		public void setDirty(boolean dirty) {this.dirty = dirty;}
		public void setState(DiagramState state) {this.state = state;}
	}
	
	// =================== Handle Event Listeners ===================
		public void addChangeListener(ChangeListener l) {
		     listenerList.add(ChangeListener.class, l);
		 }

		 public void removeChangeListener(ChangeListener l) {
		     listenerList.remove(ChangeListener.class, l);
		 }


		 // Notify all listeners that have registered interest for
		 // notification on this event type.  The event instance
		 // is lazily created using the parameters passed into
		 // the fire method.
		 public void fireStateChanged() {
		     // Guaranteed to return a non-null array
		     Object[] listeners = listenerList.getListenerList();
		     // Process the listeners last to first, notifying
		     // those that are interested in this event
		     for (int i = listeners.length-2; i>=0; i-=2) {
		         if (listeners[i]==ChangeListener.class) {
		             ((ChangeListener)listeners[i+1]).stateChanged(changeEvent);
		         }
		     }
		 }
}
