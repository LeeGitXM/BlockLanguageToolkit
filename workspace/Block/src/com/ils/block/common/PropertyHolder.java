/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.block.common;

import java.util.HashMap;

import javax.swing.event.EventListenerList;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.control.BlockPropertyChangeEvent;
import com.ils.blt.common.control.BlockPropertyChangeListener;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * This class holds the property objects for a block. It also allows the
 * block to receive notifications of property changes. 
 */
public class PropertyHolder extends HashMap<String,BlockProperty> {
	private static final long serialVersionUID = 3907675980238399664L;
	protected final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final EventListenerList listenerList;
	
	/**
	 * Constructor: 
	 */
	public PropertyHolder() {
		listenerList = new EventListenerList();
	}

	 public void addBlockPropertyChangeListener(BlockPropertyChangeListener l) {
	     listenerList.add(BlockPropertyChangeListener.class, l);
	 }

	 public void removeBlockPropertyChangeListener(BlockPropertyChangeListener l) {
	     listenerList.remove(BlockPropertyChangeListener.class, l);
	 }

	 /**
	  * Notify all PropertyChange listeners.  The event instance is lazily created using the parameters passed into
	  * the fire method.
	  * @param source block Id of originating block.
	  * @param name
	  * @param oldValue
	  * @param newValue
	  */ 
	 protected void firePropertyChange(String source,String name,QualifiedValue oldValue,QualifiedValue newValue) {
	     // Guaranteed to return a non-null array
	     Object[] listeners = listenerList.getListenerList();
	     // Process the listeners last to first, notifying
	     // those that are interested in this event
	     BlockPropertyChangeEvent event = new BlockPropertyChangeEvent(source,name,oldValue,newValue);
	     for (int i = listeners.length-2; i>=0; i-=2) {
	         ((BlockPropertyChangeListener)listeners[i+1]).propertyChange(event);
	     }
	 }
}