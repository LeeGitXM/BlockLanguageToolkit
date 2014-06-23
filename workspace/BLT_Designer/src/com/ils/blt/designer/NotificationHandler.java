/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer;

import java.util.HashMap;
import java.util.Map;

import javax.swing.SwingUtilities;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.control.NotificationChangeListener;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.PushNotificationListener;
import com.inductiveautomation.ignition.common.gateway.messages.PushNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  The controller is a singleton used to manage interactions between sequential control blocks
 *  in the Designer scope with the model of these blocks in the Gateway. The handler maintains a map of 
 *  blocks, by name, and uses this to apply property value changes as indicated
 *  by the Gateway.
 *  
 *  The moduleId used within the calls refers to the module that has the handler for the 
 *  method that is invoked.
 */
public class NotificationHandler implements PushNotificationListener {
	private static String TAG = "NotificationHandler";
	private final LoggerEx log;
	private final Map<String,NotificationChangeListener> changeListenerMap;
	private static NotificationHandler instance = null;
	
	/**
	 * The handler, make this private per Singleton pattern ...
	 */
	private NotificationHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		// Register as listener for notifications
		GatewayConnectionManager.getInstance().addPushNotificationListener(this);
		changeListenerMap = new HashMap<String,NotificationChangeListener>();
	}
	
	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static NotificationHandler getInstance() {
		if( instance==null) {
			synchronized(NotificationHandler.class) {
				instance = new NotificationHandler();
			}
		}
		return instance;
	}
	
	/**
	 * Receive notification from the gateway. The messages contain a key which must match an entry
	 * in our map of listeners. In addition, we must match the moduleId. For now, there is only
	 * one listener per key.
	 *   1) The module ID
	 *   2) The key (contains a UUID)
	 *   3) Lookup object with UUID
	 *   4) Update the object state.
	 */
	@Override
	public void receiveNotification(PushNotification notice) {
		String moduleId = notice.getModuleId();
		if( moduleId.equals(BLTProperties.MODULE_ID)) {
			String key = notice.getMessageType();
			Object payload = notice.getMessage();
			log.infof("%s.receiveNotification: key=%s,value=%s",TAG,key,payload.toString());
			if( payload instanceof QualifiedValue ) {
				NotificationChangeListener listener = changeListenerMap.get(key);
				if( listener != null ) {
					listener.valueChange((QualifiedValue)payload);
					// Repaint the workspace
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",TAG,key,payload.toString());
				}
			}
			else {
				log.warnf("%s.receiveNotification: key:%s, payload %s (ignored)",TAG,key,payload.getClass().getName());
			}
			
		}
	}

	/**
	 * The key used for PushNotification is unique for each receiver. Consequently we make a map
	 * containing each interested recipient, by key. When an update arrives we notify, at most,
	 * one listener.
	 * @param key
	 * @param listener
	 */
	public void addNotificationChangeListener(String key,NotificationChangeListener listener) {
		log.tracef("%s.addNotificationChangeListener: key=%s (%s)",TAG,key,listener.getClass().getName());
		changeListenerMap.put(key, listener);
	}
	
	/**
	 * Remove the specified object from the listener map.
	 * @param key
	 */
	public void removeNotificationChangeListener(String key) {
		log.tracef("%s.removeNotificationChangeListener: key=%s",TAG,key);
		changeListenerMap.remove(key);
	}
}
