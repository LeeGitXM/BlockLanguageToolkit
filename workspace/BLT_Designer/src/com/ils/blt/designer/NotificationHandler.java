/**
 *   (c) 2013-2016  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer;

import java.util.HashMap;
import java.util.Map;

import javax.swing.SwingUtilities;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.client.gateway_interface.PushNotificationListener;
import com.inductiveautomation.ignition.common.gateway.messages.PushNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 *  The handler is a singleton used to manage interactions between sequential control blocks
 *  in the Designer scope with the model of these blocks in the Gateway. The handler maintains a map of 
 *  blocks, by name, and uses this to apply property value changes as indicated by the Gateway.
 *  
 *  We maintain a payload table so that we can inform diagrams with the latest values when they are
 *  displayed for the first time, or re-displayed.
 *  
 *  The moduleId used within the calls refers to the module that has the handler for the 
 *  method that is invoked.
 */
public class NotificationHandler implements PushNotificationListener {
	private static String TAG = "NotificationHandler";
	private final LoggerEx log;
	private final Map<String,Map<String,NotificationChangeListener>> changeListenerMap;
	private final Map<String,Object> payloadMap;        // Keyed by the message type.
	private static NotificationHandler instance = null;
	private BLTDesignerHook hook = null;
	
	/**
	 * The handler, make this private per Singleton pattern ...
	 */
	private NotificationHandler() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		// Register as listener for notifications
		GatewayConnectionManager.getInstance().addPushNotificationListener(this);
		// The first string is the key that we're listening on. Then we get a map
		// keyed by "source", a string unique to the component getting the notification.
		changeListenerMap = new HashMap<String,Map<String,NotificationChangeListener>>();
		payloadMap = new HashMap<String,Object>();
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
	
	public void setHook(BLTDesignerHook h) { this.hook = h; }
	
	/**
	 * Receive notification from the gateway. The messages contain a key which must match an entry
	 * in our map of listeners. In addition, we must match the moduleId. There is only one listener 
	 * per key. Multiple entities registering for the same notification must differentiate themselves
	 * by the key's "source" attribute.
	 * 
	 * Our notification is filtered on ModuleId. Otherwise each listener registers for a specific
	 * NotificationKey - a string. In general, it contains a type, blockUUID and name.
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
			log.tracef("%s.receiveNotification: key=%s,value=%s",TAG,key,(payload==null?"null":payload.toString()));
			if( payload==null ) return; // Ignore
			
			// Process alert change notifications independent of "attached" status
			if(NotificationKey.isDiagramAlertKey(key)) {
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						log.tracef("%s.receiveNotification: key=%s - notifying %s",TAG,key,listener.getClass().getName());
						// Listener is the node status manager - // Refresh the Nav tree
						long resourceId = Long.parseLong(key.substring(2));
						listener.diagramAlertChange(resourceId, payload.toString());
					}
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",TAG,key,payload.toString());
				}
			}
			else if(hook==null || !hook.attachDiagrams()) {
				;  // Ignore UI updates if the diagrams are "detached" 
			}
			else if( payload instanceof QualifiedValue ) {
				payloadMap.put(key, payload);
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {

					for(NotificationChangeListener listener:listeners.values()) {
						log.tracef("%s.receiveNotification: key=%s - notifying %s",TAG,key,listener.getClass().getName());
						listener.valueChange((QualifiedValue)payload);
					}
					// Repaint the workspace
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",TAG,key,payload.toString());
				}
			}
			else if(NotificationKey.isPropertyBindingKey(key)) {
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						log.tracef("%s.receiveNotification: key=%s - notifying %s",TAG,key,listener.getClass().getName());
						listener.bindingChange(payload.toString());
					}
					// Repaint the workspace
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",TAG,key,payload.toString());
				}
			}
			else if(NotificationKey.isWatermarkKey(key)) {
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						log.tracef("%s.receiveNotification: key=%s - notifying %s",TAG,key,listener.getClass().getName());
						listener.watermarkChange(payload.toString());
					}
					// Repaint the workspace
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",TAG,key,payload.toString());
				}
			}
			else {
				log.warnf("%s.receiveNotification: key:%s, payload %s=%s (ignored)",TAG,key,payload.getClass().getName(),payload.toString());
			}
			
		}
	}
	/**
	 * Receive notification from a ProcessViewDiagram in the Designer. This is a mechanism
	 * to restore the diagram display to its state prior to its last serialization.
	 * Note: This is used only for properties bound to ENGINE.
	 */
	public void initializeNotification(String key,QualifiedValue value) {
		if( key==null || value==null) return;
		// Only initialize the payload map if the key doesn't exist
		Object payload = payloadMap.get(key);
		if( payload==null) payloadMap.put(key, value);
		Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
		if( listeners != null ) {
			for(NotificationChangeListener listener:listeners.values()) {
				log.tracef("%s.initializeNotification: key=%s - notifying %s",TAG,key,listener.getClass().getName());
				if( NotificationKey.isPropertyValueKey(key) ) listener.valueChange(value);
			}
			// Repaint the workspace
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	/**
	 * The key used for PushNotification is unique for each receiver. Consequently we make a map
	 * containing each interested recipient, by key. When an update arrives we notify each listener
	 * registered for the event. On registration, we update with the latest status.
	 * @param key
	 * @param listener
	 */
	public void addNotificationChangeListener(String key,String source,NotificationChangeListener listener) {
		log.debugf("%s.addNotificationChangeListener: source=%s key=%s (%s)",TAG,source,key,listener.getClass().getName());
		Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
		if( listeners==null) {
			listeners = new HashMap<>();
			changeListenerMap.put(key, listeners);
		}
		listeners.put(source,listener); 
		
		// Make an immediate update 
		Object payload = payloadMap.get(key);
		if( payload!=null && ((QualifiedValue)payload).getValue()!=null ) {
			listener.valueChange((QualifiedValue)payload);
			// Repaint the workspace
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	
	/**
	 * Remove the specified object from the listener map.
	 * @param key
	 */
	public void removeNotificationChangeListener(String key, String source) {
		log.tracef("%s.removeNotificationChangeListener: key=%s",TAG,key);
		Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
		if( listeners!=null ) {
			listeners.remove(source);
			if( listeners.isEmpty()) changeListenerMap.remove(key);
		}	
	}
}
