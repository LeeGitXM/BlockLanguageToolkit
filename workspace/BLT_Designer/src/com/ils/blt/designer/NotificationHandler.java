/**
 *   (c) 2013-2021  ILS Automation. All rights reserved.
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
 *  method that is invoked. All push notifications are made using the BlockExecutionController.
 *  Refer to it to determine the class of the payload..
 */
public class NotificationHandler implements PushNotificationListener {
	private static String CLSS = "NotificationHandler";
	private final LoggerEx log;
	private final boolean DEBUG = false;
	private final Map<String,Map<String,NotificationChangeListener>> changeListenerMap;
	private final Map<String,Object> payloadMap;        // Keyed by the message type.
	private static NotificationHandler instance = null;
	private BLTDesignerHook hook = null;
	
	/**
	 * The handler, make this private per Singleton pattern ...
	 */
	private NotificationHandler() {
		log = LogUtil.getLogger(getClass().getPackageName());
		// Register as listener for notifications
		GatewayConnectionManager.getInstance().addPushNotificationListener(this);
		// The first string is the key that we're listening on. Then we get a map
		// keyed by "source", a string unique to the component getting the notification.
		changeListenerMap = new HashMap<String,Map<String,NotificationChangeListener>>();
		payloadMap = new HashMap<>();
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
			Object message = notice.getMessage();	
			if( message==null ) return; // Ignore
			if(DEBUG) log.infof("%s.receiveNotification: key=%s,value=%s",CLSS,key,message.toString());
			
			// Process alert change notifications independent of "attached" status
			if(NotificationKey.isAlertKey(key)) {
				String payload = message.toString();
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						// Listener is the node status manager - // Refresh the bell badges on the Nav tree
						//log.infof("%s.receiveNotification: diagram key=%s - notifying %s of %s",CLSS,key,
						//		listener.getClass().getName(),payload.toString());
						String path = key.substring(2);
						listener.diagramStateChange(path, payload);
					}
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",CLSS,key,payload);
				}
			}
			// Auxiliary data have changed. These may be associated with a block, application or family
			else if(NotificationKey.isAuxDataKey(key)) {
				QualifiedValue payload = (QualifiedValue)message;
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						log.infof("%s.receiveNotification: value %s=%s - notifying %s",CLSS,
								key,payload.getValue().toString(),listener.getClass().getName());
						listener.valueChange(payload);
					}
				}
				else {
					log.infof("%s.receiveNotification: no receiver for key=%s,value=%s",CLSS,key,payload.toString());
				}
			}
			// Notify an open diagram of a state change
			// Used mostly by NavTree to animate diagram status
			else if(NotificationKey.isDiagramKey(key)) {
				String payload = message.toString();
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						//log.infof("%s.receiveNotification: diagram key=%s - notifying %s of %s, ",CLSS,key,
								  //listener.getClass().getName(),payload.toString());
						// Listener is a diagram, the value is the new state. 
						// The diagram is expected to ignore all but its own resId
						String path = key.substring(2);
						listener.diagramStateChange(path, payload);
					}
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",CLSS,key,payload);
				}
			}
			// Listener is a basic anchor point.
			else if(NotificationKey.isConnectionKey(key)) {
				QualifiedValue payload = (QualifiedValue)message;
				payloadMap.put(key, payload);
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						//log.infof("%s.receiveNotification: value key=%s - notifying %s",CLSS,key,listener.getClass().getName());
						listener.valueChange(payload);
					}
					// Repaint the workspace
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",CLSS,key,payload.toString());
				}
			}
			else if(hook==null || !hook.attachDiagrams()) {
				;  // Ignore UI updates if the diagrams are "detached" 
			}
			else if(NotificationKey.isNameChangeKey(key)) {
				String payload = message.toString();
				payloadMap.put(key, payload);
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						log.tracef("%s.receiveNotification: rename key=%s - notifying %s",CLSS,key,listener.getClass().getName());
						listener.nameChange(payload);
					}
					// Repaint the workspace
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",CLSS,key,payload.toString());
				}
			}
			// Payload is a qualified value, but we report its simple value.
			// Key is P:blockid:pname
			else if(NotificationKey.isPropertyValueKey(key)) {
				Object payload = ((QualifiedValue)message).getValue();
				payloadMap.put(key, payload);
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						log.tracef("%s.receiveNotification: value %s=%s - notifying %s",CLSS,
								key,payload,listener.getClass().getName());
						String pname = NotificationKey.propertyFromKey(key);
						listener.propertyChange(pname,payload);
					}
					// Repaint the workspace
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",CLSS,key,payload.toString());
				}
			}
			else if(NotificationKey.isPropertyBindingKey(key)) {
				String payload = message.toString();
				payloadMap.put(key, payload);
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						log.tracef("%s.receiveNotification: binding key=%s - notifying %s",CLSS,key,listener.getClass().getName());
						String pname = NotificationKey.propertyFromKey(key);
						listener.bindingChange(pname,payload);
					}
					// Repaint the workspace
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",CLSS,key,payload.toString());
				}
			}
			else if(NotificationKey.isWatermarkKey(key)) {
				String payload = message.toString();
				payloadMap.put(key, payload);
				Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
				if( listeners != null ) {
					for(NotificationChangeListener listener:listeners.values()) {
						log.tracef("%s.receiveNotification: key=%s - notifying %s",CLSS,key,listener.getClass().getName());
						listener.watermarkChange(payload);
					}
					// Repaint the workspace
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key=%s,value=%s",CLSS,key,payload.toString());
				}
			}
			else {
				log.warnf("%s.receiveNotification: key:%s, payload %s=%s (ignored)",CLSS,key,message.getClass().getName(),message.toString());
			}
			
		}
	}
	/**
	 * Receive notification from a ProcessViewDiagram in the Designer. This is a mechanism
	 * to restore the diagram display to its state prior to its last serialization.
	 */
	public void initializeBlockNameNotification(String key,String name) {
		if( key==null || name==null) return;
		
		// Only initialize the payload map if the key doesn't exist
		String payload = (String)payloadMap.get(key);
		if(payload==null) payloadMap.put(key, name);
		Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
		if( listeners != null ) {
			for(NotificationChangeListener listener:listeners.values()) {
				log.tracef("%s.initializeBlockNameNotification: key=%s - notifying %s",CLSS,key,listener.getClass().getName());
				if( NotificationKey.isNameChangeKey(key) ) listener.nameChange(name);
			}
			// Repaint the workspace
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	/**
	 * Receive notification from a ProcessViewDiagram in the Designer. This is a mechanism
	 * to restore the diagram display to its state prior to its last serialization.
	 */
	public void initializePropertyBindingNotification(String key,Object value) {
		if( key==null || value==null) return;
		// Only initialize the payload map if the key doesn't exist
		Object payload = payloadMap.get(key);
		if( payload==null) payloadMap.put(key, value);
		Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
		if( listeners != null ) {
			for(NotificationChangeListener listener:listeners.values()) {
				log.tracef("%s.initializePropertyBindingNotification: key=%s - notifying %s",CLSS,key,listener.getClass().getName());
				if( NotificationKey.isPropertyBindingKey(key) ) {
					String pname = NotificationKey.propertyFromKey(key);
					listener.propertyChange(pname,value);
				}
			}
			// Repaint the workspace
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	/**
	 * Receive notification from a ProcessViewDiagram in the Designer. This is a mechanism
	 * to restore the diagram display to its state prior to its last serialization.
	 */
	public void initializePropertyValueNotification(String key,Object value) {
		if( key==null || value==null) return;
		// Only initialize the payload map if the key doesn't exist
		Object payload = payloadMap.get(key);
		if( payload==null) payloadMap.put(key, value);
		Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
		if( listeners != null ) {
			for(NotificationChangeListener listener:listeners.values()) {
				log.tracef("%s.initializePropertyValueNotification: key=%s - notifying %s",CLSS,key,listener.getClass().getName());
				if( NotificationKey.isPropertyValueKey(key) ) {
					String pname = NotificationKey.propertyFromKey(key);
					listener.propertyChange(pname,value);
				}
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
		addNotificationChangeListener(key,source,listener,true);
	}
	/**
	 * The key used for PushNotification is unique for each receiver. Consequently we make a map
	 * containing each interested recipient, by key. When an update arrives we notify each listener
	 * registered for the event.
	 * @param key
	 * @param listener
	 * @param update if true and if there is an existing notification matching the key, that notification
	 *               is sent immediately.
	 */
	public void addNotificationChangeListener(String key,String source,NotificationChangeListener listener,boolean update) {
		//log.infof("%s.addNotificationChangeListener: source=%s key=%s (%s)",CLSS,source,key,listener.getClass().getName());
		Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
		if( listeners==null) {
			listeners = new HashMap<>();
			changeListenerMap.put(key, listeners);
		}
		listeners.put(source,listener); 
		
		// Make an immediate update 
		Object payload = payloadMap.get(key);
		if( update && payload!=null ) {
			if( NotificationKey.isAuxDataKey(key))         listener.valueChange((QualifiedValue)payload);
			else if( NotificationKey.isConnectionKey(key)) {
				listener.valueChange((QualifiedValue)payload);
			}
			else if( NotificationKey.isNameChangeKey(key)) listener.nameChange(payload.toString());
			else if(NotificationKey.isPropertyBindingKey(key)) {
				String pname = NotificationKey.propertyFromKey(key);
				listener.bindingChange(pname,payload.toString());
			}
			// The property value key is a "simple" value, not a Qualified value
			else if(NotificationKey.isPropertyValueKey(key))   {
				String pname = NotificationKey.propertyFromKey(key);
				listener.propertyChange(pname,payload);
			}
			else if(NotificationKey.isWatermarkKey(key)) listener.watermarkChange(payload.toString());
			// Repaint the workspace
			SwingUtilities.invokeLater(new WorkspaceRepainter());
		}
	}
	
	/**
	 * Remove the specified object from the listener map.
	 * @param key
	 */
	public void removeNotificationChangeListener(String key, String source) {
		log.tracef("%s.removeNotificationChangeListener: key=%s",CLSS,key);
		Map<String,NotificationChangeListener> listeners = changeListenerMap.get(key);
		if( listeners!=null ) {
			listeners.remove(source);
			if( listeners.isEmpty()) changeListenerMap.remove(key);
		}	
	}
}
