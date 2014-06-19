/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer;

import java.util.HashMap;
import java.util.Map;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.notification.NotificationChangeListener;
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

	
	/**
	 * The handler, a delegate of the hook class ...
	 */
	public NotificationHandler() {
		super();
		log = LogUtil.getLogger(getClass().getPackage().getName());
		changeListenerMap = new HashMap<String,NotificationChangeListener>();
	}
	
	
	
	/**
	 * Receive notification from the gateway. The messages contain a key which must match an entry
	 * in our map of listeners. In addition, we must match the moduleId.
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
			log.debugf("%s.receiveNotification: key:%s,value:%s",TAG,key,payload.toString());
			if( payload instanceof QualifiedValue ) {
				NotificationChangeListener listener = changeListenerMap.get(key);
				if( listener != null ) {
					listener.valueChange((QualifiedValue)payload);
				}
				else {
					log.debugf("%s.receiveNotification: no receiver for key:%s,value:%s",TAG,key,payload.toString());
				}
			}
			else {
				log.warnf("%s.receiveNotification: key:%s, payload %s (ignored)",TAG,key,payload.getClass().getName());
			}
			
		}
		
		/*
		if(!type.equalsIgnoreCase(DTProperties.GATEWAY_BLOCK_STATE_MESSAGE)) return;
		try {
			Properties props = (Properties) payload;
			String model = props.getProperty(ILSProperties.MSG_WORKSPACE_ID);
			String name  = props.getProperty(ILSProperties.MSG_BLOCK_NAME);
			String state = props.getProperty(ILSProperties.MSG_BLOCK_STATE);
			// The workspace ID must match the model
			if( model.equals(wksp.getParentFolderUUID())) {
				List<AbstractCoreComponent> components = new ArrayList<AbstractCoreComponent>();
				wksp.findModelComponents(wksp.getRootContainer(),components);
				for( AbstractCoreComponent blk:components ) {
					if( blk.getName().equals(name) ) {
						blk.setPropertyValue(ILSProperties.MSG_BLOCK_STATE, state);
						break;
					}
				}
			}
		}
		catch(ClassCastException cce ) {}  // Ignore
		*/
	}

	/**
	 * The key used for PushNotification is unique for each receiver. Consequently we make a map
	 * containing each interested recipient, by key. When an update arrives we notify, at most,
	 * one listener.
	 * @param key
	 * @param listener
	 */
	public void addNotificationChangeListener(String key,NotificationChangeListener listener) {
		changeListenerMap.put(key, listener);
	}
	
	/**
	 * Remove the specified object from the listener map.
	 * @param key
	 */
	public void removeNotificationChangeListener(String key) {
		changeListenerMap.remove(key);
	}
}
