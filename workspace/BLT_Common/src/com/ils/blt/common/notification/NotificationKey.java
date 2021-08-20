/**
 *   (c) 2014-2016  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;

import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * This class contains static methods for generating keys for the
 * various notification target types. 
 */
public class NotificationKey
{
	/**
	 * @param resid resourceId corresponding to the diagram
	 * @return  a push-notification key for a diagram alert status. 
	 * 			The Id is simply the resource Id.
	 */
	public static String keyForAlert(ProjectResourceId resid) {
		String key = resid.getResourcePath().getPath().toString();
		return String.format("A:%s",key);
	}
	/**
	 * @param blkid string version of the block's UUID
	 * @return  a push-notification key for aux data. The Id is
	 *         the block, or node Id.
	 */
	public static String keyForAuxData(String blkid) {
		return String.format("X:%s",blkid);
	}
	/**
	 * @param blkid string version of the block's UUID
	 * @return  a push-notification key for a block.
	 */
	public static String keyForBlockName(String blkid) {
		return String.format("N:%s",blkid);
	}
	/**
	 * @param blockid string version of the block's UUID
	 * @param port name of the anchor where the connection attaches
	 * @return  a push-notification key for a connection. The block and port
	 *          uniquely identify the connection. The listener is (usually)
	 *          a BasicAnchorPoint.
	 */
	public static String keyForConnection(String blockid,String port) {
		return String.format("C:%s:%s",blockid,port);
	}
	/**
	 * @param resourceId corresponding to the diagram
	 * @return  a push-notification key for a diagram. The Id is
	 *         the diagram's resource Id.
	 */
	public static String keyForDiagram(ProjectResourceId resourceId) {
		String key = resourceId.getResourcePath().getPath().toString();
		return String.format("D:%s",key);
	}
	/**
	 * @param blkid string version of the block's UUID
	 * @param pname name of the property
	 * @return  a push-notification key for a block property. The Id is
	 *         the block Id followed by the property name.
	 */
	public static String keyForProperty(String blkid,String pname) {
		return String.format("P:%s:%s",blkid,pname);
	}
	
	/**
	 * Given either a PropertyValue or a PropertyBinding key,
	 * extract the property name.
	 * @param key
	 * @return the property name
	 */
	public static String propertyFromKey(String key) {
		int pos = key.lastIndexOf(":");
		return key.substring(pos+1);
	}
	/**
	 * @param blkid string version of the block's UUID
	 * @param pname name of the property
	 * @return  a push-notification key for a block property. The Id is
	 *         the block Id followed by the property name.
	 */
	public static String keyForPropertyBinding(String blkid,String pname) {
		return String.format("B:%s:%s",blkid,pname);
	}

	/**
	 * @param key the key value 
	 * @return  true if this is a connection key.
	 */
	public static boolean isConnectionKey(String key) {
		return key.startsWith("C");
	}
	/**
	 * Test a key for type.
	 * @param key the key value 
	 * @return  true if this is a diagram alert key.
	 */
	public static boolean isAlertKey(String key) {
		return key.startsWith("A");
	}
	/**
	 * Test a key for type.
	 * @param key the key value 
	 * @return  true if this is an aux data key.
	 */
	public static boolean isAuxDataKey(String key) {
		return key.startsWith("X");
	}
	/**
	 * Test a key for type.
	 * @param key the key value 
	 * @return  true if this is a diagram state change key.
	 */
	public static boolean isDiagramKey(String key) {
		return key.startsWith("D");
	}
	/**
	 * Test a key for type. 
	 * @param key the key value 
	 * @return  true if this is a name change key.
	 */
	public static boolean isNameChangeKey(String key) {
		return key.startsWith("N");
	}
	/**
	 * Test a key for type. 
	 * @param key the key value 
	 * @return  true if this is a binding key.
	 */
	public static boolean isPropertyBindingKey(String key) {
		return key.startsWith("B");
	}
	/**
	 * Test a key for type. 
	 * @param key the key to test 
	 * @return  true if this is a value key.
	 */
	public static boolean isPropertyValueKey(String key) {
		return key.startsWith("P");
	}
	/**
	 * Test if a key changes a watermark.
	 * @param key the key to test  
	 * @return  true if this is a watermark key.
	 */
	public static boolean isWatermarkKey(String key) {
		return key.startsWith("W");
	}
	/**
	 * @param did diagramId
	 * @return  a push-notification key for a diagram's watermark. The Id is
	 *         simply the diagram Id.
	 */
	public static String watermarkKeyForDiagram(String did) {
		return String.format("W:%s",did);
	}
}
