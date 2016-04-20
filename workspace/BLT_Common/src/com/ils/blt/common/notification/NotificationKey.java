/**
 *   (c) 2014-2016  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;


/**
 * This class contains static methods for generating keys for the
 * various notification target types. 
 */
public class NotificationKey
{
	/**
	 * @return  a push-notification key for a diagram alert status. 
	 * 			The Id is simply the resource Id.
	 */
	public static String keyForAlert(long resid) {
		return String.format("A:%d",resid);
	}
	/**
	 * @return  a push-notification key for a block.
	 */
	public static String keyForBlock(String blkid) {
		return String.format("B:%s",blkid);
	}
	/**
	 * @return  a push-notification key for a connection. The block and port
	 *          uniquely identify the connection. The listener is (usually)
	 *          a BasicAnchorPoint.
	 */
	public static String keyForConnection(String blockid,String port) {
		return String.format("C:%s:%s",blockid,port);
	}
	/**
	 * @return  a push-notification key for a diagram. The Id is
	 *         simply the diagram Id.
	 */
	public static String keyForDiagram(String did) {
		return String.format("D:%s",did);
	}
	/**
	 * @return  a push-notification key for a block property. The Id is
	 *         the block Id followed by the property name.
	 */
	public static String keyForProperty(String blkid,String pname) {
		return String.format("P:%s:%s",blkid,pname);
	}
	/**
	 * @return  a push-notification key for a block property. The Id is
	 *         the block Id followed by the property name.
	 */
	public static String keyForPropertyBinding(String blkid,String pname) {
		return String.format("B:%s:%s",blkid,pname);
	}
	/**
	 * Test a key for type. 
	 * @return  true if this is a binding key.
	 */
	public static boolean isDiagramAlertKey(String key) {
		return key.startsWith("A");
	}
	/**
	 * Test a key for type. 
	 * @return  true if this is a binding key.
	 */
	public static boolean isPropertyBindingKey(String key) {
		return key.startsWith("B");
	}
	/**
	 * Test a key for type. 
	 * @return  true if this is a value key.
	 */
	public static boolean isPropertyValueKey(String key) {
		return key.startsWith("P");
	}
	/**
	 * Test a key for type. 
	 * @return  true if this is a watermark key.
	 */
	public static boolean isWatermarkKey(String key) {
		return key.startsWith("W");
	}
	/**
	 * @return  a push-notification key for a diagram's watermark. The Id is
	 *         simply the diagram Id.
	 */
	public static String watermarkKeyForDiagram(String did) {
		return String.format("W:%s",did);
	}
}
