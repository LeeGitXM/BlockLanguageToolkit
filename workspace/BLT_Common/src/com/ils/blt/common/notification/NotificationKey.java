/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.notification;


/**
 * This class contains static methods for generating keys for the
 * various notification target types. 
 */
public class NotificationKey
{
 /**
  * @return  a push-notification key for a block.
  */
  public static String keyForBlock(String blkid) {
	  return String.format("B:%s",blkid);
  }
  /**
   * @return  a push-notification key for a connection. The Id is
   *         the connection Id, even though the listener is probably
   *         an anchor point.
   */
   public static String keyForConnection(String cxnid) {
 	  return String.format("C:%s",cxnid);
   }
   /**
    * @return  a push-notification key for a block property. The Id is
    *         the block Id followed by the property name.
    */
    public static String keyForProperty(String blkid,String pname) {
  	  return String.format("P:%s:%s",blkid,pname);
    }
 
}
