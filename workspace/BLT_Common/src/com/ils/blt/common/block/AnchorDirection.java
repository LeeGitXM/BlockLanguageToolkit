/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class is a substitute for AnchorType which is not
 * available in the Gateway. It specifies the direction of a connection
 * at an anchor point. 
 */
public enum AnchorDirection
{
            INCOMING,
            OUTGOING
            ;
           
 /**
  * @return  a comma-separated list of all directions in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (AnchorDirection type : AnchorDirection.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
