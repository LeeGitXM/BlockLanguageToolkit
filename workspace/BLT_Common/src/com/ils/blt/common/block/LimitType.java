/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class describes the possible vlues for a limit type
 */
public enum LimitType
{
            NONE,
            HIGH,
            LOW,
            BOTH,
            CONSECUTIVE
            ;
           
 /**
  * @return  a comma-separated list of all limit types in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (LimitType type : LimitType.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
