/**
 *   (c) 2016  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class describes the possible trend directions
 */
public enum TrendDirection
{
            DOWNWARD,
            UPWARD,
            BOTH
            ;
           
 /**
  * @return  a comma-separated list of all directions in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (TrendDirection type : TrendDirection.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
