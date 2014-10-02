/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class represents the permissible states of an Application
 * or Family
 */
public enum ActiveState
{
            ACTIVE,
            INACTIVE
            ;
           
 /**
  * @return  a comma-separated list of active states in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (ActiveState type : ActiveState.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
