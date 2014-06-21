/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.serializable;


/**
 * This enumeration class represents the permissible states of a diagram.
 */
public enum DiagramState
{
            ACTIVE,
            DISABLED,
            RESTRICTED
            ;
          
 /**
  * @return  a comma-separated list of all block states in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (DiagramState type : DiagramState.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
