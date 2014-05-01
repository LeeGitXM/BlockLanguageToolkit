/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.serializable;


/**
 * This enumeration class represents the permissible states of a diagram.
 * The DIRTY_DISABLED state implies that the block is both dirty and
 * disabled. A "save" when in this state returns it to "DISABLED" rather 
 * than "ACTIVE" 
 */
public enum DiagramState
{
            ACTIVE,
            DIRTY,
            DIRTY_DISABLED,
            DIRTY_TEST,
            DISABLED,
            TEST,
            UNKNOWN
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
