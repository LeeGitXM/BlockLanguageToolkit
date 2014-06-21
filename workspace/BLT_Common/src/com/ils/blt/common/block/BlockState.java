/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class represents the permissible states of execution
 * blocks. Note: the "locked" state is independent of these.
 */
public enum BlockState
{
            ACTIVE,
            ERROR,
            INITIALIZED,
            INHIBITED
            ;
           
 /**
  * @return  a comma-separated list of all block states in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (BlockState type : BlockState.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
