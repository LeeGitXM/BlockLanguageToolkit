/**
 *   (c) 2012  ILS Automation. All rights reserved. 
 */
package com.ils.diagnostics.common;


/**
 * This enumeration class represents the permissible states of execution
 * blocks. 
 */
public enum BlockState
{
            ABORTED,
            ABORTING,
            COMPLETE,
            HELD,
            IDLE,
            PAUSED,
            PAUSING,
            STOPPED,
            STOPPING,
            RESTARTING,
            RUNNING,
            ;
           
 /**
  * @return  a comma-separated list of all Service Types in a single String.
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
