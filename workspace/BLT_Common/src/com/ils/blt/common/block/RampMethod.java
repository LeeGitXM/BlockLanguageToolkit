/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class represents the permissible group ramp methods
 * or Family
 */
public enum RampMethod
{
            SHORTEST,
            LONGEST,
            NONE
            ;
           
 /**
  * @return  a comma-separated list of methods in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (RampMethod type : RampMethod.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
