/**
 *   (c) 2014-2016  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;



/**
 * This enumeration class describes the scopes available for a transmitter
 */
public enum TransmissionScope
{
            GLOBAL,
            APPLICATION,
            FAMILY,
            LOCAL,
            BLOCK
            ;
           
 /**
  * @return  a comma-separated list of all scopes in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (BindingType type : BindingType.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
