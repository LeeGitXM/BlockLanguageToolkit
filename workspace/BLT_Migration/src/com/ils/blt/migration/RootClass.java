/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.migration;


/**
 * This enumeration class holds options describing the class at the root of an import.
 * for the noise generator. 
 */
public enum RootClass
{
            APPLICATION,
            DIAGRAM
            ;
           
 /**
  * @return  a comma-separated list of all distributions in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (RootClass type : RootClass.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
