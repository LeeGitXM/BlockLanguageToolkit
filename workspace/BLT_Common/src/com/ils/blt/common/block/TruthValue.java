/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class represents the legal states for a truth-value. 
 * An instance of this class is passed along a connector of type "TRUTH-VALUE". 
 */
public enum TruthValue
{
			UNSET,
            FALSE,
            TRUE,
            UNKNOWN
            ;
           
 /**
  * @return  a comma-separated list of all truth states in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (TruthValue type : TruthValue.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
 
}
