/**
 *   (c) 2016  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class describes the possible options for slope calculation
 */
public enum SlopeCalculationOption
{
            AVERAGE,
            LINEARFIT
            ;
           
 /**
  * @return  a comma-separated list of all options in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (SlopeCalculationOption type : SlopeCalculationOption.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
