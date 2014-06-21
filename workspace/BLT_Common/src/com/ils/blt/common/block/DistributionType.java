/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class represents the available statistical distributions
 * for the noise generator. 
 */
public enum DistributionType
{
            EXPONENTIAL,
            NORMAL,
            UNIFORM
            ;
           
 /**
  * @return  a comma-separated list of all distributions in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (DistributionType type : DistributionType.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
