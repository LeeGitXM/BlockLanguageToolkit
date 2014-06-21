/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * A placement hint is a non-binding directive to the UI
 * for a block, telling it where to locate the anchor point. 
 */
public enum PlacementHint
{
			R,
            L,
            T,
            B,
            UNSPECIFIED
            ;
           
 /**
  * @return  a comma-separated list of all hints in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (PlacementHint type : PlacementHint.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
 
}
