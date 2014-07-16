/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class represents the available rendering options for 
 * a block in a diagram in the designer. 
 */
public enum BlockStyle
{
			ARROW,
			CLAMP,
            DIAMOND,
            LOGIC_AND,
            LOGIC_NOT,
            LOGIC_NTRUE,
            LOGIC_OR,
            ICON,
            JUNCTION,
            READOUT,
            ROUND,
            SQUARE,
            NOTE
            ;
           
 /**
  * @return  a comma-separated list of all block styles in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (BlockStyle type : BlockStyle.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
