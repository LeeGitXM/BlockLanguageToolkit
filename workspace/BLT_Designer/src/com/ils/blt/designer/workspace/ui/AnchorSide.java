/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.designer.workspace.ui;


/**
 * This enumeration class represents permissible positions of an anchor with respect to its view 
 */
public enum AnchorSide
{
            TOP,
            RIGHT,
            BOTTOM,
            LEFT
            ;
           
 /**
  * @return  a comma-separated list of all anchor positions in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (AnchorSide type : AnchorSide.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
