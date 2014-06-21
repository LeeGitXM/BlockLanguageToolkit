/**
 *   (c) 2013-2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.block;


/**
 * This enumeration class represents permissible data types for a block property.
 * A number of these types have fixed vocabularies and are edited via choose boxes.
 */
public enum PropertyType
{
            STRING,
            DOUBLE,
            INTEGER,
            BOOLEAN,
            HTML,                // Gets an html editor
            DISTRIBUTION,
            LIMIT,               // Limit type (UPPER,LOWER,BOTH)
            SCOPE,               // Transmission scope
            TRUTHVALUE,
            LIST,                // List of strings, char used to delimit is first value
            SCRIPT,              // Gets a Python editor
            OBJECT               // Untyped primitive
            ;
           
 /**
  * @return  a comma-separated list of all attribute types in a single String.
  */
  public static String names()
  {
    StringBuffer names = new StringBuffer();
    for (PropertyType type : PropertyType.values())
    {
      names.append(type.name()+", ");
    }
    return names.substring(0, names.length()-2);
  }
}
