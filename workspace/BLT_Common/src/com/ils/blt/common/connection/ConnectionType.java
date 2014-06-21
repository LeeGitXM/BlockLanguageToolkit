/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.connection;

import com.ils.blt.common.block.PropertyType;



/**
 * This enumeration class represents the permissible types of connections.
 */
public enum ConnectionType
{
	ANY,
	DATA,
	TEXT,
	SIGNAL,
	TRUTHVALUE
	;

	/**
	 * @return  a ConnectionType appropriate for the given property type.
	 */
	public static ConnectionType connectionTypeForPropertyType(PropertyType propertyType) {
		ConnectionType ctype = ConnectionType.ANY;
		if( propertyType==PropertyType.DOUBLE ||
		    propertyType==PropertyType.INTEGER  )    ctype = ConnectionType.DATA;
		else if( propertyType==PropertyType.BOOLEAN )ctype = ConnectionType.TRUTHVALUE;
		else if( propertyType==PropertyType.STRING ) ctype = ConnectionType.TEXT;
		return ctype;

	}
	/**
	 * @return  a comma-separated list of all Connection Types in a single String.
	 */
	public static String names() {
		StringBuffer names = new StringBuffer();
		for (ConnectionType type : ConnectionType.values())
		{
			names.append(type.name()+", ");
		}
		return names.substring(0, names.length()-2);
	}
}
