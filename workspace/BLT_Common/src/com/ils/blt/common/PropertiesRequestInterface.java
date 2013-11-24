/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.common;

/**
 *  This scripting interface describes methods that bring block attribute 
 *  information to entities outside the Gateway scope.
 *  
 *  In every case the returned information is constructed in JSON as
 *  a dictionary of dictionaries keyed by a string. The embedded dictionaries
 *  themselves have a string key and a string value.
 */
public interface PropertiesRequestInterface  {

	/**
	 * Obtain a keyed-list of attribute-value pairs for the block identified by the specified.
	 * key. On the Gateway side, a block instance is created if it does not already exist.
	 * In the case where a block instance is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param key a string representing the id of the cell within the project.
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the block object.
	 */
	public String getBlockAttributes(String key,String json);

	/**
	 * Obtain a keyed list of attribute-value pairs for the connection represented by the supplied.
	 * key. On the Gateway side, a connection instance is created if it does not already exist.
	 * In the case where a connection is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param key a string representing the id of the cell within the project.
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the connection.
	 */
	public String getConnectionAttributes(String key,String json);
	/**
	 * Obtain a list of keyed attribute-value pairs (that is a list of dictionaries) containing
	 * default values of attributes for every executable block class. These attributes may include
	 * static elements as well.
	 * 
	 * @return a string representing a JSON document containing a list of keyed attribute dictionaries
	 * 								 corresponding to all executable block classes.
	 */
	public String getPaletteBlockAttributes();
	/**
	 * Obtain a list of keyed attribute-value pairs (that is a list of dictionaries) containing
	 * default values of attributes for every connection type.
	 * 
	 * @return a string representing a JSON document containing a list of keyed attribute dictionaries
	 * 								 corresponding to all connection classes.
	 */
	public String getPaletteConnectionAttributes();
}