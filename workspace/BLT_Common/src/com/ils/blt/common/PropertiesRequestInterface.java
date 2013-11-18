/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.common;

/**
 *  This is the scripting interface describes the methods supplied by a handler
 *  that brings external class information to the JGraphX model.
 */
public interface PropertiesRequestInterface  {

	/**
	 * Obtain a list of attribute-value pairs for the class represented by this block.
	 * On the Python side, a class instance is created if it does not already exist.
	 * In the case where a class instance is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param key a string representing the id of the cell within the project.
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the block object.
	 */
	public String getBlockAttributes(String key,String json);

	/**
	 * Obtain a list of attribute-value pairs for the class represented by this connection.
	 * On the Python side, a class instance is created if it does not already exist.
	 * In the case where a class instance is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param key a string representing the id of the cell within the project.
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the connection.
	 */
	public String getConnectionAttributes(String key,String json); 
}