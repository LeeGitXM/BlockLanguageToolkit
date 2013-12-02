/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.common;

/**
 *  This scripting interface describes methods available in the Designer scope 
 *  for discovering and editing properties of diagrams, blocks, and connections.
 *  
 *  Typically, the returned information is constructed in JSON as
 *  a dictionary of dictionaries keyed by a string. The embedded dictionaries
 *  themselves have a string key and a string value.
 */
public interface BlockPropertiesInterface  {
	/**
	 * Enable or disable a diagram.
	 * 
	 * @param projectId of the project to which the diagram belongs
	 * @param resourceId of the model resource for this diagram
	 * @param flag true to enable the diagram
	 */
	public void enableDiagram(long projectId,long resourceId,boolean flag);
	
	/**
	 * Obtain a keyed-list of attribute-value pairs for the block identified by the specified.
	 * key. On the Gateway side, a block instance is created if it does not already exist.
	 * In the case where a block instance is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param projectId of the project to which the diagram belongs
	 * @param resourceId of the model resource for this diagram
	 * @param blockId the identifier of a block within the diagram
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the block object.
	 */
	public String getBlockAttributes(long projectId,long resourceId,String blockId,String json);

	/**
	 * Obtain a keyed list of attribute-value pairs for the connection represented by the supplied.
	 * key. On the Gateway side, a connection instance is created if it does not already exist.
	 * In the case where a connection is created, the attribute values will be filled 
	 * with appropriate defaults.
	 * 
	 * @param projectId of the project to which the diagram belongs
	 * @param resourceId of the model resource for this diagram.
	 * @param connectionId the identifier of the connection within the diagram
	 * @param json string representing an array of attributes
	 * @return a string representing a JSON document containing an array of attributes corresponding
	 *         to the connection.
	 */
	public String getConnectionAttributes(long projectId,long resourceId,String connectionId,String json);
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
	 * default values of attributes for every connection type. The dictionary elements are defined
	 * in BlockProperties as: class, icon (a resource path), palette-name (name as seen on palette),
	 * shape (needed to paint object in diagram)
	 * 
	 * @return a string representing a JSON document containing a list of keyed attribute dictionaries
	 * 								 one for each connection class.
	 */
	public String getPaletteConnectionAttributes();
}