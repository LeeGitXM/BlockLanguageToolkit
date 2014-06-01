/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *   Based on sample code in the IA-scripting-module
 *   by Travis Cox.
 */
package com.ils.blt.test.common;


import java.util.Date;


/**
 *  Define a scripting interface that allows the creation and use of SQLTag
 *  providers. Note that providers and the tags they control do not persist 
 *  across Gateway restarts.
 */
public interface TagProviderScriptingInterface   {

	/**
	 * Create a simple tag provider for SQLTags only. These tags are not associated with an OPC server.
	 * Once the provider exists, tags may be created against it.
	 * 
	 * @param name, provider name. This is the name that appears in the [..] portion of a complete tag path.
	 */
	public void createProvider(String name);
	/**
	 * Create an expression tag. Specify its provider, path and data type. We expect the provider to be one
	 * of the providers created through this scripting interface.
	 * 
	 * @param provider - name of the tag provider
	 * @param tagPath - path relative to the provider (source)
	 * @param dataType, valid values are "Int1", "Int2", "Int4", "Int8", "Float4", "Float8", "Boolean", "String", "DateTime", "DataSet"
	 * @param expr, the expression
	 */
	public void createExpression(String provider, String tagPath, String dataType,String expr);
	
	/**
	 * Create a tag. Specify its provider, path and data type. We expect the provider to be one
	 * of the providers created through this scripting interface.
	 * 
	 * @param provider - name of the tag provider
	 * @param tagPath - path relative to the provider (source)
	 * @param dataType, valid values are "Int1", "Int2", "Int4", "Int8", "Float4", "Float8", "Boolean", "String", "DateTime", "DataSet"
	 */
	public void createTag(String provider, String tagPath, String dataType);

	/**
	 * Remove a SQLTag given its path. This will succeed only if the tag is "owned"
	 * by the provider that is specified.
	 * 
	 * @param provider - name of the tag provider
	 * @param tagPath - path relative to the provider
	 */
	public void deleteTag(String provider,String tagPath);
	
	/**
	 * Shutdown the named provider. All tags associated with that provider
	 * will (should) disappear.
	 * 
	 * @param name, provider name. This is the name that appears in the [..] portion of a complete tag path.
	 */
	public void removeProvider(String name);
	
	/**
	 * Update the expression of an expression tag. The provider is expected to be one
	 * of the providers configured through this interface.
	 * 
	 * @param provider.
	 * @param path
	 * @param expr, the expression
	 */
	public void updateExpression(String provider, String tagPath, String expr);
	
	/**
	 * Update the value of a given tag. The value will be converted appropriately
	 * according to the datatype of the tag. The provider is expected to be one
	 * of the providers configured through this interface.
	 * 
	 * @param provider.
	 * @param path
	 * @param value
	 * @param timestamp, the time at which the value applies
	 */
	public void updateTag(String provider, String tagPath, String value, Date timestamp);
}
	
