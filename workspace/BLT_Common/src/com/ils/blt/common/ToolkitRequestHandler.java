/**
 *   (c) 2014-2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;

/**
 *  This interface is a common point for managing requests to the gateway dealing with
 *  the execution engine and block status. It is designed for use by Java code in the
 *  designer as well as Python scripting. It provides a way to request/set properties 
 *   of diagrams, blocks and connections.
 *  
 *  Each request is relayed to the Gateway scope via an RPC call.
 */
public interface ToolkitRequestHandler  {
	
	/**
	 * @param nodeId identifier of the parent node.
	 * @return a list of resources that are children of the specified resource
	 */
	public List<SerializableResourceDescriptor> childNodes(String nodeId);
	/**
	 * Remove all current diagrams from the controller.
	 */
	public void clearController();
	/**
	 * Create a resource descriptor from its string components
	 * @param project name
	 * @param resource path
	 * @param resource type
	 */
	public ProjectResourceId createResourceId(String projectName,String path, String type);
	/**
	 * Create a SQLTag memory tag given its path and data type. The path must contain the
	 * provider name in brackets.
	 */
	public void createTag(DataType type,String path);
	/**
	 * Delete a SQLTag given its path. The path must contain the
	 * provider name in brackets.
	 */
	public void deleteTag(String path);
	/**
	 * Determine whether or not the indicated diagram is known to the controller.
	 * @param diagramId string representation of the diagram's unique id
	 * @return true if the referenced diagram exists in the gateway
	 */
	public boolean diagramExists(ProjectResourceId diagramId) ;

	/**
	 * @param resourceId string representation of the application's unique id
	 * @return the name of the application that is equal to or
	 *         superior to the node with the specified UUID
	 */
	public String getApplicationName(ProjectResourceId resourceId);
	
	/**
	 * Obtain a list of BlockProperty objects for the specified block. 
	 * CAUTION: If the block is not known to the gateway it will be created.
	 *          This method is for internal use in the designer.
	 * 
	 * @param className class name of the block
	 * @param resId identifier of the resource that is the diagram holding the block
	 * @param blockId UUID of the block
	 *
	 * @return an array of block properties for the subject block
	 */
	public List<BlockProperty> getBlockProperties(String className,ProjectResourceId resid,UUID blockId) ;
	
	/**
	 * Obtain a list of prototypes suitable for display on the block palette
	 *
	 * @return an array of PalleteProtypes
	 */
	public List<PalettePrototype> getBlockPrototypes() ;
	
	/**
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockName name of the block within the diagram
	 * @return the id of the specified block.
	 */
	public String getBlockId(String diagramId, String blockName) ;
	
	/**
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockName name of the block within the diagram
	 * @return the current state of the specified block.
	 */
	public String getBlockState(String diagramId, String blockName) ;

	/**
	 * Determine whether or not the engine is running.
	 * @return the current controller state as a String
	 */
	public String getControllerState() ;
	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * database.  
	 * @param uuid the uniqueId (string) of any node in the nav tree.
	 * @return database name
	 */
	public String getDatabaseForId(ProjectResourceId id);
	
	/**
	 * It appears that there is no way in the client to obtain a list of data sources
	 * (database connection names). Consequently, we implement our own. 
	 * 
	 * @return a list of data sources configured and enabled in the gateway.
	 */
	public List<String> getDatasourceNames();
	/**
	 * @param diagramId String representation of the diagram's internal Id.
	 * @return a descriptor for the diagram that corresponds to that Id.
	 */
	public SerializableResourceDescriptor getDiagram(ProjectResourceId diagramId) ;
	
	/**
	 * @param blockId String representation of the block's internal Id.
	 * @return the diagram that is a parent of the specified block.
	 */
	public SerializableResourceDescriptor getDiagramForBlock(String blockId) ;
	/**
	 * @param id identifier of the resource that is the diagram holding the block
	 * @return the current state of the specified diagram.
	 */
	public DiagramState getDiagramState(ProjectResourceId id) ;
	
	/**
	 * @param uuid identifier of any node as a string
	 * @return the name of the family that is equal to or
	 *         superior to the node with the specified UUID
	 */
	public String getFamilyName(ProjectResourceId uuid);
	/**
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockId identifier of the block within the diagram
	 * @return an explanation for the state of a block.
	 */
	public String getExplanation(ProjectResourceId diagramId,String blockId);
	
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @return internal details of a block for debugging purposes.
	 */
	public SerializableBlockStateDescriptor getInternalState(String diagramId,String blockId) ;
	/**
	 * Find the name of the isolation datasource from the internal SQLite database. 
	 * @return isolation database name
	 */
	public String getIsolationDatabase();
	/**
	 * Find the name of the isolation tag provider from the internal SQLite database. 
	 * @return isolation tag provider name
	 */
	public String getIsolationTagProvider();
	/**
	 * Find the name of the production datasource from the internal SQLite database. 
	 * @return production database name
	 */
	public String getProductionDatabase();
	/**
	 * Find the name of the isolation tag provider from the internal SQLite database. 
	 * @return production tag provider name
	 */
	public String getProductionTagProvider();
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the binding associated with a specified block property. If there is no 
	 *         binding, an empty string is returned.
	 */
	public Object getPropertyBinding(ProjectResourceId diagramId,String blockId,String propertyName) ;
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of a specified block property.
	 */
	public Object getPropertyValue(ProjectResourceId diagramId,String blockId,String propertyName) ;
	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * tag provider.  
	 * @param uuid the uniqueId (string) of any node in the nav tree.
	 * @return tag provider name
	 */
	public String getProviderForUUID(ProjectResourceId uuid);
	/**
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockName name of the block within the diagram
	 * @return the time at which the block last changed its state
	 */
	public Date getTimeOfLastBlockStateChange(ProjectResourceId diagramId, String blockName) ;
	/**
	 * Acquire a value from the HSQL database table associated with the toolkit. A
	 * null is returned if the string is not found.
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of the specified property.
	 */
	public String getToolkitProperty(String propertyName) ;
	/**
	 * Retrieve the configured browser path from the ORM database HelpRecord. This is used for 
	 * context-sensitive help.
	 * @return the configured browser path (for Windows)
	 */
	public String getWindowsBrowserPath();
	/**
	 * @param id the diagram expressed as a resource
	 * @return the alert state of the specified diagram;
	 */
	public boolean isAlerting(ProjectResourceId id) ;
	
	/**
	 * @return whether or not the engine is running
	 */
	public boolean isControllerRunning() ;
	/**
	 * Query a diagram in the gateway for list of its blocks that are downstream
	 * of the specified block. 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks downstream of the specified block.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(ProjectResourceId diagramId,String blockName);
	/**
	 * List all blocks that have properties bound to the supplied tag path. 
	 * @param tagpath the path for the tag of interest.
	 * @return a list of blocks associated with the tag.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksForTag(String tagpath) ;

	/**
	 * Query a block in the gateway for list of the blocks connected to the named port. 
	 * @param diagramId of the parent diagram
	 * @param blockId identifier of the block
	 * @param portName port of the anchor of interest
	 * @return a list of blocks connected to the named port.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(ProjectResourceId diagramId,String blockId,String portName) ;
	/**
	 * Query a diagram in the gateway for list of its blocks. 
	 * @param diagramId of the parent diagram
	 * @return a list of blocks belonging to the diagram.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksInDiagram(ProjectResourceId diagramId) ;
	/**
	 * Query a diagram in the gateway for list of its blocks that are downstream
	 * of the specified block. If any of those blocks are sinks, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks downstream of the specified block.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksGloballyDownstreamOf(ProjectResourceId diagramId,String blockName); 
	/**
	 * Query a diagram in the gateway for list of its blocks that are upstream
	 * of the specified block. If any of those blocks are sources, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks upstream of the specified block.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksGloballyUpstreamOf(ProjectResourceId diagramId,String blockName); 
	/**
	 * @param className fully qualified class name of blocks to be listed
	 * @return a list of state descriptors for blocks that are of the specified class.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksOfClass(String className);
	/**
	 * Query a diagram in the gateway for list of its blocks that are upstream
	 * of the specified block. 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks upstream of the specified block..
	 */
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(ProjectResourceId diagramId,String blockName); 
	/**
	 * The result is a list of SerializableBlockState descriptors for those 
	 * blocks in any project that have configuration issues. Descriptor attributes
	 * contain at least the project and a path to the block. The descriptor 
	 * contains a textual description of whatever problem is detected.
	 * @return a list of blocks that have incomplete or incorrect configuration.
	 */
	public List<SerializableBlockStateDescriptor> listConfigurationErrors() ;
	/**
	 * The result is a list of SerializableBlockState descriptors that correspond 
	 * to blocks in any project that contain properties bound to tags where those property
	 * values do not match the current tag values. The descriptor 
	 * contains a textual description of the property that is not responding.
	 * @return a list of blocks that have subscription errors.
	 */
	public List<SerializableBlockStateDescriptor> listSubscriptionErrors() ;
	/**
	 * The result is a list of SerializableBlockState descriptors corresponding 
	 * to blocks in any project that have not changed values within the specified 
	 * time. 
	 * @param hours the number of hours that the block must have remained unchanged
	 *        in order to report an issue
	 * @param className the category of blocks for which the request is to apply. 
	 *        Null or empty implies all classes.
	 * @return a list of blocks that have incomplete or incorrect configuration.
	 */
	public List<SerializableBlockStateDescriptor> listUnresponsiveBlocks(double hours, String className) ;
	/**
	 * @param diagramId identifier of the diagram to be queried, a String
	 * @param className fully qualified class name of blocks to be listed
	 * @return a list of block descriptors for blocks owned by a specified diagram that
	 *         are of a specified class.
	 */
	public List<SerializableBlockStateDescriptor> listDiagramBlocksOfClass(String diagramId,String className);
	
	/**
	 * Query the gateway for list of diagrams belonging to a project. 
	 * 
	 * @param projectName name of the project containing the diagrams
	 * @return a list of tree-paths to the diagrams saved (ie. known to the Gateway).
	 */
	public List<SerializableResourceDescriptor> listDiagramDescriptors(String projectName) ;
	
	/**
	 * Query the gateway for list of resource nodes that the block controller
	 * knows about. This should correspond to what is displayed in the designer
	 * nav tree for all loaded projects. 
	 * 
	 * @return a list of resources known to the BlockController.
	 */
	public List<SerializableResourceDescriptor> listResourceNodes();
	/**
	 * Query the gateway for list of its sink blocks associated with the
	 * specified source. The blocks that are returned are not constrained
	 * to be part of the same diagram, family or application.
	 * @param diagramId of the parent diagram
	 * @param blockId Id of the source block
	 * @return a list of blocks logically connected to the source.
	 */
	public List<SerializableBlockStateDescriptor> listSinksForSource(ProjectResourceId diagramId,String blockId) ;
	/**
	 * Query the gateway for list of its source blocks associated with the
	 * specified sink. The blocks that are returned are not constrained
	 * to be part of the same diagram, family or application.
	 * @param diagramId of the parent diagram
	 * @param blockId Id of the sink block  
	 * @return a list of blocks logically connected to the sink.
	 */
	public List<SerializableBlockStateDescriptor> listSourcesForSink(ProjectResourceId diagramId,String blockId) ;
	/** 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a colon-separated path to the specified block. The path includes
	 *         the project name.
	 */
	public String pathForBlock(ProjectResourceId diagramId,String blockName);
	/** 
	 * @param nodeId UUID as a String of a node in the navigation tree
	 * @return a slash-separated path to the specified node. The path 
	 *         root is a slash representing the top node of the navigation tree.
	 */
	public String pathForNode(ProjectResourceId nodeId);
	/**
	 * Execute propagate() on a specified block. Send its current state
	 * to the outputs.
	 * @param diagramId id of the parent diagram
	 * @param blockId id of the subject block as a string
	 */
	public void propagateBlockState(ProjectResourceId diagramId,String blockId) ;
	/**
	 * Execute the getAux extension function in Gateway scope for the indicated node.
	 * Inform the designer of the results via a notification
	 * @param resid the resourceId of an application to be refreshed
	 * @param nodeId identifier of the node to be 
	 * @param provider tag provider
	 * @param db datasource
	 * @return the auxiliary data as stored in the database
	 */
	public GeneralPurposeDataContainer readAuxData(ProjectResourceId resid,String nodeId,String provider,String db);
	/** Update a single property for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 * @param name the new name
	 */
	public void renameBlock(ProjectResourceId duuid,String buuid,String name ) ;
	/**
	 * Rename a SQLTag given its path and new name. The path must contain the
	 * provider name in brackets.
	 */
	public void renameTag(String name,String path);
	/**
	 * Execute reset() on a specified block
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram 
	 */
	public void resetBlock(ProjectResourceId diagramId,String blockName) ;

	/**
	 * Execute reset() on every block on the diagram
	 *  @param diagramId id of the parent diagram
	 */
	public void resetDiagram(ProjectResourceId diagramId) ;
	/**
	 * Determine whether or not the indicated resource is known to the controller.
	 * @param resid identifier of the resource that is the diagram holding the block, a Long
	 * @return true if the resource is found
	 */
	public boolean resourceExists(ProjectResourceId resid) ;
	/**
	 * Execute stop() then start() on a specified block
	 * @param diagramId id of the parent diagram
	 * @param blockName name of the block within the diagram 
	 */
	public void restartBlock(ProjectResourceId diagramId,String blockName) ;
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a tree-path.
	 * There may be no successful recipients.
	 * 
	 * @param diagramId id of the parent diagram
	 * @param command string of the signal
	 * @param message embedded in the transmitted signal
	 * @param arg also a component of the transmitted signal
	 * @return true if the signal was sent
	 */
	public boolean sendLocalSignal(ProjectResourceId diagramId,String command,String message,String arg) ;
	/**
	 * Send a signal directly to a specified block.
	 * This is a "local" transmission. The signal timestamp is "now".
	 * 
	 * @param diagramId diagram identifier
	 * @param blockName name of the target block
	 * @param command string of the signal.
	 * @param message command payload
	 * @return true on success
	 */
	public boolean sendSignal(ProjectResourceId diagramId,String blockName,String command,String message);
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a tree-path.
	 * There may be no successful recipients. This version time-stamps the signal sent
	 * 
	 * @param diagramId diagram identifier as a string
	 * @param command string of the signal
	 * @param message embedded in the transmitted signal
	 * @param arg also a component of the transmitted signal
	 * @param time unix time, milli-seconds since the start of the epoch
	 * @return true if the signal was sent 
	 */
	public boolean sendTimestampedSignal(ProjectResourceId diagramId,String command,String message,String arg,long time) ;
	
	/**
	 * Set the state of every diagram that is a member of the application to
	 * the specified value.
	 * @param appname name of the application
	 * @param state new state for all diagrams in application
	 */
	public void setApplicationState(String appname, String state);

	/** Update all changed properties for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 * @param props a collection of properties with changes
	 */
	public void setBlockProperties(ProjectResourceId duuid,UUID buuid, Collection<BlockProperty> props ) ;
	/** Update a single property for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 * @param property the changed property
	 */
	public void setBlockProperty(ProjectResourceId duuid,UUID buuid,BlockProperty property ) ;
	
	/** Change the binding on a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param blockId Id of the block as a String
	 * @param prop the changed property
	 * @param value the new binding of the property. The value must be a legal tag path 
	 */
	public void setBlockPropertyBinding(ProjectResourceId diagramId,String blockId,String prop,String value ) ;
	
	/** Change the value of a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname name of the block
	 * @param pname the changed property
	 * @param value the new value of the property. The value will be coerced into the correct data type in the gateway 
	 */
	public void setBlockPropertyValue(ProjectResourceId diagramId,String bname,String pname,String value ) ;

	/** 
	 * Drive a block to the specified state. 
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname block name
	 * @param state the new state of the block. The value will be coerced into a truth-value in the gateway 
	 */
	public void setBlockState(ProjectResourceId diagramId,String bname,String state );
	public void setDiagramState(ProjectResourceId resid, String state) ;
	
	/**
	 * Save a value into the HSQL database table associated with the toolkit. The 
	 * table contains name-value pairs, so any name is allowable.
	 * @param propertyName name of the property for which a value is to be set
	 * @param value the new value of the property.
	 */
	public void setToolkitProperty(String propertyName,String value) ;

	/**
	 * Start the block execution engine in the gateway.
	 */
	public void startController() ;
	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	public void stopController() ;
	/**
	 * Direct the blocks in a specified diagram to report their
	 * status values. This is in order to update the UI. 
	 */
	public void triggerStatusNotifications() ;
	
	/** Update connections for a block. New connections will be added, old connections
	 * may undergo a type conversion.  
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 * @param anchors  a pre-provided list of anchors that will be augmented by this call
	 */
	public void updateBlockAnchors(ProjectResourceId duuid,UUID buuid, Collection<SerializableAnchor> anchors ) ;
	/**
	 * Execute the setAux extension function in Gateway scope for the indicated node
	 * @param resid the resourceId of an application to be refreshed
	 * @param nodeId identifier of node or block affected.
	 * @param container the new auxiliary data
	 * @param provider tag provider
	 * @param db datasource
	 */
	public void writeAuxData(ProjectResourceId resid,String nodeId,GeneralPurposeDataContainer container,String provider,String db);

}
