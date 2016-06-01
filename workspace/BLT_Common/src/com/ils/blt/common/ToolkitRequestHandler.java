/**
 *   (c) 2014  ILS Automation. All rights reserved.
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
	 * @return a list of resources that are children of the specified resource
	 */
	public List<SerializableResourceDescriptor> childNodes(String nodeId);
	/**
	 * Remove all current diagrams from the controller.
	 */
	public void clearController();
	/**
	 * Determine whether or not the indicated diagram is known to the controller.
	 * @param diagramId string representation of the diagram's unique id
	 */
	public boolean diagramExists(String diagramId) ;

	/**
	 * Execute evaluate() on a specified block
	 */
	public void evaluateBlock(String diagramId,String blockId) ;
	/**
	 * @param uuid string representation of the application's unique id
	 * @return the name of the application that is equal to or
	 *         superior to the node with the specified UUID
	 */
	public String getApplicationName(String uuid);
	
	
	/**
	 * Obtain a list of BlockProperty objects for the specified block. 
	 * CAUTION: If the block is not known to the gateway it will be created.
	 *          This method is for internal use in the designer.
	 * 
	 * @param className class name of the block
	 * @param projectId
	 * @param resourceId
	 * @param blockId
	 *
	 * @return an array of block properties for the subject block
	 */
	public List<BlockProperty> getBlockProperties(String className,long projectId,long resourceId,UUID blockId) ;
	
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
	 */
	public String getControllerState() ;
	/**
	 * Find the parent application or diagram of the entity referenced by
	 * the supplied id. Test the state and return the name of the appropriate
	 * database.  
	 * @param uuid the uniqueId (string) of any node in the nav tree.
	 * @return database name
	 */
	public String getDatabaseForUUID(String uuid);
	
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
	public SerializableResourceDescriptor getDiagram(String diagramId) ;
	
	/**
	 * blockId String representation of the block's internal Id.
	 * @return the diagram that is a parent of the specified block.
	 */
	public SerializableResourceDescriptor getDiagramForBlock(String blockId) ;
	/**
	 * @return the current state of the specified diagram.
	 */
	public DiagramState getDiagramState(Long projectId, Long resourceId) ;
	public DiagramState getDiagramState(String diagramId);
	
	/**
	 * @return the name of the family that is equal to or
	 *         superior to the node with the specified UUID
	 */
	public String getFamilyName(String uuid);
	/**
	 * @return an explanation for the state of a block.
	 */
	public String getExplanation(String diagramId,String blockId);
	
	/**
	 * @return internal details of a block for debugging purposes.
	 */
	public SerializableBlockStateDescriptor getInternalState(String diagramId,String blockId) ;
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the binding associated with a specified block property. If there is no 
	 *         binding, an empty string is returned.
	 */
	public Object getPropertyBinding(String diagramId,String blockId,String propertyName) ;
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of a specified block property.
	 */
	public Object getPropertyValue(String diagramId,String blockId,String propertyName) ;
	/**
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockName name of the block within the diagram
	 * @return the time at which the block last changed its state
	 */
	public Date getTimeOfLastBlockStateChange(String diagramId, String blockName) ;
	/**
	 * Acquire a value from the HSQL database table associated with the toolkit. A
	 * null is returned if the string is not found.
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of the specified property.
	 */
	public String getToolkitProperty(String propertyName) ;
	/**
	 * @return the alert state of the specified diagram;
	 */
	public boolean isAlerting(Long projectId, Long resourceId) ;
	
	/**
	 * Determine whether or not the engine is running.
	 */
	public boolean isControllerRunning() ;
	/**
	 * Query a diagram in the gateway for list of its blocks that are downstream
	 * of the specified block. 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks downstream of the specified block.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(String diagramId,String blockName);
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
	 * @param port name of the anchor of interest
	 * @return a list of blocks connected to the named port.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(String diagramId,String blockId,String portName) ;
	/**
	 * Query a diagram in the gateway for list of its blocks. 
	 * @param diagramId of the parent diagram
	 * @return a list of blocks belonging to the diagram.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksInDiagram(String diagramId) ;
	/**
	 * Query a diagram in the gateway for list of its blocks that are downstream
	 * of the specified block. If any of those blocks are sinks, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks downstream of the specified block.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksGloballyDownstreamOf(String diagramId,String blockName); 
	/**
	 * Query a diagram in the gateway for list of its blocks that are upstream
	 * of the specified block. If any of those blocks are sources, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks upstream of the specified block.
	 */
	public List<SerializableBlockStateDescriptor> listBlocksGloballyUpstreamOf(String diagramId,String blockName); 
	/**
	 * Query a diagram in the gateway for list of its blocks that are upstream
	 * of the specified block. 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks upstream of the specified block..
	 */
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(String diagramId,String blockName); 
	/**
	 * The result is a list of SerializableBlockState descriptors for those 
	 * blocks in any project that have configuration issues. Descriptor attributes
	 * contain at least the project and a path to the block. The descriptor 
	 * contains a textual description of whatever problem is detected.
	 * @return a list of blocks that have incomplete or incorrect configuration.
	 */
	public List<SerializableBlockStateDescriptor> listConfigurationErrors() ;
	
	/**
	 * @param diagramId identifier of the diagram to be queried, a String
	 * @param className fully qualified class name of blocks to be listed
	 * @return a list of ids for blocks owned by a specified diagram that
	 *         are of a specified class.
	 */
	public List<SerializableBlockStateDescriptor> listDiagramBlocksOfClass(String diagramId,String className);
	
	/**
	 * Query the gateway for list of diagrams belonging to a project. 
	 * 
	 * @param projectName
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
	 * @param blockName name of the block within the diagram 
	 * @return a list of blocks logically connected to the source.
	 */
	public List<SerializableBlockStateDescriptor> listSinksForSource(String diagramId,String blockName) ;
	/**
	 * Query the gateway for list of its source blocks associated with the
	 * specified sink. The blocks that are returned are not constrained
	 * to be part of the same diagram, family or application.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram  
	 * @return a list of blocks logically connected to the sink.
	 */
	public List<SerializableBlockStateDescriptor> listSourcesForSink(String diagramId,String blockName) ;
	/** 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a colon-separated path to the specified block. The path includes
	 *         the project name.
	 */
	public String pathForBlock(String diagramId,String blockName);
	/** 
	 * @param nodeId UUID as a String of a node in the navigation tree
	 * @return a slash-separated path to the specified node. The path 
	 *         root is a slash representing the top node of the navigation tree.
	 */
	public String pathForNode(String nodeId);
	/**
	 * Execute reset() on a specified block
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram 
	 */
	public void resetBlock(String diagramId,String blockName) ;

	/**
	 * Execute reset() on every block on the diagram
	 */
	public void resetDiagram(String diagramId) ;
	/**
	 * Determine whether or not the indicated resource is known to the controller.
	 */
	public boolean resourceExists(Long projectId,Long resid) ;
	/**
	 * Execute stop() then start() on a specified block
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram 
	 */
	public void restartBlock(String diagramId,String blockName) ;
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a tree-path.
	 * There may be no successful recipients.
	 * 
	 * @param diagramId
	 * @param command string of the signal
	 * @param message embedded in the transmitted signal
	 * @param arg also a component of the transmitted signal
	 */
	public boolean sendLocalSignal(String diagramId,String command,String message,String arg) ;
	/**
	 * Send a signal directly to a specified block.
	 * This is a "local" transmission. The signal timestamp is "now".
	 * 
	 * @param diagramId diagram identifier
	 * @param command string of the signal.
	 * @param message command payload
	 * @return true on success
	 */
	public boolean sendSignal(String diagramId,String blockName,String command,String message);
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The diagram is specified by a tree-path.
	 * There may be no successful recipients. This version time-stamps the signal sent
	 * 
	 * @param diagramId
	 * @param command string of the signal
	 * @param message embedded in the transmitted signal
	 * @param arg also a component of the transmitted signal
	 * @param time unix time, milli-seconds since the start of the epoch
	 */
	public boolean sendTimestampedSignal(String diagramId,String command,String message,String arg,long time) ;
	
	/**
	 * Set the state of every diagram that is a member of the application to
	 * the specified value.
	 * @param appname
	 * @param state
	 */
	public void setApplicationState(String appname, String state);

	/** Update all changed properties for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 */
	public void setBlockProperties(UUID duuid,UUID buuid, Collection<BlockProperty> props ) ;
	/** Update a single property for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 * @param property the changed property
	 */
	public void setBlockProperty(UUID duuid,UUID buuid,BlockProperty property ) ;
	
	/** Change the value of a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname 
	 * @param pname the changed property
	 * @param value the new value of the property. The value will be coerced into the correct data type in the gateway 
	 */
	public void setBlockPropertyValue(String diagramId,String bname,String pname,String value ) ;

	/** 
	 * Drive a block to the specified state. 
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname 
	 * @param state the new state of the block. The value will be coerced into a truth-value in the gateway 
	 */
	public void setBlockState(String diagramId,String bname,String state );
	public void setDiagramState(Long projectId, Long resourceId, String state) ;
	public void setDiagramState(String diagramId, String state);
	/**
	 * Save a value into the HSQL database table associated with the toolkit. The 
	 * table contains name-value pairs, so any name is allowable.
	 * @param propertyName name of the property for which a value is to be set
	 * @param the new value of the property.
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
	 */
	public void updateBlockAnchors(UUID duuid,UUID buuid, Collection<SerializableAnchor> anchors ) ;

}
