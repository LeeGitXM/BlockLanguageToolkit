/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;


/**
 * This class exposes python-callable functions that deal with properties
 * of applications, families, diagrams, blocks and connections. It also handles
 * functions of the engine itself. All requests are delegated to the 
 * ApplicationRequestManager.
 * 
 * These calls are available from Designer or Client Ignition scopes. The 
 * python package name is: system.ils.blt.diagram.
 */
public class ApplicationScriptFunctions   {
	private static ApplicationRequestHandler handler = new ApplicationRequestHandler();

	public static List<SerializableResourceDescriptor> childNodes(String nodeId) {
		return handler.childNodes(nodeId);
	}
	/**
	 * Remove all running diagrams from the controller. 
	 * Cancel all tag subscriptions. 
	 */
	public static void clearController() {
		handler.clearController();
	}
	/**
	 * Clear any watermark currently on a diagram.
	 * @param diagramId the diagram identifier as a string
	 */
	public static void clearWatermark(String diagramId) {
		handler.clearWatermark(diagramId);
	}
	/**
	 * Delete a SQLTag given its path. The path must contain the
	 * provider name in brackets.
	 */
	public static void deleteTag(String path) {
		handler.deleteTag(path);
	}
	/**
	 * @param uuid id of the application
	 * @return the application name
	 */
	public static String getApplicationName(String uuid) {
		return handler.getApplicationName(uuid);
	}
	
	/**
	 * Query the gateway for a list of prototypes for the defined blocks.
	 * The prototypes are used for populating the palette.
	 * @return a list of prototypes or descriptions for defined blocks. 
	 */
	@SuppressWarnings("rawtypes")
	public static List getBlockPrototypes() {
		List<PalettePrototype> result = handler.getBlockPrototypes();
		return result;
	}
	/**
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockName name of the block within the diagram
	 * @return the id of the specified block.
	 */
	public static String getBlockId(String diagramId, String blockName) {
		return handler.getBlockId(diagramId,blockName);
	}
	/**
	 * @return the current state of the controller.
	 */
	public static String getBlockState(String diagramId,String blockName) {
		return handler.getBlockState(diagramId,blockName);
	}
	/**
	 * @return the current state of the controller.
	 */
	public static String getControllerState() {
		return handler.getControllerState();
	}

	public static String getDatabaseForUUID(String uuid) {
		return handler.getDatabaseForUUID(uuid);
	}
	
	public static List<String> getDatasourceNames() {
		return handler.getDatasourceNames();
	}
	/**
	 * @param diagramId String representation of the diagram's internal Id.
	 * @return a descriptor for the diagram that corresponds to that Id.
	 */
	public static SerializableResourceDescriptor getDiagram(String diagramId)  {
		return handler.getDiagram(diagramId);
	}
	
	/**
	 * Query the gateway for list of diagrams 
	 * 
	 * @param projectName
	 * @return a list of tree-paths to the diagrams saved (ie. known to the Gateway).
	 */
	@SuppressWarnings("rawtypes")
	public static List getDiagramDescriptors(String projectName) {
		return handler.listDiagramDescriptors(projectName);
	}
	/**
	 * @param blockId String representation of the block's internal Id.
	 * @return the diagram that is a parent of the specified block.
	 */
	public static SerializableResourceDescriptor getDiagramForBlock(String blockId) {
		return handler.getDiagramForBlock(blockId);
	}
	/**
	 * @return the current state of the specified diagram.
	 */
	public static DiagramState getDiagramState(Long projectId, Long resourceId)  {
		return handler.getDiagramState(projectId,resourceId);
	}
	/**
	 * @return the current state of the specified diagram.
	 */
	public static DiagramState getDiagramState(String diagramId)  {
		return handler.getDiagramState(diagramId);
	}
	/**
	 * @return an explanation for the state of a block.
	 */
	public static String getExplanation(String diagramId,String blockId) {
		return handler.getExplanation(diagramId,blockId);
	}

	public static String getFamilyName(String uuid) {
		return handler.getFamilyName(uuid);
	}
	/**
	 * @return the the internal state of a block.
	 */
	public static SerializableBlockStateDescriptor getInternalState(String diagramId,String blockId) {
		return handler.getInternalState(diagramId,blockId);
	}
	/**
	 * Find the name of the isolation datasource from the internal SQLite database. 
	 * @return isolation database name
	 */
	public static String getIsolationDatabase() {
		return handler.getIsolationDatabase();
	}
	/**
	 * Find the name of the isolation tag provider from the internal SQLite database. 
	 * @return isolation tag provider name
	 */
	public static String getIsolationTagProvider() {
		return handler.getIsolationTagProvider();
	}
	/**
	 * Find the name of the production datasource from the internal SQLite database. 
	 * @return production database name
	 */
	public static String getProductionDatabase() {
		return handler.getProductionDatabase();
	}
	/**
	 * Find the name of the isolation tag provider from the internal SQLite database. 
	 * @return production tag provider name
	 */
	public static String getProductionTagProvider() {
		return handler.getProductionTagProvider();
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the binding (tag path) of a specified block property.
	 */
	public static Object getPropertyBinding(String diagramId,String blockId,String propertyName) {
		return handler.getPropertyBinding(diagramId,blockId,propertyName);
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of a specified block property.
	 */
	public static Object getPropertyValue(String diagramId,String blockId,String propertyName) {
		return handler.getPropertyValue(diagramId,blockId,propertyName);
	}
	/**
	 * @param diagramId string representation of the diagram's unique id
	 * @param blockName name of the block within the diagram
	 * @return the time at which the block last changed its state
	 */
	public static Date getTimeOfLastBlockStateChange(String diagramId, String blockName) {
		return handler.getTimeOfLastBlockStateChange(diagramId,blockName);
	}
	/**
	 * Acquire a value from the HSQL database table associated with the toolkit. A
	 * null is returned if the string is not found.
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of the specified property.
	 */
	public static String getToolkitProperty(String propertyName) {
		return handler.getToolkitProperty(propertyName);
	}
	/**
	 * @return the alert state of the specified diagram;
	 */
	public static boolean isAlerting(Long projectId, Long resourceId) {
		return handler.isAlerting(projectId, resourceId);
	}
	/**
	 * Query a block in the gateway for list of the blocks connected to the named port. 
	 * @param diagramId of the parent diagram
	 * @param blockId identifier of the block
	 * @param portName of the anchor of interest
	 * @return a list of blocks connected to the named port.
	 */
	public static List<SerializableBlockStateDescriptor> listBlocksConnectedAtPort(String diagramId,String blockId,String portName) {
		return handler.listBlocksConnectedAtPort(diagramId,blockId,portName);
	}
	/**
	 * Query a diagram in the gateway for list of its blocks that are downstream
	 * of the specified block. 
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @return a list of blocks downstream of the block specified, in the diagram.
	 */
	public static List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(String diagramId,String blockName){
		return handler.listBlocksDownstreamOf(diagramId,blockName);
	}
	/**
	 * List all blocks that have properties bound to the supplied tag path. 
	 * @param tagpath the path for the tag of interest.
	 * @return a list of blocks associated with the tag.
	 */
	public static List<SerializableBlockStateDescriptor> listBlocksForTag(String tagpath) {
		return handler.listBlocksForTag(tagpath);
	}
	/**
	 * Query a diagram in the gateway for list of its blocks that are downstream
	 * of the specified block. If any of those blocks are sinks, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks downstream of the specified block.
	 */
	public static List<SerializableBlockStateDescriptor> listBlocksGloballyDownstreamOf(String diagramId,String blockName) {
		return handler.listBlocksGloballyDownstreamOf(diagramId, blockName);
	}
	/**
	 * Query a diagram in the gateway for list of its blocks that are upstream
	 * of the specified block. If any of those blocks are sources, then continue
	 * the search on the diagrams they are connected to.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram that is the starting point
	 * @return a list of blocks upstream of the specified block.
	 */
	public static List<SerializableBlockStateDescriptor> listBlocksGloballyUpstreamOf(String diagramId,String blockName) {
		return handler.listBlocksGloballyUpstreamOf(diagramId, blockName);
	}
	/**
	 * Query a diagram in the gateway for list of its blocks. 
	 * @param diagramId identifier of the diagram owning the blocks, a String 
	 * @return a list of blocks belonging to the diagram.
	 */
	public static List<SerializableBlockStateDescriptor> listBlocksInDiagram(String diagramId) {
		return handler.listBlocksInDiagram(diagramId);
	}
	
	/**
	 * Query a diagram in the gateway for list of its blocks that are upstream
	 * of the specified block. 
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockName name of the block within the diagram that is the starting point
	 * @return a list of blocks upstream of the block specified, in the diagram.
	 */
	public static List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(String diagramId,String blockName){
		return handler.listBlocksUpstreamOf(diagramId,blockName);
	}
	/**
	 * The result is a list of SerializableBlockState descriptors for those 
	 * blocks in any project that have configuration issues. Descriptor attributes
	 * contain at least the project and a path to the block. The descriptor 
	 * contains a textual description of whatever problem is detected.
	 * @return a list of blocks that have incomplete or incorrect configuration.
	 */
	public static List<SerializableBlockStateDescriptor> listConfigurationErrors() {
		return handler.listConfigurationErrors();
	}
	/**
	 * The result is a list of SerializableBlockState descriptors that correspond 
	 * to blocks in any project that contain properties bound to tags where those property
	 * values do not match the current tag values. The descriptor 
	 * contains a textual description of the property that is not responding.
	 * @return a list of blocks that have subscription errors.
	 */
	public static List<SerializableBlockStateDescriptor> listSubscriptionErrors() {
		return handler.listSubscriptionErrors();
	}
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
	public static List<SerializableBlockStateDescriptor> listUnresponsiveBlocks(double hours, String className) {
		return handler.listUnresponsiveBlocks(hours,className);
	}
	/**
	 * @param diagramId identifier of the diagram to be queried, a String
	 * @param className fully qualified class name of blocks to be listed
	 * @return a list of ids for blocks owned by a specified diagram that are of a
	 *         specified class.
	 */
	@SuppressWarnings({ "rawtypes" })
	public static List listDiagramBlocksOfClass(String diagramId,String className) {
		return handler.listDiagramBlocksOfClass(diagramId,className);
	}
	/**
	 * Query the gateway for list of diagrams belonging to a project. 
	 * 
	 * @param projectName name of the project containing the diagrams
	 * @return a list of tree-paths to the diagrams saved (ie. known to the Gateway).
	 */
	public static List<SerializableResourceDescriptor> listDiagramDescriptors(String projectName) {
		return handler.listDiagramDescriptors(projectName);
	}
	/**
	 * Query the gateway for list of resource nodes that the block controller
	 * knows about. This should correspond to what is displayed in the designer
	 * nav tree for all loaded projects. 
	 * 
	 * @return a list of resources known to the BlockController.
	 */
	public static List<SerializableResourceDescriptor> listResourceNodes(){
		return handler.listResourceNodes();
	}
	/**
	 * Query the gateway for list of its sink blocks associated with the
	 * specified source. The blocks that are returned are not constrained
	 * to be part of the same diagram, family or application.
	 * @param diagramId id of the enclosing diagram, a String
	 * @param blockName name of the source block, a String 
	 * @return a list of blocks logically connected to the source.
	 */
	public static List<SerializableBlockStateDescriptor> listSinksForSource(String diagramId,String blockName) {
		return handler.listSinksForSource(diagramId,blockName);
	}
	/**
	 * Query the gateway for list of its source blocks associated with the
	 * specified sink. The blocks that are returned all belong to the same
	 * application as the sink.
	 * @param diagramId id of the enclosing diagram, a String
	 * @param blockName name of the sink block, a String 
	 * @return a list of blocks logically connected to the sink.
	 */
	public static List<SerializableBlockStateDescriptor> listSourcesForSink(String diagramId,String blockName) {
		return handler.listSourcesForSink(diagramId,blockName);
	}
	/** 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a colon-separated path to the specified block. The path includes
	 *         the project name
	 */
	public static String pathForBlock(String diagramId,String blockName) {
		return handler.pathForBlock(diagramId,blockName);
	}
	/** 
	 * @param nodeId UUID as a String of a node in the navigation tree
	 * @return a slash-separated path to the specified node. The path 
	 *         root is a slash representing the top node of the navigation tree.
	 */
	public static String pathForNode(String nodeId) {
		return handler.pathForNode(nodeId);
	}
	/**
	 * Post a (simulated) block result on its output.
	 * @param diagramId the parent diagram
	 * @param blockId of the subject block
	 * @param port connection on which to receive the output
	 * @param value to be propagated
	 */
	public static void postResult(String diagramId,String blockId,String port,String value) {
		handler.postResult(diagramId,blockId, port, value);
	}
	/**
	 * @param diagramId the parent diagram
	 * @param blockId of the block within the diagram
	 */
	public static void propagateBlockState(String diagramId,String blockId) {
		handler.propagateBlockState(diagramId,blockId);
	}
	/**
	 * Query the gateway for list of resources that it knows about. This is
	 * a debugging aid. 
	 * 
	 * @return a list of resources known to the BlockController.
	 */
	@SuppressWarnings("rawtypes")
	public static List queryControllerResources() {
		List<SerializableResourceDescriptor> result = handler.listResourceNodes();
		return result;
	}
	/** Update a single property for a block 
	 * @param duuid diagram unique Id
	 * @param buuid block unique Id
	 * @param name the new name
	 */
	public static void renameBlock(String duuid,String buuid,String name ) {
		handler.renameBlock(duuid, buuid, name);
	}
	/**
	 * Rename a SQLTag given its path and new name. The path must contain the
	 * provider name in brackets.
	 */
	public static void renameTag(String name,String path) {
		handler.renameTag(name,path);
	}
	/**
	 * Execute reset() on the specified block
	 * @param diagramId the parent diagram
	 * @param blockId of the block to be reset
	 */
	public static void resetBlock(String diagramId,String blockId) {
		handler.resetBlock(diagramId,blockId);
	}
	/**
	 * Execute reset() on every block inside the controller
	 * @param diagramId diagram identifier, a string
	 */
	public static void resetDiagram(String diagramId) {
		handler.resetDiagram(diagramId);
	}
	/**
	 * Execute stop() then start() on the specified block
	 * @param diagramId diagram identifier, a string
	 * @param blockId block identifier, a string
	 */
	public static void restartBlock(String diagramId,String blockId) {
		handler.restartBlock(diagramId,blockId);
	}
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The signal timestamp is "now".
	 * 
	 * @param diagramId diagram identifier
	 * @param command string of the signal.
	 * @param message command payload
	 * @param arg a modifier meant to distinguish who receives the signal, e.e class name
	 * @return true on success
	 */
	public static boolean sendLocalSignal(String diagramId,String command,String message,String arg) {
		return handler.sendLocalSignal(diagramId,command,message,arg);
	}
	/**
	 * Send a signal directly to a specified block.
	 * This is a "local" transmission. The signal timestamp is "now".
	 * 
	 * @param diagramId diagram identifier
	 * @param blockName name of the block
	 * @param command string of the signal.
	 * @param message command payload
	 * @return true on success
	 */
	public static boolean sendSignal(String diagramId,String blockName,String command,String message) {
		return handler.sendSignal(diagramId,blockName,command,message);
	}
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission.
	 * 
	 * @param diagramId diagram identifier
	 * @param command string of the signal.
	 * @param message text to accompany the command.
	 * @param arg an optional command argument.
	 * @param time the timestamp associated with the signal
	 * @return true on success
	 */
	public static boolean sendTimestampedSignal(String diagramId,String command,String message,String arg,long time) {
		return handler.sendTimestampedSignal(diagramId,command,message,arg,time);
	}
	/**
	 * Change the state of every diagram in the named application
	 * to the specified state.
	 * @param appname name of the application
	 * @param state new diagram state
	 */
	public static void setApplicationState(String appname, String state) {
		handler.setApplicationState(appname, state);
	}
	/** 
	 * Drive a block to the specified state. Other than the state change, there are normally
	 * no other side effects. 
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname block name
	 * @param value the new state of the block. The value will be coerced into a truth-value in the gateway 
	 */
	public static void setBlockState(String diagramId,String bname,String value ) {
		handler.setBlockState(diagramId, bname, value);
	}
	/** Change the binding on a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname name of the block
	 * @param pname the changed property
	 * @param value the new binding of the property. The value must be a legal tag path 
	 */
	public static void setBlockPropertyBinding(String diagramId,String bname,String pname,String value ) {
		handler.setBlockPropertyBinding(diagramId, bname, pname, value);
	}
	
	/** Change the value of a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname block name
	 * @param pname the changed property
	 * @param value the new value of the property. The value will be coerced into the correct data type in the gateway 
	 */
	public static void setBlockPropertyValue(String diagramId,String bname,String pname,String value ) {
		handler.setBlockPropertyValue(diagramId, bname, pname, value);
	}
	/**
	 * Specify the new state of a diagram
	 * @param diagramId diagram's unique Id as a String
	 * @param state the new state
	 */
	public static void setDiagramState(String diagramId, String state)  {
		handler.setDiagramState(diagramId,state);
	}
	/**
	 * Tell the testing timer about the difference between test time
	 * and current time.
	 * @param offset the difference between test time and current time
	 *        ~ msecs. A positive number implies that the test time is
	 *        in the past.
	 */
	public void setTestTimeOffset(long offset) {
		handler.setTestTimeOffset(offset);
	}
	
	/**
	 * Set a clock rate factor. This will change timing for isolation mode only.
	 * This method is provided as a hook for test frameworks.
	 * @param factor the amount to speed up or slow down the clock.
	 */
	public static void setTimeFactor(double factor) {
		handler.setTimeFactor(factor);
	}
	/**
	 * Save a value into the HSQL database table associated with the toolkit. The 
	 * table contains name-value pairs, so any name is allowable.
	 * @param propertyName name of the property for which a value is to be set
	 * @param value the new value of the property.
	 */
	public static void setToolkitProperty(String propertyName,String value) {
		handler.setToolkitProperty(propertyName,value);
	}
	/**
	 * Define a watermark for a diagram. 
	 */
	public static void setWatermark(String diagramId,String text) {
		handler.setWatermark(diagramId,text);
	}
	/**
	 * Start the block execution engine in the gateway.
	 */
	public static void startController() {
		handler.startController();
	}
	/**
	 * Shutdown the block execution engine in the gateway.
	 */
	public static void stopController() {
		handler.stopController();
	}

	/**
	 * Direct the blocks in a specified diagram to report their
	 * status values. This is in order to update the UI. 
	 */
	public static void triggerStatusNotifications() {
		handler.triggerStatusNotifications();
	}
}