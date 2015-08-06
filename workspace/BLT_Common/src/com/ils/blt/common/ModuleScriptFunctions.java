/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.common;

import java.util.List;

import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;


/**
 * This class exposes python-callable functions that deal with properties
 * of applications, families, diagrams, blocks and connections. It also handles
 * functions of the engine itself. All requests are delegated to the 
 * ApplicationRequestManager.
 * 
 * These calls are available from any Ignition scope.
 */
public class ModuleScriptFunctions   {
	private static ToolkitRequestHandler handler = null;
	
	public static void setRequestHandler(ToolkitRequestHandler h) { handler = h; }

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
	
	public static String getApplicationName(String uuid) {
		return handler.getApplicationName(uuid);
	}
	/**
	 * Query the gateway for a list of prototypes for the defined blocks. 
	 */
	@SuppressWarnings("rawtypes")
	public static List getBlockPrototypes() {
		List<PalettePrototype> result = handler.getBlockPrototypes();
		return result;
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
	 * blockId String representation of the block's internal Id.
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
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the value of a specified block property.
	 */
	public static Object getPropertyValue(String diagramId,String blockId,String propertyName) {
		return handler.getPropertyValue(diagramId,blockId,propertyName);
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
	 * @param projectName
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
	 * @param blockId identifier for the source block, a String 
	 * @return a list of blocks logically connected to the source.
	 */
	public static List<SerializableBlockStateDescriptor> listSinksForSource(String diagramId,String blockName) {
		return handler.listSinksForSource(diagramId,blockName);
	}
	/**
	 * Query the gateway for list of its source blocks associated with the
	 * specified sink. The blocks that are returned all belong to the same
	 * application as the sink.
	 * @param blockId identifier for the sink block, a String 
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
	 * Post a (simulated) block result on its output.
	 * @param diagramId the parent diagram
	 * @param blockId
	 * @param port
	 * @param value
	 */
	public static void postResult(String diagramId,String blockId,String port,String value) {
		handler.postValue(diagramId,blockId, port, value);
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
	/**
	 * Execute reset() on the specified block
	 */
	public static void resetBlock(String diagramId,String blockId) {
		handler.resetBlock(diagramId,blockId);
	}

	/**
	 * Execute reset() on every block inside the controller
	 */
	public static void resetDiagram(String diagramId) {
		handler.resetDiagram(diagramId);
	}
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission.
	 * 
	 * @param diagramId diagram identifier
	 * @param className filter of the receiver blocks to be targeted.
	 * @param command string of the signal.
	 * @return true on success
	 */
	public static boolean sendLocalSignal(String diagramId,String command,String message,String arg) {
		return handler.sendLocalSignal(diagramId,command,message,arg);
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
	 * Specify the new state of a diagram
	 * @param diagramId
	 * @param state
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
	public static void setTestTimeOffset(long offset) {
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
	 * @param the new value of the property.
	 */
	public static void setToolkitProperty(String propertyName,String value) {
		handler.setToolkitProperty(propertyName,value);
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