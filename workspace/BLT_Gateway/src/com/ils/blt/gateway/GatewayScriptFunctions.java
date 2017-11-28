/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.Date;
import java.util.List;

import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;


/**
 * This class exposes python-callable functions in the Gateway scope. Currently
 * the main user of these is the test framework. These mirror the functions
 * available in Client/Designer scope.  All requests are delegated to the 
 * same request handler as the GatewayRpcDispatcher.
 * 
 * These calls are available from the Gateway Ignition scope. The 
 * python package name is: system.ils.blt.diagram.
 */
public class GatewayScriptFunctions   {
	private static ControllerRequestHandler handler = ControllerRequestHandler.getInstance();
	private static PythonRequestHandler pyhandler = new PythonRequestHandler();
	
	/**
	 * @param nodeId identifier of a node in the Designer NavTree
	 * @return a list of the node's children
	 */
	public static List<SerializableResourceDescriptor> childNodes(String nodeId) {
		return handler.childNodes(nodeId);
	}
	/**
	 * Clear any watermark on a diagram.
	 * @param diagramId string representation of the diagram's unique id 
	 */
	public static void clearWatermark(String diagramId) {
		handler.clearWatermark(diagramId);
	}
	/**
	 * Remove all running diagrams from the controller. 
	 * Cancel all tag subscriptions. 
	 */
	public static void clearController() {
		handler.clearController();
	}
	/**
	 * @param uuid string value of application's UUID
	 * @return name of the application
	 */
	public static String getApplicationName(String uuid) {
		return handler.getApplicationName(uuid);
	}
	/**
	 * Query the gateway for a list of prototypes for the defined blocks.
	 * @return a list containing prototypes for all existing blocks 
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
	 * @param diagramId diagram identifier
	 * @param blockName name of the target block
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
	/**
	 * @param uuid string value of node's UUID
	 * @return name of the database connection appropriate to the node's state
	 */
	public static String getDatabaseForUUID(String uuid) {
		return handler.getDatabaseForUUID(uuid);
	}
	/**
	 * @return a list of datasource names
	 */
	public static List<String> getDatasourceNames() {
		return handler.getDatasourceNames();
	}
	/**
	 * @param diagramId String representation of the diagram's internal Id.
	 * @return a descriptor for the diagram that corresponds to that Id.
	 */
	public static SerializableResourceDescriptor getDiagram(String diagramId) {
		return handler.getDiagram(diagramId);
	}
	/**
	 * Query the gateway for list of all diagrams. This method takes
	 * the name of a project which is unused. Its purpose is to provide
	 * method signature compatibility with the Designer/Client scope.
	 * 
	 * @param dummyProject is unused
	 * @return a list of tree-paths to the diagrams saved 
	 *         (ie. known to the Gateway).
	 */
	@SuppressWarnings("rawtypes")
	public static List getDiagramDescriptors(String dummyProject) {
		return handler.getDiagramDescriptors();
	}
	/**
	 * Query the gateway for list of all diagrams. 
	 * 
	 * @return a list of tree-paths to the diagrams saved 
	 *         (ie. known to the Gateway).
	 */
	@SuppressWarnings("rawtypes")
	public static List getDiagramDescriptors() {
		return handler.getDiagramDescriptors();
	}
	/**
	 * @param blockId String representation of the block's internal Id.
	 * @return the diagram that is a parent of the specified block.
	 */
	public static SerializableResourceDescriptor getDiagramForBlock(String blockId) {
		return handler.getDiagramForBlock(blockId);
	}
	/**
	 * @param diagramId the id of the diagram as a String
	 * @return the current state of the specified diagram.
	 */
	public static DiagramState getDiagramState(String diagramId)  {
		return handler.getDiagramState(diagramId);
	}
	/**
	 * @param uuid string value of family's UUID
	 * @return name of the family
	 */
	public static String getFamilyName(String uuid) {
		return handler.getFamilyName(uuid);
	}

	/**
	 * The Python request handler is made available to
	 * every block that is implemented in Python. The
	 * handler provides facilities to acquire database and
	 * tag provider references, and to submit block outputs.
	 * 
	 * The handler also provides access to diagram and block objects.
	 * 
	 * @return the python request handler.
	 */
	public static PythonRequestHandler getHandler() {
		return pyhandler;
	}
	/**
	 * @param diagramId diagram identifier as a string
	 * @param blockId block's identifier as a string
	 * @return an explanation for the state of a block.
	 */
	public static String getExplanation(String diagramId,String blockId) {
		return handler.getExplanation(diagramId,blockId);
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block
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
	 * Find the name of the production tag provider from the internal SQLite database. 
	 * @return production tag provider name
	 */
	public static String getProductionTagProvider() {
		return handler.getProductionTagProvider();
	}
	/**
	 * @param diagramId identifier of the diagram owning the block, a String
	 * @param blockId identifier of the block within the diagram, a String
	 * @param propertyName name of the property for which a value is to be returned
	 * @return the binding (e.g. tag path)  of a specified block property.
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
	 * The controller request handler is made available to scripting
	 * functions that execute in the Gateway.
	 * 
	 * @return the python request handler.
	 */
	public static ControllerRequestHandler getRequestHandler() {
		return handler;
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
	 * Query a diagram in the gateway for list of its blocks. 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks belonging to the diagram.
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
	public static List<SerializableBlockStateDescriptor> listBlocksGloballyDownstreamOf(String diagramId, String blockName) {
		return handler.listBlocksGloballyDownstreamOf(diagramId, blockName);
	}
	public static List<SerializableBlockStateDescriptor> listBlocksGloballyUpstreamOf(String diagramId, String blockName) {
		return handler.listBlocksGloballyUpstreamOf(diagramId, blockName);
	}
	/**
	 * Query a diagram in the gateway for list of its blocks. 
	 * @param diagramId of the parent diagram
	 * @return a list of blocks belonging to the diagram.
	 */
	public static List<SerializableBlockStateDescriptor> listBlocksInDiagram(String diagramId) {
		return handler.listBlocksInDiagram(diagramId);
	}
	/**
	 * Query a diagram in the gateway for list of its blocks that are upstream
	 * of the specified block. 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
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
	 * Query an application in the gateway for list of descendants down to the level of a diagram. 
	 * @param appName of the parent application
	 * @return a list of nodes under the named application
	 */
	public static List<SerializableResourceDescriptor> listDescriptorsForApplication(String appName) {
		return handler.listDescriptorsForApplication(appName);
	}
	/**
	 * Query a family in the gateway for list of descendants down to the level of a diagram. 
	 * @param appName of the parent application
	 * @param famName of the parent family
	 * @return a list of nodes under the named family
	 */
	public static List<SerializableResourceDescriptor> listDescriptorsForFamily(String appName,String famName) {
		return handler.listDescriptorsForFamily(appName,famName);
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
	 * @param projectName name of the project
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
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks logically connected to the source.
	 */
	public static List<SerializableBlockStateDescriptor> listSinksForSource(String diagramId,String blockName) {
		return handler.listSinksForSource(diagramId,blockName);
	}
	/**
	 * Query the gateway for list of its source blocks associated with the
	 * specified sink. The blocks that are returned all belong to the same
	 * application as the sink.
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a list of blocks logically connected to the sink.
	 */
	public static List<SerializableBlockStateDescriptor> listSourcesForSink(String diagramId,String blockName) {
		return handler.listSourcesForSink(diagramId,blockName);
	}
	/** 
	 * @param diagramId of the parent diagram
	 * @param blockName name of the block within the diagram
	 * @return a colon-separated path to the specified block. The path includes
	 *         the project name.
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
	 * @param blockId identified of the target block
	 * @param port name of the target port
	 * @param value to be propagated
	 */
	public static void postResult(String diagramId,String blockId,String port,String value) {
		handler.postValue(diagramId,blockId, port, value);
	}
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
	/**
	 * Execute reset() on the specified block
	 * @param diagramId the parent diagram
	 * @param blockId identifier of the target block
	 */
	public static void resetBlock(String diagramId,String blockId) {
		handler.resetBlock(diagramId,blockId);
	}
	/**
	 * Execute reset() on every block inside the diagram
	 * @param diagramId the parent diagram
	 */
	public static void resetDiagram(String diagramId) {
		handler.resetDiagram(diagramId);
	}
	/**
	 * Execute stop() then start() on the specified block
	 * @param diagramId the parent diagram
	 * @param blockId identifier of the target block
	 */
	public static void restartBlock(String diagramId,String blockId) {
		handler.restartBlock(diagramId,blockId);
	}
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission.
	 * 
	 * @param diagramId diagram identifier
	 * @param command string of the signal.
	 * @param message command payload
	 * @param arg argument
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
	 * @param blockName name of the target block
	 * @param command string of the signal.
	 * @param message command payload
	 * @return true on success
	 */
	public static boolean sendSignal(String diagramId,String blockName,String command,String message) {
		return handler.sendSignal(diagramId,blockName,command,message);
	}
	
	/**
	 * Send a signal to all blocks of a particular class on a specified diagram.
	 * This is a "local" transmission. The timestamp is supplied in the call.
	 * 
	 * @param diagramId diagram identifier
	 * @param command string of the signal.
	 * @param message text of the signal
	 * @param arg argument
	 * @param time test-aware time stamp assigned to the signal qualified value
	 * @return true on success
	 */
	public static boolean sendTimestampedSignal(String diagramId,String command,String message,String arg,long time) {
		return handler.sendTimestampedSignal(diagramId,command,message,arg,time);
	}
	/**
	 * Set the state for every diagram under the named application.
	 * @param app name of the application
	 * @param state new state of the diagrams
	 */
	public static void setApplicationState(String app,String state) {
		handler.setApplicationState(app,state);
	}
	/** Change the value of a block property in such a way that the block and UI
	 * are notified of the change.
	 *  
	 * @param diagramId diagram's unique Id as a String
	 * @param bname name of the block
	 * @param pname the changed property
	 * @param value the new value of the property. The value will be coerced into the correct data type in the gateway 
	 */
	public static void setBlockPropertyValue(String diagramId,String bname,String pname,String value )  {
		handler.setBlockPropertyValue(diagramId, bname, pname, value);
	}
	/**
	 * Specify the new state of a block
	 * @param diagramId diagram's unique Id as a String
	 * @param bname name of the block
	 * @param state new state for the diagram
	 */
	public static void setBlockState(String diagramId,String bname,String state ) {
		handler.setBlockState(diagramId,bname,state);
	}
	
	/**
	 * Specify the new state of a diagram
	 * @param diagramId diagram's unique Id as a String
	 * @param state new state for the diagram
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
	 * Set a clock rate factor. This must NOT be exercised in a production environment.
	 * This is a hook for testing only.
	 * @param factor the amount to speed up or slow down the clock.
	 */
	public static void setTimeFactor(Double factor) {
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
	 * Define a watermark for a diagram. Setting an empty string is equivalent to
	 * deleting the watermark.
	 *  @param diagramId diagram's unique Id as a String
	 *  @param text to be displayed
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