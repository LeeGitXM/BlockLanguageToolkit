/**
e *   (c) 2013-2016  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.Date;
import java.util.UUID;

import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessApplication;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.ProcessFamily;
import com.ils.blt.gateway.engine.ProcessNode;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataQuality;

/**
 * This class exposes python-callable requests directed at the execution engine. 
 * An instance of this class is exposed as the "handler" member of each block
 * implemented in python.
 */
public class PythonRequestHandler   {
	private static final String CLSS = "PythonRequestHandler";
	private static ILSLogger log = LogMaker.getLogger(PythonRequestHandler.class);
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	private static String alerterClassName = null;
	
	public PythonRequestHandler() {}
	/**
	 * Traverse the parent nodes until we find an Application. If there 
	 * are none in our ancestry, return null.
	 * 
	 * @param nodeId idenILSLoggertifier for the node, a string version of a UUID
	 * @return the ancestrial application
	 */
	public ProcessApplication getApplication(String nodeId)  {
		//log.infof("%s.getApplication, node = %s ",TAG,nodeId);
		ProcessApplication app = null;
		try {
			UUID uuid = UUID.fromString(nodeId);
			
			ProcessNode node = controller.getProcessNode(uuid);
			while( node!=null ) {
				if( node instanceof ProcessApplication ) {
					app = (ProcessApplication)node;
					//log.infof("%s.getApplication, found application = %s ",TAG,app.getName());
					break;
				}
				node = controller.getProcessNode(node.getParent());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getApplication: %s is an illegal UUID (%s)",TAG,nodeId,iae.getMessage());
		}
		return app;
	}
	
	
	/**
	 * Find a block given the Id of the parent diagram the block itself.
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @param blockId identifier for the block, a string version of a UUID
	 * @return the referenced block
	 */
	public ProcessBlock getBlock(String parent,String blockId)  {
		log.tracef("%s.getBlock, diagram.block = %s.%s ",TAG,parent,blockId);
		ProcessBlock block = null;
		try {
			UUID parentuuid = UUID.fromString(parent);
			ProcessNode node = controller.getProcessNode(parentuuid);
			if( node instanceof ProcessDiagram ) {
				UUID uuid = UUID.fromString(blockId);
				ProcessDiagram diag = (ProcessDiagram)node;
				block = diag.getBlock(uuid);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getBlock: on of %s or %s is an illegal UUID (%s)",TAG,parent,blockId,iae.getMessage());
		}
		return block;
	}
	/**
	 * Find a block given the Id of the parent diagram the block itself.
	 * 
	 * @param diagram container for the blocks
	 * @param blockName name of the block within the diagram
	 * @return a string version of the Id of the block with the specified name, else null
	 */
	public String getBlockId(ProcessDiagram diagram,String blockName)  {
		String id = null;
		for( ProcessBlock block:diagram.getProcessBlocks() ) {
			if( block.getName().equalsIgnoreCase(blockName)) {
				id = block.getBlockId().toString();
				break;
			}
		}
		return id;
	}
	
	/**
	 * Search the tree of node ancestors until we find one with the project set.
	 * @param uuidString identifier for a node, a string version of a UUID
	 * @return the default database for the project containing this node
	 */
	public String getDefaultDatabase(String uuidString)  {
		String dbName = "";
		try {
			UUID uuid = UUID.fromString(uuidString);
			ProcessNode node = controller.getProcessNode(uuid);
			while( node!=null) {
				if(node instanceof ProcessDiagram ) {
					ProcessDiagram diagram = (ProcessDiagram)node;
					if( diagram.getState().equals(DiagramState.ISOLATED)) dbName = controller.getIsolationDatabase();
					else dbName = controller.getProductionDatabase();
					break;
				}
				node = controller.getProcessNode(node.getParent());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDefaultDatabase: %s is an illegal UUID (%s)",TAG,uuidString,iae.getMessage());
		}
		if( !dbName.isEmpty() ) log.debugf("%s.getDefaultDatabase: %s ",TAG,dbName);
		else                   log.warnf("%s.getDefaultDatabase: Database for diagram %s not found,",TAG,uuidString);
		return dbName;
	}
	/**
	 * @return the name of the isolation datasource
	 */
	public String getIsolationDatabase() {
		return controller.getIsolationDatabase();
	}
	/**
	 * @return the name of the production datasource
	 */
	public String getProductionDatabase() {
		return controller.getProductionDatabase();
	}
	/**
	 * @param uuidString identifier for the diagram, a string version of a UUID
	 * @return the default tag provider for the project associated with 
	 *         the specified diagram
	 */
	public String getDefaultTagProvider(String uuidString)  {
		log.tracef("%s.getDefaultTagProvider, node = %s ",TAG,uuidString);
		String provider = "";
		try {
			UUID uuid = UUID.fromString(uuidString);
			ProcessNode node = controller.getProcessNode(uuid);
			while(  node!=null) {
				if(node instanceof ProcessDiagram ) {
					ProcessDiagram diagram = (ProcessDiagram)node;
					if( diagram.getState().equals(DiagramState.ISOLATED)) provider = controller.getIsolationProvider();
					else provider = controller.getProductionProvider();
					break;
				}
				node = controller.getProcessNode(node.getParent());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDefaultTagProvider: %s is an illegal UUID (%s)",TAG,uuidString,iae.getMessage());
		}
		if( provider.isEmpty() ) log.warnf("%s.getDefaultTagProvider: Provider for diagram %s not found,",TAG,uuidString);
		return provider;
	}
	/**
	 * Given an identifier string, return the associated diagram. 
	 * The parent of a block should be a diagram.
	 * 
	 * @return the diagram
	 */
	public ProcessDiagram getDiagram(String diagramId)  {
		log.tracef("%s.getDiagram, diagram = %s ",TAG,diagramId);
		ProcessDiagram diag = null;
		try {
			UUID diagramuuid = UUID.fromString(diagramId);
			ProcessNode node = controller.getProcessNode(diagramuuid);
			if( node instanceof ProcessDiagram ) {
				diag = (ProcessDiagram)node;
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDiagram: %s is an illegal UUID (%s)",TAG,diagramId,iae.getMessage());
		}
		return diag;
	}
	/**
	 * Traverse the parent nodes until we find a Family. If there 
	 * are none in our ancestry, return null.
	 * 
	 * @param nodeId identifier for the node, a string version of a UUID
	 * @return the ancestrial family
	 */
	public ProcessFamily getFamily(String nodeId)  {
		log.tracef("%s.getFamily, node = %s ",TAG,nodeId);
		ProcessFamily fam = null;
		try {
			UUID nodeuuid = UUID.fromString(nodeId);
			
			ProcessNode node = controller.getProcessNode(nodeuuid);
			while( node!=null ) {
				if( node instanceof ProcessFamily ) {
					fam = (ProcessFamily)node;
					break;
				}
				node = controller.getProcessNode(node.getParent());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getFamily: %s is an illegal UUID (%s)",TAG,nodeId,iae.getMessage());
		}
		return fam;
	}

	public boolean isAlerting(ProcessDiagram diagram) {
		return getAlertStatus(diagram);
	}
	/**
	 * Inform anyone who will listen about the diagram alerting status.
	 * 
	 * @param block the block that is responsible for a status change
	 */
	public void postAlertingStatus(ProcessBlock block)  {
		// It only matters if this is an alerter
		if( block.getClassName().equalsIgnoreCase(alerterClassName) ) {
			ProcessNode node = controller.getProcessNode(block.getParentId());
			if( node instanceof ProcessDiagram ) {
				ProcessDiagram diagram = (ProcessDiagram)node;
				boolean alerting = getAlertStatus(diagram);
				controller.sendAlertNotification(diagram.getResourceId(), (alerting?"true":"false"));
			}	
		}
	}
	/**
	 * Handle the block placing a new value on its output. The input may be PyObjects.
	 * This method is not "test-time" aware.
	 * 
	 * @param parent identifier for the parent, a string version of a UUID
	 * @param id block identifier a string version of the UUID
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 * @param time timestamp of the notification
	 */
	public void postValue(String parent,String id,String port,String value,String quality,long time)  {
		log.debugf("%s.postValue - %s = %s (%s) on %s",TAG,id,value.toString(),quality.toString(),port);
		
		try {
			UUID uuid = UUID.fromString(id);
			ProcessDiagram diagram = getDiagram(parent); 
			// Do nothing if diagram is disabled
			if( diagram!=null && !diagram.getState().equals(DiagramState.DISABLED)) {
				UUID parentuuid = UUID.fromString(parent);
				ControllerRequestHandler.getInstance().postValue(parentuuid,uuid,port,value,quality);
				controller.sendConnectionNotification(id, port, 
						new BasicQualifiedValue(value,
								new BasicQuality(quality,(quality.equalsIgnoreCase("good")?Quality.Level.Good:Quality.Level.Bad)),
								new Date(time)));
				ProcessBlock block = diagram.getBlock(uuid);
				postAlertingStatus(block);
				
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",TAG,parent,id,iae.getMessage());
		}
	}
	
	/**
	 * Handle the block placing a new value on its output. The input may be PyObjects.
	 * 
	 * @param id block identifier a string version of the UUID
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 * @param time timestamp of the notification
	 */
	public void sendConnectionNotification(String id, String port, String value,String quality,long time)  {
		log.tracef("%s.sendConnectionNotification - %s = %s on %s",TAG,id,value.toString(),port);
		controller.sendConnectionNotification(id, port, new BasicQualifiedValue(value,
				new BasicQuality(quality,(quality.equalsIgnoreCase("good")?Quality.Level.Good:Quality.Level.Bad)),
				new Date(time)));
	}
	/**
	 * Handle a block setting a new property value.
	 * 
	 * @param id block identifier a string version of the UUID
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 */
	public void sendPropertyNotification(String id, String port, String value)  {
		log.tracef("%s.sendConnectionNotification - %s = %s on %s",TAG,id,value.toString(),port);
		controller.sendConnectionNotification(id, port, new BasicQualifiedValue(value));
	}
	/**
	 * Broadcast a result to blocks in the diagram
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @param command the value of the signal
	 * @param message text of the signal
	 * @param arg an argument
	 * @param time the time associated with this signal
	 */
	public void sendTimestampedSignal(String parent,String command,String message,String arg,long time)  {
		log.debugf("%s.sendLocalSignal - %s = %s %s %s ",TAG,parent,command,message,arg);
		ControllerRequestHandler.getInstance().sendTimestampedSignal(parent,command,message,arg,time);
		
	}
	/**
	 * Specify a class of block that, when it has a true state, gives the entire diagram
	 * an "alerting" state. For now there is only a single class defined. This could easily
	 * be a list. This is global.
	 * @param cname
	 */
	public void setAlerterClass(String cname) {
		alerterClassName = cname;
	}
	/**
	 * Write a value to the named tag. The provider is the provider appropriate to
	 * the referenced diagram
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @param tagPath path to the tag
	 * @param data the value to be written
	 * @param quality the quality of the output
	 * @param time the time associated with this write operation
	 */
	public void updateTag(String parent,String tagPath,String data,String quality,long time)  {
		if( tagPath==null || tagPath.isEmpty() ) return;   // Fail silently
		log.debugf("%s.updateTag - %s = %s %s %s %d ",TAG,parent,tagPath,data,quality,time);

		UUID diagId = UUID.fromString(parent);
		Quality q = DataQuality.GOOD_DATA;
		if(!quality.equalsIgnoreCase("good")) q = new BasicQuality(quality,Quality.Level.Bad);
		QualifiedValue qv = new BasicQualifiedValue(data,q,new Date(time));
		controller.updateTag(diagId, tagPath, qv);
	}
	
	/*
	 * This method is only called on a state change of one of the Python blocks under the
	 * assumption that hard-coded blocks do not impact the diagram's alert status.
	 */
	private boolean getAlertStatus(ProcessDiagram diagram) {
		boolean result = false;
		if( alerterClassName!=null ) {
			for(ProcessBlock blk:diagram.getProcessBlocks()){
				if(blk.getClassName().equalsIgnoreCase(alerterClassName)) {
					if(blk.getState().equals(TruthValue.TRUE)) {
						result = true;
						break;
					}
				}
			}
		}
		return result;
	}
}