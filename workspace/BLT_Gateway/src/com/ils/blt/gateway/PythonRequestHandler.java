/**
e *   (c) 2013-2022  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.Date;
import java.util.UUID;

import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.ProcessNode;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualityCode;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * This class exposes python-callable requests directed at the execution engine. 
 * An instance of this class is exposed as the "handler" member of each block
 * implemented in python.
 */
public class PythonRequestHandler   {
	private static final String CLSS = "PythonRequestHandler";
	private static LoggerEx log = LogUtil.getLogger(PythonRequestHandler.class.getPackageName());
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	private static String alerterClassName = null;
	
	public PythonRequestHandler() {}

	
	
	/**
	 * Find a block given the Id of the parent diagram the block itself.
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @param blockId identifier for the block, a string version of a UUID
	 * @return the referenced block
	 */
	public ProcessBlock getBlock(ProjectResourceId parent,String blockId)  {
		log.tracef("%s.getBlock, diagram.block = %s.%s ",CLSS,parent,blockId);
		ProcessBlock block = null;
		try {
			ProcessNode node = controller.getProcessNode(parent);
			if( node instanceof ProcessDiagram ) {
				UUID uuid = UUID.fromString(blockId);
				ProcessDiagram diag = (ProcessDiagram)node;
				block = diag.getProcessBlock(uuid);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getBlock: on of %s or %s is an illegal UUID (%s)",CLSS,parent,blockId,iae.getMessage());
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
	public String getDefaultDatabase(ProjectResourceId resourceId)  {
		String dbName = "";

		ProcessNode node = controller.getProcessNode(resourceId);
		while( node!=null) {
			if(node instanceof ProcessDiagram ) {
				ProcessDiagram diagram = (ProcessDiagram)node;
				if( diagram.getState().equals(DiagramState.ISOLATED)) dbName = controller.getProjectIsolationDatabase(resourceId.getProjectName());
				else dbName = controller.getProjectProductionDatabase(resourceId.getProjectName());
				break;
			}
			node = controller.getParentNode(node);
		}

		if( !dbName.isEmpty() ) log.debugf("%s.getDefaultDatabase: %s ",CLSS,dbName);
		else                   log.warnf("%s.getDefaultDatabase: Database for diagram %s not found,",CLSS,resourceId.getResourcePath().getPath().toString());
		return dbName;
	}
	
	/**
	 * @return the name of the isolation datasource
	 */
	public String getProjectIsolationDatabase(String projectName) {
		return controller.getProjectIsolationDatabase(projectName);
	}
	/**
	 * @return the name of the production datasource
	 */
	public String getProjectProductionDatabase(String projectName) {
		return controller.getProjectProductionDatabase(projectName);
	}
	/**
	 * @param uuidString identifier for the diagram, a string version of a UUID
	 * @return the default tag provider for the project associated with 
	 *         the specified diagram
	 */
	public String getDefaultTagProvider(ProjectResourceId resid)  {
		log.tracef("%s.getDefaultTagProvider, node = %s ",CLSS,resid.getResourcePath().getPath().toString());
		String provider = "";
		try {
			ProcessNode node = controller.getProcessNode(resid);
			while(  node!=null) {
				if(node instanceof ProcessDiagram ) {
					ProcessDiagram diagram = (ProcessDiagram)node;
					if( diagram.getState().equals(DiagramState.ISOLATED)) provider = controller.getProjectIsolationProvider(resid.getProjectName());
					else provider = controller.getProjectProductionProvider(resid.getProjectName());
					break;
				}
				node = controller.getParentNode(node);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDefaultTagProvider: %s is an illegal UUID (%s)",CLSS,resid.getResourcePath().getPath().toString(),iae.getMessage());
		}
		if( provider.isEmpty() ) log.warnf("%s.getDefaultTagProvider: Provider for diagram %s not found,",CLSS,resid.getResourcePath().getPath().toString());
		return provider;
	}
	/**
	 * Given an identifier string, return the associated diagram. 
	 * The parent of a block should be a diagram.
	 * 
	 * @return the diagram
	 */
	public ProcessDiagram getDiagram(ProjectResourceId diagramId)  {
		log.tracef("%s.getDiagram, diagram = %s ",CLSS,diagramId);
		ProcessDiagram result = null;
		try {
			ProcessNode node = controller.getProcessNode(diagramId);
			if( node instanceof ProcessDiagram ) {
				result = (ProcessDiagram)node;
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDiagram: %s is an illegal UUID (%s)",CLSS,diagramId,iae.getMessage());
		}
		return result;
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
	public void postValue(ProjectResourceId parent,String id,String port,String value,String quality,long time)  {
		log.debugf("%s.postValue - %s = %s (%s) on %s",CLSS,id,value.toString(),quality.toString(),port);
		
		try {
			UUID uuid = UUID.fromString(id);
			ProcessDiagram diagram = getDiagram(parent); 
			// Do nothing if diagram is disabled
			if( diagram!=null && !diagram.getState().equals(DiagramState.DISABLED)) {
				ControllerRequestHandler.getInstance().postValue(parent,id,port,value);
				controller.sendConnectionNotification(id, port, 
						new BasicQualifiedValue(value,
								(quality.equalsIgnoreCase("good")?QualityCode.Good:QualityCode.Bad),
								new Date(time)));
				ProcessBlock block = diagram.getProcessBlock(uuid);
				postAlertingStatus(block);
				
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",CLSS,parent,id,iae.getMessage());
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
		log.tracef("%s.sendConnectionNotification - %s = %s on %s",CLSS,id,value.toString(),port);
		controller.sendConnectionNotification(id, port, new BasicQualifiedValue(value,
				(quality.equalsIgnoreCase("good")?QualityCode.Good:QualityCode.Bad),
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
		log.tracef("%s.sendConnectionNotification - %s = %s on %s",CLSS,id,value.toString(),port);
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
	public void sendTimestampedSignal(ProjectResourceId parent,String command,String message,String arg,long time)  {
		log.debugf("%s.sendLocalSignal - %s = %s %s %s ",CLSS,parent,command,message,arg);
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
	public void updateTag(ProjectResourceId diagId,String tagPath,String data,String quality,long time)  {
		if( tagPath==null || tagPath.isEmpty() ) return;   // Fail silently
		log.debugf("%s.updateTag - %s = %s %s %s %d ",CLSS,diagId.getResourcePath().getPath().toString(),tagPath,data,quality,time);

		QualityCode q = QualityCode.Good;
		if(!quality.equalsIgnoreCase("good")) q = QualityCode.Bad;
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