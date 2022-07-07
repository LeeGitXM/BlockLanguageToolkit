/**
e *   (c) 2013-2022  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.Date;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
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
import com.inductiveautomation.ignition.common.project.resource.ResourceType;
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

	public ProjectResourceId createResourceId(String projectName, String path) {
		ProjectResourceId resourceId = new ProjectResourceId(projectName,BLTProperties.DIAGRAM_RESOURCE_TYPE,path);
		return resourceId;
	}
	/**
	 * Find a block given the resourcePath of the parent diagram and the UUID of the block itself.
	 * 
	 * @param projectName project for the diagram
	 * @param resource path, a string
	 * @param blockId identifier for the block, a string version of a UUID
	 * @return the referenced block
	 */
	public ProcessBlock getBlock(String projectName,String resourcePath,String blockId)  {
		log.tracef("%s.getBlock, diagram.block = %s.%s ",CLSS,resourcePath,blockId);
		ProcessBlock block = null;
		try {
			ProcessNode node = controller.getProcessNode(projectName,resourcePath);
			if( node instanceof ProcessDiagram ) {
				UUID uuid = UUID.fromString(blockId);
				ProcessDiagram diag = (ProcessDiagram)node;
				block = diag.getProcessBlock(uuid);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getBlock: on of %s or %s is an illegal UUID (%s)",CLSS,resourcePath,blockId,iae.getMessage());
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
	 *
	 * @param projectName project for the diagram
	 * @param resource path, a string
	 * @return the default database for the project containing this node
	 */
	public String getDefaultDatabase(String projectName,String resourcePath)  {
		String dbName = "";

		ProcessNode node = controller.getProcessNode(projectName,resourcePath);
		while( node!=null) {
			if(node instanceof ProcessDiagram ) {
				ProcessDiagram diagram = (ProcessDiagram)node;
				if( diagram.getState().equals(DiagramState.ISOLATED)) dbName = controller.getProjectIsolationDatabase(projectName);
				else dbName = controller.getProjectProductionDatabase(projectName);
				break;
			}
			node = controller.getParentNode(node);
		}

		if( !dbName.isEmpty() ) log.debugf("%s.getDefaultDatabase: %s ",CLSS,dbName);
		else                   log.warnf("%s.getDefaultDatabase: Database for diagram %s not found,",CLSS,resourcePath);
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
	 * @param projectName project for the diagram
	 * @param resource path, a string
	 * @return the default tag provider for the project associated with 
	 *         the specified diagram
	 */
	public String getDefaultTagProvider(String projectName,String resourcePath)  {
		log.tracef("%s.getDefaultTagProvider, node = %s ",CLSS,resourcePath);
		String provider = "";
		try {
			ProcessNode node = controller.getProcessNode(projectName,resourcePath);
			while(  node!=null) {
				if(node instanceof ProcessDiagram ) {
					ProcessDiagram diagram = (ProcessDiagram)node;
					if( diagram.getState().equals(DiagramState.ISOLATED)) provider = controller.getProjectIsolationProvider(projectName);
					else provider = controller.getProjectProductionProvider(projectName);
					break;
				}
				node = controller.getParentNode(node);
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDefaultTagProvider: %s is an illegal UUID (%s)",CLSS,resourcePath,iae.getMessage());
		}
		if( provider.isEmpty() ) log.warnf("%s.getDefaultTagProvider: Provider for diagram %s not found,",CLSS,resourcePath);
		return provider;
	}
	/**
	 * Given an identifier string, return the associated diagram. 
	 * The parent of a block should be a diagram.
	 * @param projectName project for the diagram
	 * @param resource path
	 * @return the diagram
	 */
	public ProcessDiagram getDiagram(String projectName,String resourcePath)  {
		log.tracef("%s.getDiagram, diagram = %s ",CLSS,resourcePath);
		ProcessDiagram result = null;
		try {
			ProcessNode node = controller.getProcessNode(projectName,resourcePath);
			if( node instanceof ProcessDiagram ) {
				result = (ProcessDiagram)node;
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDiagram: %s is an illegal UUID (%s)",CLSS,resourcePath,iae.getMessage());
		}
		return result;
	}

	/**
	 * Handle the block placing a new value on its output. The input may be PyObjects.
	 * This method is not "test-time" aware.
	 * 
	 * @param projectName project for the diagram
	 * @param resource path
	 * @return the diagram
	 * @param id block identifier a string version of the UUID
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 * @param time timestamp of the notification
	 */
	public void postValue(String projectName,String resourcePath,String id,String port,String value,String quality,long time)  {
		log.debugf("%s.postValue - %s = %s (%s) on %s",CLSS,id,value.toString(),quality.toString(),port);
		
		try {
			UUID uuid = UUID.fromString(id);
			ProcessDiagram diagram = getDiagram(projectName,resourcePath); 
			// Do nothing if diagram is disabled
			if( diagram!=null && !diagram.getState().equals(DiagramState.DISABLED)) {
				ControllerRequestHandler.getInstance().postValue(projectName,resourcePath,id,port,value);
				controller.sendConnectionNotification(id, port, 
						new BasicQualifiedValue(value,
								(quality.equalsIgnoreCase("good")?QualityCode.Good:QualityCode.Bad),
								new Date(time)));
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",CLSS,resourcePath,id,iae.getMessage());
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
	 * @param projectName project for the diagram
	 * @param resource path
	 * @param command the value of the signal
	 * @param message text of the signal
	 * @param arg an argument
	 * @param time the time associated with this signal
	 */
	public void sendTimestampedSignal(String projectName,String resourcePath,String command,String message,String arg,long time)  {
		log.debugf("%s.sendLocalSignal - %s = %s %s %s ",CLSS,resourcePath,command,message,arg);
		ProjectResourceId diagId = new ProjectResourceId(projectName,BLTProperties.DIAGRAM_RESOURCE_TYPE,resourcePath);
		ControllerRequestHandler.getInstance().sendTimestampedSignal(diagId,command,message,arg,time);
		
	}

	/**
	 * Write a value to the named tag. The provider is the provider appropriate to
	 * the referenced diagram
	 * 
	 * @param projectName project for the diagram
	 * @param resource path, a string
	 * @param tagPath path to the tag
	 * @param data the value to be written
	 * @param quality the quality of the output
	 * @param time the time associated with this write operation
	 */
	public void updateTag(String projectName,String resourcePath,String tagPath,String data,String quality,long time)  {
		if( tagPath==null || tagPath.isEmpty() ) return;   // Fail silently
		log.debugf("%s.updateTag - %s = %s %s %s %d ",CLSS,resourcePath,tagPath,data,quality,time);

		QualityCode q = QualityCode.Good;
		if(!quality.equalsIgnoreCase("good")) q = QualityCode.Bad;
		ProjectResourceId diagId = new ProjectResourceId(projectName,BLTProperties.DIAGRAM_RESOURCE_TYPE,resourcePath);
		QualifiedValue qv = new BasicQualifiedValue(data,q,new Date(time));
		controller.updateTag(diagId, tagPath, qv);
	}
}