/**
e *   (c) 2013-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.UUID;

import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessApplication;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.ProcessFamily;
import com.ils.blt.gateway.engine.ProcessNode;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * This class exposes python-callable requests directed at the execution engine. 
 * The class is accessed in Python via direct import 
 */
public class PythonRequestHandler   {
	private static final String TAG = "PythonRequestHandler";
	private static LoggerEx log = LogUtil.getLogger(PythonRequestHandler.class.getPackage().getName());
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	
	public PythonRequestHandler() {}
	/**
	 * Traverse the parent nodes until we find an Application. If there 
	 * are none in our ancestry, return null.
	 * 
	 * @param nodeId identifier for the node, a string version of a UUID
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
		if( !dbName.isEmpty() ) log.infof("%s.getDefaultDatabase: %s ",TAG,dbName);
		else                   log.warnf("%s.getDefaultDatabase: Database for diagram %s not found,",TAG,uuidString);
		return dbName;
	}
	/**
	 * @param parent identifier for the diagram, a string version of a UUID
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
	 * @param parent identifier for the block, a string version of a UUID
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

	/**
	 * Handle the block placing a new value on its output. The input may be PyObjects.
	 * 
	 * @param parent identifier for the parent, a string version of a UUID
	 * @param id block identifier a string version of the UUID
	 * @param port the output port on which to insert the result
	 * @param value the result of the block's computation
	 * @param quality of the reported output
	 */
	public void postValue(String parent,String id,String port,String value,String quality)  {
		log.infof("%s.postValue - %s = %s (%s) on %s",TAG,id,value.toString(),quality.toString(),port);
		
		try {
			UUID uuid = UUID.fromString(id);
			UUID parentuuid = UUID.fromString(parent);
			ControllerRequestHandler.getInstance().postValue(parentuuid,uuid,port,value,quality);
			controller.sendConnectionNotification(id, port, new BasicQualifiedValue(value));
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",TAG,parent,id,iae.getMessage());
		}
	}
	/**
	 * Broadcast a result to blocks in the diagram
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @param className name of the class of blocks to be signaled
	 * @param command the value of the signal
	 */
	public void sendLocalSignal(String parent,String command,String message,String arg)  {
		log.infof("%s.sendLocalSignal - %s = %s %s %s ",TAG,parent,command,message,arg);
		
		ControllerRequestHandler.getInstance().sendLocalSignal(parent,command,message,arg);
		
	}
}