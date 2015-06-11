/**
e *   (c) 2013-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.schematic;

import java.util.UUID;

import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.CoreBlock;
import com.ils.blt.gateway.common.BasicDiagram;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessApplication;
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
	private final SchematicRequestHandler requestHandler;
	
	public PythonRequestHandler(SchematicRequestHandler handler) {
		this.requestHandler = handler;
	}
	
	/**
	 * Find a block given the Id of the parent diagram the block itself.
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @param blockId identifier for the block, a string version of a UUID
	 * @return the referenced block
	 */
	public CoreBlock getBlock(String parent,String blockId)  {
		log.tracef("%s.getBlock, diagram.block = %s.%s ",TAG,parent,blockId);
		CoreBlock block = null;
		try {
			UUID parentuuid = UUID.fromString(parent);
			ProcessNode node = controller.getProcessNode(parentuuid);
			if( node instanceof BasicDiagram ) {
				UUID uuid = UUID.fromString(blockId);
				BasicDiagram diag = (BasicDiagram)node;
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
	public String getBlockId(BasicDiagram diagram,String blockName)  {
		String id = null;
		for( CoreBlock block:diagram.getDiagramBlocks() ) {
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
				if(node instanceof BasicDiagram ) {
					BasicDiagram diagram = (BasicDiagram)node;
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
				if(node instanceof BasicDiagram ) {
					BasicDiagram diagram = (BasicDiagram)node;
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
	public BasicDiagram getDiagram(String diagramId)  {
		log.tracef("%s.getDiagram, diagram = %s ",TAG,diagramId);
		BasicDiagram diag = null;
		try {
			UUID diagramuuid = UUID.fromString(diagramId);
			ProcessNode node = controller.getProcessNode(diagramuuid);
			if( node instanceof BasicDiagram ) {
					diag = (BasicDiagram)node;
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDiagram: %s is an illegal UUID (%s)",TAG,diagramId,iae.getMessage());
		}
		return diag;
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
		log.debugf("%s.postValue - %s = %s (%s) on %s",TAG,id,value.toString(),quality.toString(),port);
		
		try {
			UUID uuid = UUID.fromString(id);
			UUID parentuuid = UUID.fromString(parent);
			requestHandler.postValue(parentuuid,uuid,port,value,quality);
			controller.sendConnectionNotification(id, port, new BasicQualifiedValue(value));
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.postValue: one of %s or %s illegal UUID (%s)",TAG,parent,id,iae.getMessage());
		}
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
	public void sendConnectionNotification(String id, String port, String value)  {
		log.tracef("%s.sendConnectionNotification - %s = %s on %s",TAG,id,value.toString(),port);
		controller.sendConnectionNotification(id, port, new BasicQualifiedValue(value));
	}
	/**
	 * Broadcast a result to blocks in the diagram
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @param className name of the class of blocks to be signaled
	 * @param command the value of the signal
	 */
	public void sendLocalSignal(String parent,String command,String message,String arg)  {
		log.debugf("%s.sendLocalSignal - %s = %s %s %s ",TAG,parent,command,message,arg);
		
		requestHandler.sendLocalSignal(parent,command,message,arg);
		
	}
}