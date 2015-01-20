/**
e *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway;

import java.util.UUID;

import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ProcessApplication;
import com.ils.blt.gateway.engine.ProcessDiagram;
import com.ils.blt.gateway.engine.ProcessFamily;
import com.ils.blt.gateway.engine.ProcessNode;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * This class exposes python-callable requests directed at the execution engine. 
 * The class is accessed in Python via direct import 
 */
public class PythonRequestHandler   {
	private static final String TAG = "PythonRequestHandler";
	private static LoggerEx log = LogUtil.getLogger(PythonRequestHandler.class.getPackage().getName());
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	private static GatewayContext context = null;
	
	public PythonRequestHandler() {
		
	}
	/**
	 * Traverse the parent nodes until we find an Application. If there 
	 * are none in our ancestry, return null.
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @return the ancestrial application
	 */
	public ProcessApplication getApplication(String parent)  {
		log.infof("%s.getApplication, diagram = %s ",TAG,parent);
		ProcessApplication app = null;
		try {
			UUID parentuuid = UUID.fromString(parent);
			
			ProcessNode node = controller.getProcessNode(parentuuid);
			while( node!=null ) {
				if( node instanceof ProcessApplication ) {
					app = (ProcessApplication)node;
					break;
				}
				node = controller.getProcessNode(node.getParent());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getApplication: %s is an illegal UUID (%s)",TAG,parent,iae.getMessage());
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
		log.infof("%s.getBlock, diagram.block = %s.%s ",TAG,parent,blockId);
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
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @return the default database for the project containing this diagram
	 */
	public String getDefaultDatabase(String parent)  {
		log.infof("%s.getDefaultDatabase, diagram = %s ",TAG,parent);
		String dbName = null;
		try {
			UUID parentuuid = UUID.fromString(parent);
			ProcessNode node = controller.getProcessNode(parentuuid);
			if( node instanceof ProcessDiagram ) {
					ProcessDiagram diag = (ProcessDiagram)node;
					if( diag!=null) {
						long projectId = diag.getProjectId();
						dbName = context.getProjectManager().getProps(projectId, ProjectVersion.Published).getDefaultDatasourceName();
					}
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDefaultDatabase: %s is an illegal UUID (%s)",TAG,parent,iae.getMessage());
		}
		return dbName;
	}
	/**
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @return the default tag provider for the project associated with 
	 *         the specified diagram
	 */
	public String getDefaultTagProvider(String parent)  {
		log.infof("%s.getDefaultTagProvider, diagram = %s ",TAG,parent);
		String provider = null;
		try {
			UUID parentuuid = UUID.fromString(parent);
			ProcessNode node = controller.getProcessNode(parentuuid);
			if( node instanceof ProcessDiagram ) {
				ProcessDiagram diag = (ProcessDiagram)node;
				if( diag!=null) {
					long projectId = diag.getProjectId();
					provider = context.getProjectManager().getProps(projectId, ProjectVersion.Published).getDefaultSQLTagsProviderName();
				}
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDefaultTagProvider: %s is an illegal UUID (%s)",TAG,parent,iae.getMessage());
		}
		return provider;
	}
	/**
	 * Given an identifier string, return the associated diagram. 
	 * The parent of a block should be a diagram.
	 * 
	 * @param parent identifier for the block, a string version of a UUID
	 * @return the diagram
	 */
	public ProcessDiagram getDiagram(String parent)  {
		log.infof("%s.getDiagram, diagram = %s ",TAG,parent);
		ProcessDiagram diag = null;
		try {
			UUID parentuuid = UUID.fromString(parent);
			ProcessNode node = controller.getProcessNode(parentuuid);
			if( node instanceof ProcessDiagram ) {
					diag = (ProcessDiagram)node;
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getDiagram: %s is an illegal UUID (%s)",TAG,parent,iae.getMessage());
		}
		return diag;
	}
	/**
	 * Traverse the parent nodes until we find a family. If there 
	 * are none in our ancestry, return null.
	 * 
	 * @param parent identifier for the diagram, a string version of a UUID
	 * @return the ancestrial family
	 */
	public ProcessFamily getFamily(String parent)  {
		log.infof("%s.getFamily, diagram = %s ",TAG,parent);
		ProcessFamily fam = null;
		try {
			UUID parentuuid = UUID.fromString(parent);
			ProcessNode node = controller.getProcessNode(parentuuid);
			while( node!=null ) {
				if( node instanceof ProcessFamily ) {
					fam = (ProcessFamily)node;
					break;
				}
				node = controller.getProcessNode(node.getParent());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.getFamily: %s is an illegal UUID (%s)",TAG,parent,iae.getMessage());
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
	
	public static void setContext(GatewayContext ctx) { context=ctx;}
}