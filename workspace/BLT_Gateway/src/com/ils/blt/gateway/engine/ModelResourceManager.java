/**
 *   (c) 2012-2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.block.ProcessBlock;
import com.ils.block.common.BindingType;
import com.ils.block.common.BlockProperty;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.connection.Connection;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.project.ProjectListener;

/**
 * The model resource manager keeps track of model (blt-model) resources. On startup
 * and whenever a resource change takes place, the manager analyzes the resources
 * and extracts diagram and block information. This information is relayed to the
 * block manager via a passed-in controller instance.
 * 
 * We make this a Singleton since access from the Gateway hook was problematic.
 * 
 * NOTE: The project listener interface only triggers when the user selects 
 *       "Save and Publish".  We provide separate entry points for application
 *       startup and for the user selecting "Save" from the Designer.
 *
 */
public class ModelResourceManager implements ProjectListener  {
	
	private static String TAG = "ModelResourceManager";
	private GatewayContext context = null;
	private final LoggerEx log;
	private final BlockExecutionController controller;
	/** The diagrams are keyed by projectId, then resourceID */
	private final Hashtable<Long,Hashtable<Long,ProcessDiagram>> models;
	
	/**
	 * Initially we query the gateway context to discover what resources exists. After that
	 * we rely on notifications of project resource updates. After discovering block resources
	 * we deserialize and inform the BlockExecutionController.
	 * 
	 * @param cntx the gateway context. 
	 */
	public ModelResourceManager(BlockExecutionController c) { 
		this.controller = c;
		log = LogUtil.getLogger(getClass().getPackage().getName());
		models = new Hashtable<Long,Hashtable<Long,ProcessDiagram>>();
	}

	
	public void setContext(GatewayContext cntx) {
		this.context = cntx;
		context.getProjectManager().addProjectListener(this);
	}
	
	/**
	 * Set a ProcessDiagram for a resource. There is a one-one correspondence 
	 * between a model-project and diagram.
	 * @param projectId the identity of a project
	 * @param resourceId the identity of the model resource
	 * @param model the diagram logic
	 */
	public void addResource(Long projectId,Long resourceId,ProcessDiagram model) {
		Hashtable<Long,ProcessDiagram> projectModels = models.get(projectId);
		if( projectModels==null ) {
			projectModels = new Hashtable<Long,ProcessDiagram>();
			models.put(projectId,projectModels);
		}
		projectModels.put(resourceId, model);
	}
	
	public void deleteResources(Long projectId) {
		Hashtable<Long,ProcessDiagram> diagrams = models.get(projectId);
		for(ProcessDiagram diagram:diagrams.values()) {
			for(ProcessBlock block:diagram.getProcessBlocks() ) {
				for(BlockProperty bp:block.getProperties()) {
					controller.stopSubscription(block, bp.getName());
				}	
			}
		}
		models.remove(projectId);
	}
	
	/**
	 * Remove a diagram within a project.
	 * Presumably the diagram has been deleted.
	 * @param projectId the identity of a project.
	 */
	public void deleteResource(Long projectId,Long resourceId) {
		Hashtable<Long,ProcessDiagram> projectModel = models.get(projectId);
		if( projectModel!= null ) {
			ProcessDiagram diagram = projectModel.get(resourceId);
			if( diagram!=null ) {
				for(ProcessBlock block:diagram.getProcessBlocks() ) {
					for(BlockProperty bp:block.getProperties()) {
						controller.stopSubscription(block, bp.getName());
					}
				}
			}
			projectModel.remove(resourceId);
		}
	}
	
	
	/**
	 * Get a block from an existing diagrams. 
	 * @param projectId
	 * @param resourceId
	 * @param blockId
	 * @return the specified ProcessBlock. If not found, return null. 
	 */
	public ProcessBlock getBlock(Long projectId,Long resourceId,UUID blockId) {
		ProcessBlock block = null;
		Hashtable<Long,ProcessDiagram> projectModels = models.get(projectId);
		if( projectModels!=null ) {
			ProcessDiagram dm = projectModels.get(resourceId);
			if( dm!=null ) {
				block = dm.getBlock(blockId);
			}
		}
		return block;
	}
	
	/**
	 * Get a connection from the existing diagrams. 
	 * @param projectId
	 * @param resourceId
	 * @param connectionId
	 * @return the specified Connection. If not found, return null. 
	 */
	public Connection getConnection(long projectId,long resourceId,String connectionId) {
		Connection cxn = null;
		Hashtable<Long,ProcessDiagram> projectModels = models.get(new Long(projectId));
		if( projectModels!=null ) {
			ProcessDiagram dm = projectModels.get(new Long(resourceId));
			if( dm!=null ) {
				cxn = dm.getConnection(connectionId);
			}
		}
		return cxn;
	}
	
	/**
	 * Get a connection from the existing diagrams. 
	 * @param projectId
	 * @param resourceId
	 * @param connectionId
	 * @return the specified Connection. If not found, return null. 
	 */
	public ProcessDiagram getDiagram(Long projectId,Long resourceId) {
		ProcessDiagram diagram = null;
		Hashtable<Long,ProcessDiagram> processDiagrams = models.get(projectId);
		if( processDiagrams!=null ) {
			diagram = processDiagrams.get(resourceId);
		}
		return diagram;
	}
	
	public void updateModel(long projectId) {
		Project project = context.getProjectManager().getProject(projectId, ApplicationScope.DESIGNER, ProjectVersion.Staging);
		if( project==null) {
			log.errorf("%s: updateModel: No project found with Id %d",TAG,projectId);
			return;
		}
		List<ProjectResource> resources = project.getResources();
		for (ProjectResource res : resources) {
			if( res.getResourceType().equals(BLTProperties.MODEL_RESOURCE_TYPE)) {
				log.tracef("%s: projectUpdated: found model resource %s (%d)",TAG,res.getName(),res.getResourceId());
				ProcessDiagram diagram = deserializeModelResource(projectId,res);
				if( diagram!=null) {
					addResource(new Long(projectId), new Long(res.getResourceId()), diagram);
				}
				else {
					log.warnf("%s: Failed to create DOM from resource",TAG);
				}
			}
		}
	}

	/**
	 *  We've discovered a changed model resource. Deserialize and convert into a ProcessDiagram.
	 *  Note: We had difficulty with the Ignition XML serializer because it didn't handle Java generics;
	 *        thus the use of JSON. The returned object was not an instanceof...
	 * @param res
	 */ 
	private ProcessDiagram deserializeModelResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		String json = new String(serializedObj);
		log.infof("%s: deserializeModelResource: json = %s",TAG,json);
		ProcessDiagram diagram = null;
		try{
			ObjectMapper mapper = new ObjectMapper();
			SerializableDiagram sd = mapper.readValue(json, SerializableDiagram.class);
			if( sd!=null ) {
				log.infof("%s: deserializeModelResource: successfully deserialized diagram %s",TAG,sd.getName());
				diagram = new ProcessDiagram(sd,projId,res.getResourceId());
				log.infof("%s: deserializeModelResource: starting tag subscriptions ...",TAG);
				for( ProcessBlock block:diagram.getProcessBlocks()) {
					for(BlockProperty property:block.getProperties()) {
						controller.startSubscription(block, property);
					}
				}
			}
			else {
				log.warnf("%s: deserializeModelResource: deserialization failed",TAG);
			}
	
		}
		// Print stack trace
		catch( Exception ex) {
			log.warnf("%s: deserializeModelResource: exception (%s)",TAG,ex.getLocalizedMessage(),ex);
		}
		return diagram;

	}
	
	// ====================== Project Listener Interface ================================
	/**
	 * We don't care if the new project is a staging or published version.
	 * Analyze either project resources and update the controller.
	 */
	@Override
	public void projectAdded(Project staging, Project published) {
		if( staging!=null ) {
			long projectId = published.getId();
			log.debugf("%s: projectAdded - published project %d", TAG,projectId);
			List<ProjectResource> resources = published.getResources();
			for( ProjectResource res:resources ) {
				if( res.getResourceType().equalsIgnoreCase(BLTProperties.MODEL_RESOURCE_TYPE)) {
					log.debugf("%s: projectAdded - published %s %d = %s", TAG,res.getName(),
						res.getResourceId(),res.getResourceType());
					ProcessDiagram diagram = deserializeModelResource(projectId,res);
					if( diagram!=null) {
						addResource(new Long(projectId), new Long(res.getResourceId()), diagram);
						for( ProcessBlock pb:diagram.getProcessBlocks()) {
							for(BlockProperty bp:pb.getProperties()) {
								if( bp.getBindingType()==BindingType.TAG && bp.getBinding()!=null ) {
									controller.startSubscription(pb,bp);
								}
							}
						}
					}
					else {
						log.warnf("%s: projectAdded - Failed to create DOM from resource",TAG);
					}
				}
			}
		}
		
	}
	/**
	 * Assume that the project resources are already gone. This is a cleanup step.
	 */
	@Override
	public void projectDeleted(long projectId) {
		deleteResources(new Long(projectId));
		
	}
	/**
	 * Handle project resource updates of type model.
	 * @param diff represents differences to the updated project. That is any updated, dirty or deleted resources.
	 * @param vers a value of "Staging" means is a result of a "Save". A value of "Published" occurs when a 
	 *        project is published. For our purposes both actions are equivalent.
	 */
	/* (non-Javadoc)
	 * @see com.inductiveautomation.ignition.gateway.project.ProjectListener#projectUpdated(com.inductiveautomation.ignition.common.project.Project, com.inductiveautomation.ignition.common.project.ProjectVersion)
	 */
	@Override
	public void projectUpdated(Project diff, ProjectVersion vers) { 
		log.debugf("%s: projectUpdated - version = %s", TAG,vers.toString());
		if( vers==ProjectVersion.Published ) return;  // Consider only the "Staging" version
		
		long projectId = diff.getId();
		Set<Long> deleted = diff.getDeletedResources();
		for (Long  resid : deleted) {
			deleteResource(projectId,resid);
		}
		
		log.debugf("%s: projectUpdated - res of type model = %d", TAG,diff.getResourcesOfType(BLTProperties.MODULE_ID, BLTProperties.MODEL_RESOURCE_TYPE).size());
		List<ProjectResource> resources = diff.getResourcesOfType(BLTProperties.MODULE_ID, BLTProperties.MODEL_RESOURCE_TYPE);
		for( ProjectResource res:resources ) {
			log.debugf("%s: projectUpdated -  %s %d = %s (%s)", TAG,res.getName(),
					res.getResourceId(),res.getResourceType(),(diff.isResourceDirty(res)?"dirty":"clean"));
			ProcessDiagram diagram = deserializeModelResource(projectId,res);
			if( diagram!=null) {
				addResource(new Long(projectId), new Long(res.getResourceId()), diagram);
			}
			else {
				log.warnf("%s: projectUpdated - Failed to create DOM from resource",TAG);
			}


		}
	}
	
}
