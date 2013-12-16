/**
 *   (c) 2012-2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.List;
import java.util.Set;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.common.xmlserialization.deserialization.DeserializationContext;
import com.inductiveautomation.ignition.common.xmlserialization.deserialization.XMLDeserializer;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.project.ProjectListener;

/**
 * The workspace manager keeps track of model (blt-model) resources. On startup
 * and whenever a resource change takes place, the manager analyzes the resources
 * and extracts workspace and block information. This information is relayed to the
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
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	private static ModelResourceManager instance = null;
	
	/**
	 * Initially we query the gateway context to discover what resources exists. After that
	 * we rely on notifications of project resource updates. After discovering block resources
	 * we deserialize and inform the BlockExecutionController.
	 * 
	 * @param cntx the gateway context. 
	 */
	private ModelResourceManager() { 
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static ModelResourceManager getInstance() {
		if( instance==null) {
			synchronized(ModelResourceManager.class) {
				instance = new ModelResourceManager();
			}
		}
		return instance;
	}
	
	public void setContext(GatewayContext cntx) {
		this.context = cntx;
	}
	
	public void updateModel(BlockExecutionController controller,long projectId) {
		Project project = context.getProjectManager().getProject(projectId, ApplicationScope.DESIGNER, ProjectVersion.Staging);
		if( project==null) {
			log.errorf("%s: updateModel: No project found with Id %d",TAG,projectId);
			return;
		}
		List<ProjectResource> resources = project.getResources();
		for (ProjectResource res : resources) {
			if( res.getResourceType().equals(BLTProperties.MODEL_RESOURCE_TYPE)) {
				log.infof("%s: projectUpdated: found model resource %s (%d)",TAG,res.getName(),res.getResourceId());
				SerializableDiagram diagram = deserializeModelResource(res);
				if( diagram!=null) {
					DiagramModel dm = new DiagramModel(diagram,projectId,res.getResourceId());
					controller.addResource(new Long(projectId), new Long(res.getResourceId()), dm);
				}
				else {
					log.warnf("%s: Failed to create DOM from resource",TAG);
				}
			}
		}
	}

	/**
	 *  We've discovered a changed model resource. Deserialize and convert into a CommonDiagram.
	 * @param res
	 */ 
	private SerializableDiagram deserializeModelResource(ProjectResource res) {
		byte[] serializedObj = res.getData();
		SerializableDiagram diagram = null;
		try{
			XMLDeserializer deserializer = context.createDeserializer();
			DeserializationContext decon = deserializer.deserializeBinary(serializedObj);
			Object obj = decon.getRootObjects().get(0);
			if( obj instanceof SerializableDiagram) diagram = (SerializableDiagram)obj;
	
		}
		catch( Exception ex) {
			log.warnf("%s: deserializeModelResource: exception (%s)",TAG,ex.getLocalizedMessage());
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
			long projectId = staging.getId();
			List<ProjectResource> resources = staging.getResources();
			for( ProjectResource res:resources ) {
				if( res.getResourceType().equalsIgnoreCase(BLTProperties.MODEL_RESOURCE_TYPE)) {
					log.infof("%s: projectAdded - staging %s %d = %s", TAG,res.getName(),
						res.getResourceId(),res.getResourceType());
					SerializableDiagram diagram = deserializeModelResource(res);
					if( diagram!=null) {
						DiagramModel dm = new DiagramModel(diagram,projectId,res.getResourceId());
						controller.addResource(new Long(projectId), new Long(res.getResourceId()), dm);
					}
					else {
						log.warnf("%s: Failed to create DOM from resource",TAG);
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
		controller.deleteResources(new Long(projectId));
		
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
		long projectId = diff.getId();
		Set<Long> deleted = diff.getDeletedResources();
		for (Long  resid : deleted) {
			controller.deleteResource(projectId,resid);
		}
		
		// The "dirty" ones are the new ones ??
		List<ProjectResource> resources = diff.getDirtyResources();
		for( ProjectResource dres:resources ) {
			log.infof("%s: projectUpdated - dirty %s %d = %s", TAG,dres.getName(),
					dres.getResourceId(),dres.getResourceType());
			if( dres.getResourceType().equalsIgnoreCase(BLTProperties.MODEL_RESOURCE_TYPE)) {
			log.infof("%s: projectUpdated - dirty %s %d = %s", TAG,dres.getName(),
					dres.getResourceId(),dres.getResourceType());
			}
		}
		resources = diff.getResources();   // Do these include the dirty?
		for( ProjectResource res:resources ) {
			if( res.getResourceType().equalsIgnoreCase(BLTProperties.MODEL_RESOURCE_TYPE)) {
				log.infof("%s: projectUpdated - updated %s %d = %s", TAG,res.getName(),
					res.getResourceId(),res.getResourceType());
				SerializableDiagram diagram = deserializeModelResource(res);
				if( diagram!=null) {
					DiagramModel dm = new DiagramModel(diagram,projectId,res.getResourceId());
					controller.addResource(new Long(projectId), new Long(res.getResourceId()), dm);
				}
				else {
					log.warnf("%s: Failed to create DOM from resource",TAG);
				}
			}
			
		}
	}
}
