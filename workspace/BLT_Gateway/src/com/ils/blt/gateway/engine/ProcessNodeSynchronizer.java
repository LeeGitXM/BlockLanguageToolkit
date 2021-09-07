package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.ils.blt.common.BLTProperties;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.RuntimeProject;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * Reconcile any differences between the ModelManager and the ProjectManager.
 * Consider the project manager to be "correct". Edit the models to match.
 * This is used on the wicket status page.
 */
public class ProcessNodeSynchronizer {
    private final static String CLSS = "ProcessNodeSynchronizer";
    private final ILSLogger log;
    private final ModelManager modelManager;
    private final List<ProjectResourceKey> nodesToDelete;
    private final Map<ProjectResourceKey,ProjectResource> resourceMap;
    /**
     * Constructor.
     */
    public ProcessNodeSynchronizer() {
    	this.log = LogMaker.getLogger(this);
    	this.modelManager = BlockExecutionController.getInstance().getDelegate();
    	this.nodesToDelete = new ArrayList<>();
    	this.resourceMap = createResourceMap(modelManager.getContext());
    }

    /**
     * Iterate through the resources. Create process nodes for any that
     * are not backed up in the model.
     */
    public void createMissingResources() {
    	log.infof("%s.createMissingResources ========================== Create Missing Resources ==================================", CLSS);
    	Map<ProjectResourceKey,ProcessNode> nodeMap = modelManager.getNodesByKey();
    	for(ProjectResourceKey key:resourceMap.keySet()) {
    		if( nodeMap.get(key)==null) {
    			log.infof("%s.createMissingResources: ADDING node %d:%d %s (project resource not represented)", CLSS,key.getProjectName(),key.getResourceId(),
    					resourceMap.get(key).getProjectName());
    			modelManager.analyzeResource(resourceMap.get(key),true);
    		}
    	}
    	log.infof("%s.createMissingResources ============================     Complete    ====================================", CLSS);
    }
    
    /**
     * Iterate through the ProcessNodes known to the model.
     * Delete any that are not backed up by actual process resources.
     */
    public void removeExcessNodes() {
    	log.infof("%s.removeExcessNodes ======================== Remove Excess Nodes ================================", CLSS);
    	nodesToDelete.clear();
    	Map<ProjectResourceKey,ProcessNode> nodeMap = modelManager.getNodesByKey();
    	for(ProjectResourceKey key:nodeMap.keySet()) {
    		if( resourceMap.get(key)==null ) {
    			nodesToDelete.add(key);
    			log.infof("%s.removeExcessNodes: DELETING node %d:%d (not backed by project resource)", CLSS,key.getProjectName(),key.getResourceId());
    		}
    	}
    	for(ProjectResourceKey key:nodesToDelete) {
    		modelManager.deleteResource(key.getResourceId());
    	}
    	log.infof("%s.removeExcessNodes ==========================       Complete      ==================================", CLSS);
    }
    
    /**
     * Iterate through the ProcessNodes known to the model. This includes families.
     * Delete any that do not have parents and are not root nodes.
     * Delete the accompanying resource. Hopefully this refers only
     * to a legacy issue and will never happen again.
     */
    public void removeOrphans() {
    	log.infof("%s.removeOrphans ======================== Removing Orphans ================================", CLSS);
    	nodesToDelete.clear();
    	Map<ProjectResourceKey,ProcessNode> nodesByKey = modelManager.getNodesByKey();
    	Collection<ProcessNode> nodes = nodesByKey.values();
    	RootNode root = modelManager.getRootNode();
    	for( ProcessNode child:nodes) {
    		if( !child.getResourceId().equals(root.getResourceId()) && modelManager.getProcessNode(child.getResourceId())==null ) {
    			ProjectResourceKey key = new ProjectResourceKey(child.getResourceId());
    			nodesToDelete.add(key);
    			log.infof("%s.removeOrphans: DELETING node %d:%d (has no parent)", CLSS,key.getProjectName(),key.getResourceId());
    		}
    	}
    	// Actually remove the resource.
    	for(ProjectResourceKey key:nodesToDelete) {
    		modelManager.deleteResource(key.getResourceId());
    		// Delete the current node and all its children.
    		GatewayContext context = modelManager.getContext();
    		Optional<RuntimeProject> optional = context.getProjectManager().getProject(key.getProjectName());
    		Project project = optional.get();
    		if( project!=null ) {
    			project.deleteResource(key.getResourceId(), true); // Mark as dirty
    			try {
    				context.getProjectManager().saveProject(project, null, null, "Removing orphan resource", false);
    			}
    			catch(Exception ex) {
    				log.warnf("%s.removeOrphans: Failed to save project when deleting node %d:%d (%s)", CLSS,key.getProjectName(),key.getResourceId(),
    						   ex.getMessage());
    			}
    		}

    	}
    	log.infof("%s.removeOrphans ==========================       Complete      ==================================", CLSS);
    }

    private Map<ProjectResourceKey,ProjectResource> createResourceMap(GatewayContext context) {
    	// We need to pass something serializable to the panel, thus the list of resources
    	// We also need to make sure this is up-to-date.
    	Map<ProjectResourceKey,ProjectResource> map = new HashMap<>();
    	List<String> projectNames = context.getProjectManager().getProjectNames();
    	for( String name:projectNames ) {
    		Optional<RuntimeProject> optional = context.getProjectManager().getProject(name);
    		Project project = optional.get();
    		if( !project.isEnabled() || project.getName().equals(Project.GLOBAL_PROJECT_NAME )) continue;
    		List<ProjectResource> reslist = project.getResources();
    		for( ProjectResource res:reslist ) {
    			if(res.getResourceId().getResourceType().getModuleId().equalsIgnoreCase(BLTProperties.MODULE_ID)) {
    				if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE) || 
    						res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ||
    						res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE) ||
    						res.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)   ) {
    					map.put(new ProjectResourceKey(res.getResourceId()),res);
    				}
    			}
    		}
    	}
    	return map;
    }
}
