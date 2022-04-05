package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.RuntimeProject;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.project.DesignableProject;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * Reconcile any differences between the ModelManager and the ProjectManager.
 * Consider the project manager to be "correct". Edit the models to match.
 * This is used on the wicket status page.
 */
public class ProcessNodeSynchronizer {
    private final static String CLSS = "ProcessNodeSynchronizer";
    private final LoggerEx log;
    private final ModelManager modelManager;
    private final List<ProjectResource> resourcesDelete;
    private Map<String,ProjectResource> resourceMap;
    /**
     * Constructor.
     */
    public ProcessNodeSynchronizer() {
    	this.log = LogUtil.getLogger(getClass().getPackageName());
    	this.modelManager = BlockExecutionController.getInstance().getDelegate();
    	this.resourcesDelete = new ArrayList<>();
    	this.resourceMap = createResourceMap(modelManager.getContext());
    }

    /**
     * Iterate through the resources. Create process nodes for any that
     * are not backed up in the model.
     */
    public void createMissingResources() {
    	log.infof("%s.createMissingResources ========================== Create Missing Resources ==================================", CLSS);
    	Map<String,ProcessNode> nodeMap = modelManager.getNodesById();
    	for(String key:resourceMap.keySet()) {
    		if( nodeMap.get(key)==null) {
    			log.infof("%s.createMissingResources: ADDING node %s (project resource not represented)", CLSS,key);
    			modelManager.analyzeResource(resourceMap.get(key),true);
    		}
    	}
    	log.infof("%s.createMissingResources ============================     Complete    ====================================", CLSS);
    }
    
    /**
     * Iterate through the ProcessNodes known to the model.
     * Delete any that are not backed up by actual process resources.
   
    public void removeExcessNodes() {
    	log.infof("%s.removeExcessNodes ======================== Remove Excess Nodes ================================", CLSS);
    	resourcesDelete.clear();
    	Map<String,ProcessNode> nodeMap = modelManager.getNodesById();
    	for(String key:nodeMap.keySet()) {
    		if( resourceMap.get(key)==null ) {
    			resourcesDelete.add(nodeMap.get(key));
    			log.infof("%s.removeExcessNodes: DELETING node %s (not backed by project resource)", CLSS,key);
    		}
    	}
    	for(ProjectResource res:resourcesDelete) {;
    		modelManager.deleteResource(res.getResourceId());
    	}
    	log.infof("%s.removeExcessNodes ==========================       Complete      ==================================", CLSS);
    	// Refresh map of actual resources
    	resourceMap = createResourceMap(modelManager.getContext());
    }
     */
    
    /**
     * Iterate through the ProcessNodes known to the model. This includes families.
     * Delete any that do not have parents and are not root nodes.
     * Delete the accompanying resource. Hopefully this refers only
     * to a legacy issue and will never happen again.
     */
    public void removeOrphans() {
    	log.infof("%s.removeOrphans ======================== Removing Orphans ================================", CLSS);
    	resourcesDelete.clear();
    	Map<String,ProcessNode> nodesByKey = modelManager.getNodesById();
    	Collection<ProcessNode> nodes = nodesByKey.values();
    	RootNode root = modelManager.getRootNode();
    	for( ProcessNode child:nodes) {
    		if( !child.getResourceId().equals(root.getResourceId()) && modelManager.getProcessNode(child.getResourceId())==null ) {
    			ProjectResourceId resid = child.getResourceId();
    			resourcesDelete.add(resourceMap.get(ResourceKey.keyForResource(resid)));
    			log.infof("%s.removeOrphans: DELETING node %s:%s (has no parent)", CLSS,child.getProjectName(),child.getResourceId().getResourcePath().getFolderPath());
    		}
    	}
    	// Actually remove the resource.
    	for(ProjectResource res:resourcesDelete) {
    		modelManager.deleteResource(res.getResourceId());
    		// Delete the current node and all its children.
    		GatewayContext context = modelManager.getContext();
    		Optional<RuntimeProject> optional = context.getProjectManager().getProject(res.getProjectName());
    		DesignableProject project = (DesignableProject) optional.get();
    		if( project!=null ) {
    			try {
    				project.deleteResource(res.getResourceId()); // Mark as dirty
    			}
    			catch(ResourceNotFoundException rnfe) {
    				ProjectResourceId resid = res.getResourceId();
    				log.warnf("%s.removeOrphans: DELETING node %s:%s (%s)", CLSS,resid.getProjectName(),resid.getResourcePath().getFolderPath(),rnfe.getMessage());
    			}
    		}
    	}
    	// Refresh map of actual resources
    	resourceMap = createResourceMap(modelManager.getContext());
    	log.infof("%s.removeOrphans ==========================       Complete      ==================================", CLSS);
    }

    // Create 
    private Map<String,ProjectResource> createResourceMap(GatewayContext context) {
    	// We need to pass something serializable to the panel, thus the list of resources
    	// We also need to make sure this is up-to-date.
    	Map<String,ProjectResource> map = new HashMap<>();
    	List<String> projectNames = context.getProjectManager().getProjectNames();
    	for( String name:projectNames ) {
    		Optional<RuntimeProject> optional = context.getProjectManager().getProject(name);
    		Project project = optional.get();
    		if( !project.isEnabled() || project.getName().equals(Project.GLOBAL_PROJECT_NAME )) continue;
    		List<ProjectResource> reslist = project.getResources();
    		for( ProjectResource res:reslist ) {
    			if(res.getResourceId().getResourceType().getModuleId().equalsIgnoreCase(BLTProperties.MODULE_ID)) {
    				if( 	res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ||
    						res.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)   ) {
    					map.put(ResourceKey.keyForResource(res.getResourceId()),res);
    				}
    			}
    		}
    	}
    	return map;
    }
}
