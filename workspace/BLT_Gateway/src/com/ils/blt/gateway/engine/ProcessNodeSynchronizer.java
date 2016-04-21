package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * Reconcile any differences between the ModelManager and the ProjectManager.
 * Consider the project manager to be "correct". Edit the models to match.
 * This is used on the wicket status page.
 */
public class ProcessNodeSynchronizer {
    private final static String TAG = "ProcessNodeSynchronizer";
    private final LoggerEx log;
    private final ModelManager modelManager;
    private final List<ProjectResourceKey> nodesToDelete;
    private final Map<ProjectResourceKey,ProjectResource> resourceMap;
    /**
     * Constructor.
     */
    public ProcessNodeSynchronizer() {
    	this.log = LogUtil.getLogger(getClass().getPackage().getName());
    	this.modelManager = BlockExecutionController.getInstance().getDelegate();
    	this.nodesToDelete = new ArrayList<>();
    	this.resourceMap = createResourceMap(modelManager.getContext());
    }

    /**
     * Iterate through the resources. Create process nodes for any that
     * are not backed up in the model.
     */
    public void createMissingResources() {
    	log.infof("%s.createMissingResources ========================== Create Missing Resources ==================================", TAG);
    	Map<ProjectResourceKey,ProcessNode> nodeMap = modelManager.getNodesByKey();
    	for(ProjectResourceKey key:resourceMap.keySet()) {
    		if( nodeMap.get(key)==null) {
    			log.infof("%s.createMissingResources: ADDING node %d:%d %s (project resource not represented)", TAG,key.getProjectId(),key.getResourceId(),
    					resourceMap.get(key).getName());
    			modelManager.analyzeResource(key.getProjectId(), resourceMap.get(key));
    		}
    	}
    	log.infof("%s.createMissingResources ============================     Complete    ====================================", TAG);
    }
    
    /**
     * Iterate through the ProcessNodes known to the model.
     * Delete any that are not backed up by actual process resources.
     */
    public void removeExcessNodes() {
    	log.infof("%s.removeExcessNodes ======================== Remove Excess Nodes ================================", TAG);
    	nodesToDelete.clear();
    	Map<ProjectResourceKey,ProcessNode> nodeMap = modelManager.getNodesByKey();
    	for(ProjectResourceKey key:nodeMap.keySet()) {
    		if( resourceMap.get(key)==null ) {
    			nodesToDelete.add(key);
    			log.infof("%s.removeExcessNodes: DELETING node %d:%d (not backed by project resource)", TAG,key.getProjectId(),key.getResourceId());
    		}
    	}
    	for(ProjectResourceKey key:nodesToDelete) {
    		modelManager.deleteResource(key.getProjectId(), key.getResourceId());
    	}
    	log.infof("%s.removeExcessNodes ==========================       Complete      ==================================", TAG);
    }
    
    /**
     * Iterate through the ProcessNodes known to the model. This includes families.
     * Delete any that do not have parents and are not root nodes.
     * Delete the accompanying resource. Hopefully this refers only
     * to a legacy issue and will never happen again.
     */
    public void removeOrphans() {
    	log.infof("%s.removeOrphans ======================== Removing Orphans ================================", TAG);
    	nodesToDelete.clear();
    	Map<ProjectResourceKey,ProcessNode> nodesByKey = modelManager.getNodesByKey();
    	Collection<ProcessNode> nodes = nodesByKey.values();
    	RootNode root = modelManager.getRootNode();
    	for( ProcessNode child:nodes) {
    		if( !child.getSelf().equals(root.getSelf()) && modelManager.getProcessNode(child.getParent())==null ) {
    			ProjectResourceKey key = new ProjectResourceKey(child.getProjectId(),child.getResourceId());
    			nodesToDelete.add(key);
    			log.infof("%s.removeOrphans: DELETING node %d:%d (has no parent)", TAG,key.getProjectId(),key.getResourceId());
    		}
    	}
    	// Actually remove the resource.
    	for(ProjectResourceKey key:nodesToDelete) {
    		modelManager.deleteResource(key.getProjectId(), key.getResourceId());
    		// Delete the current node and all its children.
    		GatewayContext context = modelManager.getContext();
    		Project project = context.getProjectManager().getProject(key.getProjectId(), ApplicationScope.GATEWAY, ProjectVersion.Staging);
    		if( project!=null ) {
    			project.deleteResource(key.getResourceId(), true); // Mark as dirty
    			try {
    				context.getProjectManager().saveProject(project, null, null, "Removing orphan resource", false);
    			}
    			catch(Exception ex) {
    				log.warnf("%s.removeOrphans: Failed to save project when deleting node %d:%d (%s)", TAG,key.getProjectId(),key.getResourceId(),
    						   ex.getMessage());
    			}
    		}

    	}
    	log.infof("%s.removeOrphans ==========================       Complete      ==================================", TAG);
    }

    private Map<ProjectResourceKey,ProjectResource> createResourceMap(GatewayContext context) {
    	// We need to pass something serializable to the panel, thus the list of resources
    	// We also need to make sure this is up-to-date.
    	Map<ProjectResourceKey,ProjectResource> map = new HashMap<>();
    	List<Project> projects = context.getProjectManager().getProjectsFull(ProjectVersion.Staging);
    	for( Project project:projects ) {
    		if( !project.isEnabled() || project.getId()==-1 ) continue;
    		List<ProjectResource> reslist = project.getResources();
    		for( ProjectResource res:reslist ) {
    			if( res.getModuleId().equalsIgnoreCase(BLTProperties.MODULE_ID)) {
    				if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE) || 
    						res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ||
    						res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE) ||
    						res.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)   ) {
    					map.put(new ProjectResourceKey(project.getId(),res.getResourceId()),res);
    				}
    			}
    		}
    	}
    	return map;
    }
}
