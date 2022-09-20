package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.RuntimeProject;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * Reconcile any differences between the ModelManager and the ProjectManager.
 * Consider the project manager to be "correct". Edit the models to match.
 * This is used on the wicket status page and to synchronize project updates.
 */
public class ProcessNodeSynchronizer {
    private final static String CLSS = "ProcessNodeSynchronizer";
    private final LoggerEx log;
    private final ModelManager modelManager;
    private final List<ResourcePath> nodesToDelete;
    private final List<ResourcePath> resourceList;
    /**
     * Constructor.
     */
    public ProcessNodeSynchronizer() {
    	this.log = LogUtil.getLogger(getClass().getPackageName());
    	this.modelManager = BlockExecutionController.getInstance().getDelegate();
    	this.nodesToDelete = new ArrayList<>();
    	this.resourceList = createResourceList(modelManager.getContext());
    }
    
    /**
     * Iterate through the ProcessNodes known to the model.
     * Delete any that are not backed up by actual process resources. This can occur when diagrams are deleted or renamed.
     */
    public void removeExcessNodes() {
    	// log.infof("%s.removeExcessNodes ======================== Remove Excess Nodes ================================", CLSS);
    	nodesToDelete.clear();
    	Set<ResourcePath> nodes = modelManager.getNodesByResourcePath().keySet();
    	Iterator<ResourcePath> nodeWalker = nodes.iterator();
    	while(nodeWalker.hasNext()) {
    		ResourcePath path = nodeWalker.next();
    		if( !resourceList.contains(path) ) {
    			nodesToDelete.add(path);
    			log.infof("%s.removeExcessNodes: DELETING node %s (not backed by project resource)", CLSS,path);
    		}
    	}
    	for(ResourcePath path:nodesToDelete) {;
    		modelManager.getNodesByResourcePath().remove(path);
    	}
    	// log.infof("%s.removeExcessNodes ==========================       Complete      ==================================", CLSS);
    }

   

    // Create a list of all BLT project resources.
    private List<ResourcePath> createResourceList(GatewayContext context) {
    	// We need to pass something serializable to the panel, thus the list of resources
    	// We also need to make sure this is up-to-date.
    	List<ResourcePath> resources = new ArrayList<>();
    	List<String> projectNames = context.getProjectManager().getProjectNames();
    	for( String name:projectNames ) {
    		Optional<RuntimeProject> optional = context.getProjectManager().getProject(name);
    		Project project = optional.get();
    		if( !project.isEnabled() || project.getName().equals(Project.GLOBAL_PROJECT_NAME )) continue;
    		List<ProjectResource> reslist = project.getResources();
    		for( ProjectResource res:reslist ) {
    			if(res.getResourceId().getResourceType().getModuleId().equalsIgnoreCase(BLTProperties.MODULE_ID)) {
    				if( res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)  ) {
    					resources.add(res.getResourceId().getResourcePath());
    				}
    			}
    		}
    	}
    	return resources;
    }
}
