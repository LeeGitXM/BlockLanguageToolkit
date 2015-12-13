package com.ils.blt.gateway.wicket;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.ils.blt.gateway.engine.BlockExecutionController;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.engine.ProcessNode;
import com.ils.blt.gateway.engine.ProjectResourceKey;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * Reconcile any differences between the ModelManager and the ProjectManager.
 * Consider the project manager to be "correct". Edit the models to match.
 */
public class ProcessNodeSynchronizer {
    private static final long serialVersionUID = 1L;
    private final static String TAG = "ProcessNodeSynchronizer";
    private final LoggerEx log;
    private final ModelManager modelManager;
    private final List<ProjectResourceKey> nodesToDelete;
    private final Map<ProjectResourceKey,ProjectResource> resourceMap;
    /**
     * Constructor.
     */
    public ProcessNodeSynchronizer(Map<ProjectResourceKey,ProjectResource> map) {
    	this.log = LogUtil.getLogger(getClass().getPackage().getName());
    	this.modelManager = BlockExecutionController.getInstance().getDelegate();
    	this.nodesToDelete = new ArrayList<>();
    	this.resourceMap = map;
    }

    /**
     * Iterate through the resources. Create process nodes for any that
     * are not backed up in the model.
     */
    public void createMissingResources() {
    	log.infof("%s.createMissingResources ========================== Searching Resources ==================================", TAG);
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
    	log.infof("%s.removeExcessNodes ======================== Searching Process Nodes ================================", TAG);
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
   
    private void checkChildren(ProcessNode node) {
    	for(ProcessNode child: node.getChildren()) {
    		child.getResourceId();
    	}
    }
}
