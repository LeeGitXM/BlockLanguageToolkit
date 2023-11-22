/**
 *   (c) 2014-2022 ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.BlockTagSynchronizer;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectListener;
import com.inductiveautomation.ignition.common.project.RuntimeProject;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * The model manager keeps track of the gateway version of model resources. On startup
 * and whenever a resource change takes place, the manager analyzes the resources
 * and extracts diagram and block information. This information is relayed to the
 * block manager via a passed-in controller instance.
 * 
 * In addition, the model manager keeps a gateway representation of the NavTree.
 * This is used by Designer/Client scripts to find/access model components. 
 * 
 * NOTE: The project listener interface only triggers when the user selects
 *       "Save and Publish".  We provide separate entry points for application
 *       startup and for the user selecting "Save" from the Designer.
 *
 */
public class ModelManager implements ProjectListener  {
	private static final String CLSS = "ModelManager";
	private static final boolean DEBUG = true;
	private final GatewayContext context;
	private final LoggerEx log;
	private final RootNode root;
	private final Map<ResourcePath,ProcessNode> orphansByResourcePath;
	private final Map<ResourcePath,ProcessNode> nodesByResourcePath;
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	
	/**
	 * Initially we query the gateway context to discover what resources exists. After that
	 * we rely on notifications of project resource updates. After discovering block resources
	 * we deserialize and inform the BlockExecutionController.
	 * 
	 * @param ctx the gateway context. 
	 */
	public ModelManager(GatewayContext ctx) { 
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.root = RootNode.getInstance();
		
		orphansByResourcePath = new HashMap<>();
		nodesByResourcePath = new HashMap<>();
		if(DEBUG) log.infof("--- %s.constructor() ---", CLSS);
	}
	
	/**
	 * Add a process diagram that is not associated with a project resource to 
	 * our hierarchy. This diagram will not be saved nor will ever be displayed
	 * in the UI. At this stage, no subscriptions are activated.
	 * 
	 * Currently its only use is for testing.
	 * 
	 * @param diagram the diagram to be added
	 */
	public void addTemporaryDiagram(ProcessDiagram diagram) {
		nodesByResourcePath.put(diagram.getResourceId().getResourcePath(),diagram);
	}
	
	/**
	 * Analyze a project resource for its embedded object. If, appropriate, add
	 * to the engine. Handle both additions and updates. In the case of startup
	 * read the auxiliary data from the database, otherwise we write it.
	 * @param projectId the identity of a project
	 * @param res the model resource
	 * @param startup
	 */
	public void analyzeResource(ProjectResource res,boolean startup) {
		ProjectResourceId resourceId = res.getResourceId();
		
		// This may be redundant, but this is called from several places so best to leave it here to be safe 
		if( resourceId.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
			
			if( res.isFolder() ) {
				if(DEBUG) log.infof("%s.analyzeResource: adding a folder = %s %s", CLSS, res.getResourceName(), (startup?"(STARTUP)":""));
				addModifyFolderResource(res);
			}
			else {
				if(DEBUG) log.infof("%s.analyzeResource: adding a diagram = %s %s", CLSS, res.getResourceName(), (startup?"(STARTUP)":""));
				addModifyDiagramResource(res,startup);
			}
		}
	}
	/**
	 * @return a list of all known diagrams 
	 */
	public List<ProcessDiagram> getDiagrams() {
		if(DEBUG) log.infof("*** %s.getDiagrams() ***", CLSS);
		List<ProcessDiagram> diagrams = new ArrayList<>();
		for(ProcessNode node:nodesByResourcePath.values()) {
			if( node instanceof ProcessDiagram ) diagrams.add((ProcessDiagram)node);
		}
		return diagrams;
	}
	
	/**
	 * Get a block from an existing diagram. 
	 * @param projectId project identifier
	 * @param resourceId resource identifier
	 * @param blockId identifier of the block.
	 * @return the specified ProcessBlock. If not found, return null. 
	 */
	public ProcessBlock getBlock(ProjectResourceId resourceId,UUID blockId) {
		ProcessBlock block = null;
		ProcessDiagram dm = getDiagram(resourceId);
		if( dm!=null ) {
			block = dm.getProcessBlock(blockId);
		}
		return block;
	}
	/**
	 * Get a specified block by its Id within a diagram. 
	 * @param blockId block identifier
	 * @return the specified block. If not found, return null. 
	 */
	public ProcessBlock getBlock(ProcessDiagram diagram,UUID blockId) {
		ProcessBlock node = diagram.getProcessBlock(blockId);
		return node;
	}
	
	/**
	 * Get a connection from the existing diagrams. 
	 * @param projectId
	 * @param resourceId
	 * @param connectionId
	 * @return the specified Connection. If not found, return null. 
	 */
	public Connection getConnection(ProjectResourceId resourceId,String connectionId) {
		Connection cxn = null;
		ProcessDiagram dm = getDiagram(resourceId);
		if( dm!=null ) {
			cxn = dm.getConnection(connectionId);
		}
		return cxn;
	}
	public GatewayContext getContext() { return this.context; }
	/**
	 * Get a specified diagram given projectId and resourceId. 
	 * @param projectId
	 * @param resourceId

	 * @return the specified diagram. If not found, return null. 
	 */
	public ProcessDiagram getDiagram(ProjectResourceId resourceId) {
		ProcessDiagram diagram = null;
		if( resourceId!=null ) {
			ProcessNode node = nodesByResourcePath.get(resourceId.getResourcePath());
			if( node instanceof ProcessDiagram ) diagram = (ProcessDiagram)node;
		}
		return diagram;
	}
	
	public Map<ResourcePath,ProcessNode> getNodesByResourcePath() { return nodesByResourcePath; }
	
	/**
	 * Get a specified node by its Id. 
	 * @param nodeId identifier of interest

	 * @return the specified diagram. If not found, return null. 
	 */
	public ProcessNode getProcessNode(ProjectResourceId nodeId) {
		ProcessNode node = nodesByResourcePath.get(nodeId.getResourcePath());
		return node;
	}

	/**
	 * Get a list of diagram tree paths known to the specified project. 
	 * @param projectName 
	 * @return a list of diagram tree paths. If none found, return null. 
	 */
	public synchronized List<SerializableResourceDescriptor> getDiagramDescriptors(String projectName) {
		if(DEBUG) log.infof("In %s.getDiagramDescriptors() for project %s...", CLSS, projectName);
		List<SerializableResourceDescriptor> result = new ArrayList<>();
		// First obtain a list of diagrams by recursively descending the tree
		Optional<RuntimeProject> optional = context.getProjectManager().getProject(projectName);
		Project project = optional.get();
		if( project!=null) {
			List<ProcessNode> nodes = root.allNodesForProject(projectName);
			// For each diagram discovered, create a tree path.
			for(ProcessNode node:nodes) {
				if( node instanceof ProcessDiagram ) {
					SerializableResourceDescriptor descriptor = new SerializableResourceDescriptor();
					descriptor.setName(node.getName());
					descriptor.setProjectName(projectName);
					descriptor.setPath(node.getPath());
					descriptor.setIsFolder(false);
					result.add(descriptor);
				}
			}
			if(DEBUG) log.infof("%s.getDiagramDescriptors: found %d for project %s", CLSS,result.size(),projectName);
		}
		else {
			log.warnf("%s.getDiagramDescriptors: Project %s not found", CLSS,projectName);
		}
		return result;	
	}
	
	/**
	 * Get a list of all diagram resource paths
	 * @return a list of diagram resource paths. If none found, return empty list. 
	 */
	public synchronized List<SerializableResourceDescriptor> getDiagramDescriptors() {
		if(DEBUG) log.infof("In %s.getDiagramDescriptors()...", CLSS);
		List<SerializableResourceDescriptor> result = new ArrayList<>();
		int priorSize = 0;
		List<String> projectNames = context.getProjectManager().getProjectNames();
		for( String projectName: projectNames ) {
			List<ProcessNode> nodes = root.allNodesForProject(projectName);
			// For each diagram discovered, create a tree path.
			for(ProcessNode node:nodes) {
				if( node instanceof ProcessDiagram ) {
					SerializableResourceDescriptor descriptor = new SerializableResourceDescriptor();
					descriptor.setName(node.getName());
					descriptor.setProjectName(projectName);
					descriptor.setPath(node.getPath());
					descriptor.setIsFolder(false);
					result.add(descriptor);
				}
			}
			if(DEBUG) log.infof("%s.getDiagramDescriptors: found %d (%s)", CLSS, result.size(), projectName);
		}
		return result;	
	}
	
	public synchronized List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(ProjectResourceId diagramId,UUID blockId,boolean spanDiagrams) {
		List<SerializableBlockStateDescriptor> results = new ArrayList<>();
		List<ProcessBlock> blocks = new ArrayList<>();
		ProcessDiagram diagram = getDiagram(diagramId);
		if( diagram!=null ) {
			ProcessBlock start = getBlock(diagram,blockId);
			if( start!=null ) {
				traverseDownstream(diagram,start,blocks,spanDiagrams);
				if(spanDiagrams && start.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SINK)) {
					followDownstreamConnections(start,blocks);
				}

				int index = 0;
				for( ProcessBlock block:blocks ) {
					index++;
					if( index<2 ) continue;   // Skip the start block
					results.add(block.toDescriptor());
				}
			}
			else {
				log.warnf("%s.listBlocksDownstreamOf: block %s not found on diagram %s", CLSS,blockId.toString(),diagramId.toString());
			}
		}
		else {
			log.warnf("%s.listBlocksDownstreamOf: diagram %s not found", CLSS,diagramId.getResourcePath().getPath().toString());
		}
		return results;
	}
	
	private void traverseDownstream(ProcessDiagram diagram,ProcessBlock block,List<ProcessBlock> blocks,boolean spanDiagrams) {
		if( block!=null && !blocks.contains(block)) {
			blocks.add(block);
			for(ProcessBlock blk:diagram.getDownstreamBlocks(block)) {
				traverseDownstream(diagram,blk,blocks,spanDiagrams);
				// Do an exhaustive search for all sink blocks that have the same binding
				// as the specified block. We cover all diagrams in the system.
				if( spanDiagrams && blk.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SINK) ) {
					followDownstreamConnections(blk,blocks);
				}
			}
		}
	}

	
	private void followDownstreamConnections(ProcessBlock sink,List<ProcessBlock> blocks) {
		if( sink.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SINK) ) {
			BlockProperty prop = sink.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
			if( prop!=null ) {
				String tagPath = prop.getBinding();
				if( tagPath!=null && !tagPath.isEmpty()) {
					for( ProcessDiagram diag:getDiagrams()) {
						for(ProcessBlock source:diag.getProcessBlocks()) {
							if( source.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SOURCE) ) {
								BlockProperty bp = source.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
								if( bp!=null && tagPath.equals(bp.getBinding())  ) {
									traverseDownstream(diag,source,blocks,true);
								}
							}
						}
					}
				}
			}
		}
	}
	public synchronized List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(ProjectResourceId diagramId,UUID blockId,boolean spanDiagrams) {
		List<SerializableBlockStateDescriptor> results = new ArrayList<>();
		List<ProcessBlock> blocks = new ArrayList<>();
		ProcessDiagram diagram = getDiagram(diagramId);
		if( diagram!=null ) {
			ProcessBlock start = getBlock(diagram,blockId);
			if( start!=null ) {
				traverseUpstream(diagram,start,blocks,spanDiagrams);
				if(spanDiagrams && start.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SOURCE)) {
					followUpstreamConnections(start,blocks);
				}
				int index = 0;
				for( ProcessBlock block:blocks ) {
					index++;
					if( index<2 ) continue;  // skip start block
					results.add(block.toDescriptor());
				}
			}
			else {
				log.warnf("%s.listBlocksUpstreamOf: block %s not found on diagram %s", CLSS,blockId.toString(),diagramId.toString());
			}
		}
		else {
			log.warnf("%s.listBlocksUpstreamOf: diagram %s not found", CLSS,diagramId.getResourcePath().getPath().toString());
		}
		return results;
	}
	private void traverseUpstream(ProcessDiagram diagram,ProcessBlock block,List<ProcessBlock> blocks,boolean spanDiagrams) {
		if( block!=null && !blocks.contains(block)) {
			blocks.add(block);
			for(ProcessBlock blk:diagram.getUpstreamBlocks(block)) {
				traverseUpstream(diagram,blk,blocks,spanDiagrams);
				// Do an exhaustive search for all sink blocks that have the same binding
				// as the specified block. We cover all diagrams in the system.
				if( spanDiagrams && blk.getClassName()!=null && blk.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SOURCE) ) {
					followUpstreamConnections(blk,blocks);
				}
			}
		}
	}
	private void followUpstreamConnections(ProcessBlock source,List<ProcessBlock> blocks) {
		if( source.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SOURCE) ) {
			BlockProperty prop = source.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
			if( prop!=null ) {
				String tagPath = prop.getBinding();
				if( tagPath!=null && !tagPath.isEmpty()) {
					for( ProcessDiagram diag:getDiagrams()) {
						for(ProcessBlock sink:diag.getProcessBlocks()) {
							if( sink.getClassName().equalsIgnoreCase(BlockConstants.BLOCK_CLASS_SINK) ) {
								BlockProperty bp = sink.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
								if( bp!=null && tagPath.equals(bp.getBinding())  ) {
									traverseUpstream(diag,sink,blocks,true);
								}
							}
						}
					}
				}
			}
		}	
	}

	/**
	 * Remove a diagram that is not associated with a project resource,
	 * nor with the folder hierarchy.
	 * 
	 * @param Id the UUID of the diagram to be removed
	 */
	public void removeTemporaryDiagram(ProjectResourceId id) {
		ProcessDiagram diagram = (ProcessDiagram)nodesByResourcePath.get(id.getResourcePath());
		if( diagram!=null ) {
			nodesByResourcePath.remove(diagram.getResourceId().getResourcePath());
			// Remove any subscriptions
			for( ProcessBlock pb:diagram.getProcessBlocks()) {
				for(BlockProperty bp:pb.getProperties()) {
					controller.removeSubscription(pb,bp);
				}
			}
		}
	}
	
	/**
	 * Walk the node tree and create a list of all resources currently being manned by
	 * the model manager.
	 * 
	 * @return
	 */
	public List<SerializableResourceDescriptor> queryControllerResources() {
		List<SerializableResourceDescriptor> result = new ArrayList<SerializableResourceDescriptor>();
		List<String> projectNames = context.getProjectManager().getProjectNames();
		for( String projectName:projectNames ) {
			for(ProcessNode node: root.allNodesForProject(projectName)) {
				if(node==null) continue;
				SerializableResourceDescriptor sd = new SerializableResourceDescriptor();
				sd.setName(node.getName());
				sd.setProjectName(projectName);
				if( node instanceof ProcessDiagram )sd.setIsFolder(false);
				else sd.setIsFolder(true);
				result.add(sd);
			}
		}
		return result;
	}
	/**
	 * Traverse the application trees, removing all resources. 
	 * This call does NOT do a SAVE. Therefore all resources will be restored
	 * on a restart. We simply clear all the node lists.
	 */
	public void removeAllDiagrams() {
		Collection<ProcessNode>children = root.getChildren();
		for(ProcessNode child:children) {
			root.removeChild(child);
		}
		orphansByResourcePath.clear();
		nodesByResourcePath.clear();
		nodesByResourcePath.put(root.getResourceId().getResourcePath(), root);
		if(DEBUG) log.infof("%s.removeAllDiagrams ... complete",CLSS);
	}
	
	/**
	 * Start all blocks in diagrams known to this manager. Note that, even if a diagram is
	 * DISABLED, its blocks are started. It's just that their results are not propagated.
	 */
	public void startBlocks() {
		for( ProcessNode node:nodesByResourcePath.values() ) {
			if( node instanceof ProcessDiagram ) {
				ProcessDiagram diagram = (ProcessDiagram)node;
				// Start in two passes - input blocks come last
				for( ProcessBlock pb:diagram.getProcessBlocks()) {
					if( !pb.delayBlockStart() ) pb.start();
				}
				for( ProcessBlock pb:diagram.getProcessBlocks()) {
					if( pb.delayBlockStart() ) pb.start();
				}
			}
		}
	}

	/**
	 * Stop all blocks in diagrams known to this manager. Presumably the controller has 
	 * been stopped.
	 */
	public void stopBlocks() {
		for( ProcessNode node:nodesByResourcePath.values() ) {
			if( node instanceof ProcessDiagram) {
				ProcessDiagram diagram = (ProcessDiagram)node;
				for( ProcessBlock pb:diagram.getProcessBlocks()) {
					pb.stop();
				}
			}
		}
	}
	
	// ====================== Project Listener Interface ================================
	/**
     * We don't care if the new project is a staging or published version.
     * Analyze only the staging project resources and update the controller.
     */
	@Override
	public void projectAdded(String projectName) {
		Optional<RuntimeProject> optional = context.getProjectManager().getProject(projectName);
		Project project = optional.get();
		if( project!=null ) {
			for( ProjectResource res:project.getResources() ) {
				ProjectResourceId resourceId = res.getResourceId();
				if( resourceId.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
					if(DEBUG) log.infof("%s.projectAdded: resource %s.%s (%s)", CLSS, projectName, res.getResourceName(), res.getResourceType().getTypeId());
					analyzeResource(res,false);
				}
			}
		}
	}

	
	/**
	 * Assume that the project resources are already gone. This is a cleanup step.
	 * We have confirmed that push notifications are already closed to any open designer/client.
	 * Ignition does not automatically close these when user deletes project in gateway,
	 * but we have no way of relaying that fact.
	 */
	@Override
	public void projectDeleted(String projectName) {
		log.infof("%s.projectDeleted: (id=%s)",CLSS,projectName);
		List<ProcessNode> nodes = root.allNodesForProject(projectName);
		for(ProcessNode node:nodes) {
			deleteResource(node.getResourceId());
		}
		root.removeProject(projectName);
		ProcessNodeSynchronizer pns = new ProcessNodeSynchronizer();
		pns.removeExcessNodes();   // Remove diagrams that do not correspond to a resource
	}
	
	/**
	 * Handle project resource updates of type model. NOTE: The Ignition gateway interface does not
	 * allow the designer to be open if enabling/disabling project from Gateway page.
	 * @param diff represents differences to the updated project. That is any updated, dirty or deleted resources.
	 * @param vers a value of "Staging" means is a result of a "Save". A value of "Published" occurs when a 
	 *        project is published. For our purposes both actions are equivalent(??).
	 *
	 * @see com.inductiveautomation.ignition.gateway.project.ProjectListener#projectUpdated(com.inductiveautomation.ignition.common.project.Project, com.inductiveautomation.ignition.common.project.ProjectVersion)
	 */
	@Override
	public void projectUpdated(String projectName) {
		Optional<RuntimeProject> optional = context.getProjectManager().getProject(projectName);
		Project proj = optional.get();
		log.infof("%s.projectUpdated: %s",CLSS,proj.getName());

		List<ProjectResource> resources = proj.getResources();
		ProcessNodeSynchronizer pns = new ProcessNodeSynchronizer();
		if( proj.isEnabled() ) {
			new Thread(new Runnable() {
				@Override
				public void run() {
					for( ProjectResource res:resources ) {
						if( res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
							log.infof("%s.projectUpdated: update resource %s.%s %s (%s) %s", CLSS,projectName,res.getResourceName(),
									res.getResourcePath().getPath().toString(),
									res.getResourceType().toString(),(res.isLocked()?"locked":"unlocked"));
							analyzeResource(res,false);  // Not startup
						}
					}
					pns.removeExcessNodes();   // Remove diagrams that do not correspond to a resource (e.g. due to a rename)
				}
			}).start(); 
		}
		// Delete the running resources of projects that are disabled. There is nothing displayable in the Designer
		else {   
			log.infof("%s: Project %s DISABLED, removing gateway resources",CLSS,proj.getName());
			for( ProjectResource res: proj.getResources() ) {
				if( res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
					if(DEBUG) log.infof("%s.projectUpdated: disabling %s:%s %s",CLSS,res.getResourceName(),res.getResourceId().getResourcePath().getPath().toString());
					deleteResource(res.getResourceId());
				}
			}
			pns.removeExcessNodes();   // Remove diagrams that do not correspond to a resource
		}

		log.infof("%s.projectUpdated: %s complete!", CLSS, proj.getName());
	}
	
	// ===================================== Private Methods ==========================================

	/**
	 * Handle project resource updates of type model. NOTE: The Ignition gateway interface does not
	 * allow the designer to be open if enabling/disabling project from Gateway page.
	 * @param diff represents differences to the updated project. That is any updated, dirty or deleted resources.
	 * @param vers a value of "Staging" means is a result of a "Save". A value of "Published" occurs when a 
	 *        project is published. For our purposes both actions are equivalent(??).
	 *
	 * @see com.inductiveautomation.ignition.gateway.project.ProjectListener#projectUpdated(com.inductiveautomation.ignition.common.project.Project, com.inductiveautomation.ignition.common.project.ProjectVersion)
	 */

	public void saveResource(ProjectResourceId resId, String projectName) {
		log.infof("=============================================");
		log.infof("%s.saveResource from project: %s", CLSS, projectName);
		
		Optional<RuntimeProject> optional = context.getProjectManager().getProject(projectName);
		Project proj = optional.get();
		
		List<ProjectResource> resources = proj.getResources();

		// Seems like there should be an easier way to get the resource if we have the id, but I struggled to find one
		log.tracef("Searching for the resource...");
		for( ProjectResource res:resources ) {
			if (res.getResourceId().equals(resId)) {
				log.infof("**** Found it! ******");
				//analyzeResource(res, false);  // Not startup
			}
		}

		log.infof("%s.saveResource() complete!", CLSS);
		log.infof("====================================================");
	}	
	
	
	/**
	 * Add or update a diagram in the model from a ProjectResource. The state of the 
	 * diagram is as it was serialized. There is a one-one correspondence 
	 * between a model-project and diagram. If the startup flag is set, the resource 
	 * is modified to include aux data from the extension script.
	 * @param projectId the identity of a project
	 * @param res the project resource containing the diagram
	 * @param startup
	 */
	private void addModifyDiagramResource(ProjectResource res,boolean startup) {
		ProjectResourceId resId = res.getResourceId();
		BlockTagSynchronizer bts = new BlockTagSynchronizer(res.getResourceId().getProjectName());
		if(DEBUG) log.infof("%s.addModifyDiagramResource: adding diagram:%s(%s)",CLSS,res.getResourceName(),resId.getResourcePath().getPath().toString());
		SerializableDiagram sd = deserializeDiagramResource(res);

		if( sd!=null ) {
			ProcessDiagram diagram = (ProcessDiagram)nodesByResourcePath.get(resId.getResourcePath());
			if( diagram==null) {   // this is usually run during gateway start up.
				// Create a new diagram
				if(DEBUG) log.infof("%s.addModifyDiagramResource: Creating diagram %s(%s) %s", CLSS,res.getResourceName(),
						sd.getPath().toString(),sd.getState().name());
				diagram = new ProcessDiagram(res.getProjectName(),sd,res.getResourcePath().getParent());

				// Add the new diagram to our hierarchy
				nodesByResourcePath.put(diagram.getResourceId().getResourcePath(),diagram);
				addToHierarchy(diagram);
				diagram.createBlocks(sd.getBlocks());
				diagram.updateConnections(sd.getConnections());
				if(!diagram.getState().equals(sd.getState()) ) {
					diagram.setState(sd.getState()); 
				}
				else {
					diagram.synchronizeSubscriptions();
				}
				bts.synchBlocks(diagram);
			}
			// Carefully update the diagram with new features/properties.
			// Leave existing blocks/subscriptions "as-is". 
			else {
				if(DEBUG) log.infof("%s.addModifyDiagramResource: Updating diagram %s", CLSS,res.getResourceName());
				diagram.setName(sd.getName());
				// Delete all the old connections
				diagram.clearConnections();
				// Delete blocks in the old that are not present in the new.
				// Stop subscriptions associated with those blocks.
				// Execute "delete" extension function for removed blocks
				List<ProcessBlock> deletedBlocks = diagram.removeUnusedBlocks(sd.getBlocks());
				for(ProcessBlock deletedBlock:deletedBlocks) {
					deletedBlock.onDelete();
					bts.synchDeletedBlock(deletedBlock);
				}
				diagram.createBlocks(sd.getBlocks());            // Adds blocks that are new in update
				diagram.updateConnections(sd.getConnections());  // Adds connections that are new in update
				diagram.updateProperties(sd);                    // Fixes subscriptions, as necessary
				diagram.setState(sd.getState(),true);            // Handle state change, if no change update tag subscriptions anyway.
				bts.synchBlocks(diagram);
			}

			if( diagram!=null )  {
				for(ProcessBlock block:diagram.getProcessBlocks()) {
					if( !startup ) {
						block.onSave();
					}
				}
			}
		}
		else {
			log.warnf("%s.addModifyDiagramResource - Failed to create diagram from resource (%s)",CLSS,res.getResourceName());
		}
	}	

	/**
	 * Add or update a folder resource from the ProjectResource.
	 * @param projectId the identity of a project
	 * @param resourceId the identity of the model resource
	 * @param model the diagram logic
	 */
	private void addModifyFolderResource(ProjectResource res) {
		ProjectResourceId resId = res.getResourceId();
		if(DEBUG) log.infof("%s.addModifyFolderResource:  %s(%s)",CLSS,res.getResourceName(),resId.getResourcePath().getPath());
		ProcessNode node = nodesByResourcePath.get(resId.getResourcePath());
		if( node==null ) {
			try {
				node = new ProcessNode(resId,res.getResourceName());
				// Add in the new Folder
				nodesByResourcePath.put(node.getResourceId().getResourcePath(),node);
				addToHierarchy(node);
			}
			catch( IllegalStateException ise ) {
				log.infof("%s.addModifyFolderResource: processed root node",CLSS);
			}
		}
		else {
			// The only attribute to update is the name
			node.setName(res.getResourceName());
		}
	}
	
	/**
	 * Add a process node to our hierarchy. 
	 * @param projectId the identity of a project
	 * @param node the node to be added
	 */
	private void addToHierarchy(ProcessNode node) {
		ProjectResourceId resourceId     = node.getResourceId();;
		nodesByResourcePath.put(resourceId.getResourcePath(), node);
		
		// If the parent is null, then we're the top of the chain for our project
		// Add the node to the root.
		ResourcePath parentPath = resourceId.getResourcePath().getParent();
		ResourcePath grandparentPath = resourceId.getResourcePath().getParent().getParent();
		
		if(DEBUG) log.infof("%s.addToHierarchy: %s (%s), Parent: %s - %s - %s", CLSS, node.getName(), node.getResourceId().getResourcePath().getPath(), parentPath.getName(), parentPath.getParentPath(), parentPath.getFolderPath());
		
		//
		//	9/20/23 - PAH - Something isn't correct here
		//  if( parentPath==null || parentPath.getParent().getFolderPath().equals("")   )  {
		//
		if( parentPath==null || parentPath.getFolderPath().equals("")   )  {
			root.addChild(node);
			if(DEBUG) log.infof("%s.addToHierarchy: %s is a ROOT (null parent)",CLSS,node.getName());
		}
		else if( parentPath.isModuleFolder() )  {
			root.addChild(node);
			if(DEBUG) log.infof("%s.addToHierarchy: %s is a FOLDER",CLSS,node.getName());
		}
		else {
			// If the parent is already in the tree, simply add the node as a child
			// Otherwise add to our list of orphans
			ProcessNode parent = nodesByResourcePath.get(node.getResourceId().getResourcePath());
			if(parent==null ) {
				if(DEBUG) log.infof("%s.addToHierarchy: %s is an ORPHAN (parent is %s)",CLSS,node.getName(),node.getResourceId().getFolderPath());
				orphansByResourcePath.put(resourceId.getResourcePath(), node);
			}
			else {
				if(DEBUG) log.infof("%s.addToHierarchy: %s is a CHILD of %s",CLSS,node.getName(),parent.getName());
				parent.addChild(node);
			}
		}	
		resolveOrphans();  // See if any orphans are children of new node.
	}
	
	/**
	 * Remove a resource node and all of its children within a project.
	 * Ignore resources that are not found. They easily could be children
	 * of a deleted parent.
	 * @param projectId the identity of a project.
	 * @param resourceId root of the resource tree to delete.
	 */
	public void deleteResource(ProjectResourceId resourceId) {
		if( resourceId.getResourcePath()!=null) {
			if(DEBUG) log.infof("%s.deleteResource: %s:%s",CLSS,resourceId.getProjectName(),resourceId.getResourcePath().getPath());

			ProcessNode head = nodesByResourcePath.get(resourceId.getResourcePath());
			if( head!=null ) {
				List<ProcessNode> nodesToDelete = new ArrayList<>();
				head.collectDescendants(nodesToDelete);  // "head" is in the list
				for(ProcessNode node:nodesToDelete ) {
					if( node instanceof ProcessDiagram ) {
						ProcessDiagram diagram = (ProcessDiagram)node;

						for(ProcessBlock block:diagram.getProcessBlocks()) {
							block.stop();
							for(BlockProperty prop:block.getProperties()) {
								controller.removeSubscription(block, prop);
							}
							block.onDelete();
							// If this is a source connection, delete its associated tag
							if(block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK)) {
								BlockProperty prop = block.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
								if(DEBUG) log.infof("%s.deleteResource:Deleting a sink (%s)",CLSS,resourceId.getResourcePath().getPath());
							}
						}
					}
					nodesByResourcePath.remove(resourceId.getResourcePath());

					if( node.getResourceId()!=null && node.getResourceId().getResourcePath()!=null ) {
						ProcessNode parent = nodesByResourcePath.get(node.getResourceId().getResourcePath());
						if( parent!=null ) {
							parent.removeChild(node);
						}
					}
					// Finally remove from the node maps
					nodesByResourcePath.remove(node.getResourceId().getResourcePath());
				}
			}
		}
	}
	
	/**
	 *  We've discovered a changed model resource. Deserialize and return.
	 *  Note: We had difficulty with the Ignition XML serializer because it didn't handle Java generics;
	 *        thus the use of JSON. The returned object was not an instanceof...
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	public SerializableDiagram deserializeDiagramResource(ProjectResource res) {
		SerializableDiagram sd = null;
		byte[] serializedObj = res.getData();
		if( serializedObj!=null && serializedObj.length>0 ) {
			try{
				String json = new String(serializedObj);
				//if(DEBUG) log.infof("%s.deserializeDiagramResource: json = %s",CLSS,json);
				ObjectMapper mapper = new ObjectMapper();
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
				mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
				sd = mapper.readValue(json, SerializableDiagram.class);
				if( sd!=null ) {
					/*
					if( DEBUG ) {
						log.infof("%s.deserializeDiagramResource: Successfully deserialized diagram %s",CLSS,sd.getName());
						for(SerializableBlock sb:sd.getBlocks()) {
							log.infof("%s: %s block, name = %s",CLSS,sb.getClassName(),sb.getName());
						}
					}
					*/
				}
				else {
					log.warnf("%s.deserializeDiagramResource: deserialization failed",CLSS);
				}
			}
			// Print stack trace
			catch( Exception ex) {
				log.warnf("%s.deserializeDiagramResource: exception (%s)",CLSS,ex.getLocalizedMessage(),ex);
			}
		}
		else {
			log.warnf("%s.deserializeDiagramResource: resource (%s) has no data",CLSS,res.getResourceName());
		}
		return sd;
	}
	
	/**
	 * Call this method after each node is defined. It has already been 
	 * added to the nodesByUUID and, if appropriate, the orphan list.
	 * Traverse the orphans to see if any parents have been defined.
	 * @param node
	 */
	private void resolveOrphans() {
		List<ProcessNode> reconciledOrphans = new ArrayList<ProcessNode>();
		for( ProcessNode orphan:orphansByResourcePath.values()) {
			ProcessNode parent = nodesByResourcePath.get(orphan.getResourceId().getResourcePath());
			// If is now resolved, remove node from orphan list and
			// add as child of parent. Recurse it's children.
			if(parent!=null ) {
				
				if( parent!=null ) {
					if(DEBUG) log.infof("%s.resolveOrphans: %s RECONCILED with parent (%s)",CLSS,orphan.getName(),parent.getName());
					reconciledOrphans.add(orphan);
				}
			}
		}
		for( ProcessNode orphan:reconciledOrphans) {
			ProcessNode parent = nodesByResourcePath.get(orphan.getResourceId().getResourcePath());
			parent.addChild(orphan);
			orphansByResourcePath.remove(orphan.getResourceId().getResourcePath());
		}
	}
}
