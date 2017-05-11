/**
 *   (c) 2014-2016  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.GatewayScriptExtensionManager;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;
import com.inductiveautomation.ignition.gateway.project.ProjectListener;

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
	private static final String TAG = "ModelManager";
	private static final boolean DEBUG = false;
	private final GatewayContext context;
	private final LoggerEx log;
	/** Access nodes by either UUID or tree path */
	private RootNode root;
	private final Map<ProjectUUIDKey,UUID> uuidMigrationMap;
	private final Map<Long,UUID> uuidByProjectId;
	private final Map<ProjectResourceKey,ProcessNode> nodesByKey; 
	private final Map<UUID,ProcessNode> orphansByUUID;
	private final Map<UUID,ProcessNode> nodesByUUID;
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	private final GatewayScriptExtensionManager extensionManager = GatewayScriptExtensionManager.getInstance();
	
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
		
		nodesByKey = new HashMap<>();
		orphansByUUID = new HashMap<UUID,ProcessNode>();
		nodesByUUID = new HashMap<UUID,ProcessNode>();
		uuidByProjectId = new HashMap<>();
		uuidMigrationMap = new HashMap<>();
		root = new RootNode(context);
		nodesByUUID.put(root.getSelf(), root);
		
		List<Project> projects = context.getProjectManager().getProjectsFull(ProjectVersion.Staging);
		for(Project p:projects) {
			uuidByProjectId.put(new Long(p.getId()), p.getUuid());
		}
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
		nodesByUUID.put(diagram.getSelf(),diagram);
	}
	
	/**
	 * Analyze a project resource for its embedded object. If, appropriate, add
	 * to the engine. Handle both additions and updates.
	 * @param projectId the identity of a project
	 * @param res the model resource
	 */
	public void analyzeResource(long projectId,ProjectResource res) {
		 analyzeResource(projectId,res,false);
	 }
	/**
	 * Analyze a project resource for its embedded object. If, appropriate, add
	 * to the engine. Handle both additions and updates.
	 * @param projectId the identity of a project
	 * @param res the model resource
	 * @param disable if true then create the resource in a disabled state
	 */
	public void analyzeResource(long projectId,ProjectResource res,boolean disable) {
		if( res.getModuleId()!=null && res.getModuleId().equalsIgnoreCase(BLTProperties.MODULE_ID)) {
			String type = res.getResourceType();
			
			if( type.equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
				addModifyApplicationResource(projectId,res);
			}
			else if( type.equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
				addModifyFamilyResource(projectId,res);
			}
			else if( type.equalsIgnoreCase(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				addModifyDiagramResource(projectId,res,disable);
			}
			else if( type.equalsIgnoreCase(BLTProperties.FOLDER_RESOURCE_TYPE) ) {
				addModifyFolderResource(projectId,res);
			}
			else {
				// Don't care
				log.tracef("%s.analyze: Ignoring %s resource",TAG,type);
			}
		}
	}
	/**
	 * @return a list of all known diagrams 
	 */
	public List<ProcessDiagram> getDiagrams() {
		List<ProcessDiagram> diagrams = new ArrayList<>();
		for(ProcessNode node:nodesByKey.values()) {
			if( node instanceof ProcessDiagram ) diagrams.add((ProcessDiagram)node);
		}
		return diagrams;
	}
	
	/**
	 * Get a block from an existing diagram. 
	 * @param projectId
	 * @param resourceId
	 * @param blockId identifier of the block.
	 * @return the specified ProcessBlock. If not found, return null. 
	 */
	public ProcessBlock getBlock(long projectId,long resourceId,UUID blockId) {
		ProcessBlock block = null;
		ProcessDiagram dm = getDiagram(projectId,resourceId);
		if( dm!=null ) {
			block = dm.getBlock(blockId);
		}
		return block;
	}
	/**
	 * Get a specified block by its Id within a diagram. 
	 * @param blockId
	 * @return the specified block. If not found, return null. 
	 */
	public ProcessBlock getBlock(ProcessDiagram diagram,UUID blockId) {
		ProcessBlock node = diagram.getBlock(blockId);
		return node;
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
		ProcessDiagram dm = getDiagram(projectId,resourceId);
		if( dm!=null ) {
			cxn = dm.getConnection(connectionId);
		}
		return cxn;
	}
	public GatewayContext getContext() { return this.context; }
	
	/**
	 * Get a specified diagram by its Id. 
	 * @param diagramId

	 * @return the specified diagram. If not found, return null. 
	 */
	public ProcessDiagram getDiagram(UUID diagramId) {
		ProcessDiagram diagram = null;
		ProcessNode node = nodesByUUID.get(diagramId);
		if( node instanceof ProcessDiagram ) diagram = (ProcessDiagram)node;
		return diagram;
	}
	public Map<ProjectResourceKey,ProcessNode> getNodesByKey() { return nodesByKey; }
	
	/**
	 * Get a specified node by its Id. 
	 * @param nodeId identifier of interest

	 * @return the specified diagram. If not found, return null. 
	 */
	public ProcessNode getProcessNode(UUID nodeId) {
		ProcessNode node = nodesByUUID.get(nodeId);
		return node;
	}
	
	/**
	 * Get a specified diagram given projectId and resourceId. 
	 * @param projectId
	 * @param resourceId

	 * @return the specified diagram. If not found, return null. 
	 */
	public ProcessDiagram getDiagram(long projectId,long resourceId) {
		ProcessDiagram diagram = null;
		ProjectResourceKey key = new ProjectResourceKey(projectId,resourceId);
		ProcessNode node = nodesByKey.get(key);
		if( node instanceof ProcessDiagram ) diagram = (ProcessDiagram)node;
		return diagram;
	}
	
	/**
	 * @return the named application
	 */
	public ProcessApplication getApplication(String name) {
		ProcessApplication app = null;
		for(ProcessNode node:nodesByKey.values()) {
			if( node instanceof ProcessApplication && 
				node.getName().equals(name)) {
				app = (ProcessApplication)node;
				break;
			}
		}
		return app;
	}
	

	/**
	 * Get a list of diagram tree paths known to the specified project. 
	 * @param projectName 
	 * @return a list of diagram tree paths. If none found, return null. 
	 */
	public List<SerializableResourceDescriptor> getDiagramDescriptors(String projectName) {
		List<SerializableResourceDescriptor> result = new ArrayList<>();
		// First obtain a list of diagrams by recursively descending the tree
		Long projectId = context.getProjectManager().getProjectId(projectName);
		if( projectId!=null) {
			List<ProcessNode> nodes = root.allNodesForProject(projectId);
			// For each diagram discovered, create a tree path.
			for(ProcessNode node:nodes) {
				if( node instanceof ProcessDiagram ) {
					SerializableResourceDescriptor descriptor = new SerializableResourceDescriptor();
					descriptor.setName(node.getName());
					descriptor.setId(node.getSelf().toString());
					descriptor.setProjectId(projectId);
					descriptor.setResourceId(node.getResourceId());
					descriptor.setPath(node.getTreePath(nodesByUUID));
					descriptor.setType(BLTProperties.DIAGRAM_RESOURCE_TYPE);
					result.add(descriptor);
				}
			}
		}
		else {
			log.warnf("%s.getDiagramTreePaths: Project %s not found", TAG,projectName);
		}
		return result;	
	}
	
	/**
	 * Get a list of all diagram tree paths
	 * @return a list of diagram tree paths. If none found, return null. 
	 */
	public List<SerializableResourceDescriptor> getDiagramDescriptors() {
		List<SerializableResourceDescriptor> result = new ArrayList<>();
		for( Long projectId: root.allProjects() ) {
			List<ProcessNode> nodes = root.allNodesForProject(projectId);
			// For each diagram discovered, create a tree path.
			for(ProcessNode node:nodes) {
				if( node instanceof ProcessDiagram ) {
					SerializableResourceDescriptor descriptor = new SerializableResourceDescriptor();
					descriptor.setName(node.getName());
					descriptor.setId(node.getSelf().toString());
					descriptor.setProjectId(projectId);
					descriptor.setResourceId(node.getResourceId());
					descriptor.setPath(node.getTreePath(nodesByUUID));
					descriptor.setType(BLTProperties.DIAGRAM_RESOURCE_TYPE);
					result.add(descriptor);
				}
			}
		}
		return result;	
	}
	/**
	 * @return the named family 
	 */
	public ProcessFamily getFamily(String appName,String famName) {
		ProcessFamily fam = null;
		ProcessApplication app = getApplication(appName);
		if( app!=null ) {
			for(ProcessNode node:nodesByKey.values()) {
				if( node instanceof ProcessFamily &&
					node.getName().equals(famName) ) {
					UUID parentId = node.getParent();
					while(parentId!=null) {
						ProcessNode parent = nodesByUUID.get(parentId);
						if( parent.getClass().equals(app.getClass()) &&
							parent.getName().equals(node.getName())      ) {
							fam = (ProcessFamily)parent;
							break;
						}
					}
				}
			}
		}
		return fam;
	}
	/**
	 * @return the root node of the diagram tree
	 */
	public RootNode getRootNode() { return root; }
	
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(UUID diagramId,UUID blockId,boolean spanDiagrams) {
		List<SerializableBlockStateDescriptor> results = new ArrayList<>();
		List<ProcessBlock> blocks = new ArrayList<>();
		ProcessDiagram diagram = getDiagram(diagramId);
		if( diagram!=null ) {
			ProcessBlock start = getBlock(diagram,blockId);
			if( start!=null ) {
				traverseDownstream(diagram,start,blocks,spanDiagrams);
				if(spanDiagrams && start.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SINK)) {
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
				log.warnf("%s.listBlocksDownstreamOf: block %s not found on diagram %s", TAG,blockId.toString(),diagramId.toString());
			}
		}
		else {
			log.warnf("%s.listBlocksDownstreamOf: diagram %s not found", TAG,diagramId.toString());
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
				if( spanDiagrams && blk.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SINK) ) {
					followDownstreamConnections(blk,blocks);
				}
			}
		}
	}

	
	private void followDownstreamConnections(ProcessBlock sink,List<ProcessBlock> blocks) {
		if( sink.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SINK) ) {
			BlockProperty prop = sink.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
			if( prop!=null ) {
				String tagPath = prop.getBinding();
				if( tagPath!=null && !tagPath.isEmpty()) {
					for( ProcessDiagram diag:getDiagrams()) {
						for(ProcessBlock source:diag.getProcessBlocks()) {
							if( source.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SOURCE) ) {
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
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(UUID diagramId,UUID blockId,boolean spanDiagrams) {
		List<SerializableBlockStateDescriptor> results = new ArrayList<>();
		List<ProcessBlock> blocks = new ArrayList<>();
		ProcessDiagram diagram = getDiagram(diagramId);
		if( diagram!=null ) {
			ProcessBlock start = getBlock(diagram,blockId);
			if( start!=null ) {
				traverseUpstream(diagram,start,blocks,spanDiagrams);
				if(spanDiagrams && start.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SOURCE)) {
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
				log.warnf("%s.listBlocksUpstreamOf: block %s not found on diagram %s", TAG,blockId.toString(),diagramId.toString());
			}
		}
		else {
			log.warnf("%s.listBlocksUpstreamOf: diagram %s not found", TAG,diagramId.toString());
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
				if( spanDiagrams && blk.getClassName()!=null && blk.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SOURCE) ) {
					followUpstreamConnections(blk,blocks);
				}
			}
		}
	}
	private void followUpstreamConnections(ProcessBlock source,List<ProcessBlock> blocks) {
		if( source.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SOURCE) ) {
			BlockProperty prop = source.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
			if( prop!=null ) {
				String tagPath = prop.getBinding();
				if( tagPath!=null && !tagPath.isEmpty()) {
					for( ProcessDiagram diag:getDiagrams()) {
						for(ProcessBlock sink:diag.getProcessBlocks()) {
							if( sink.getClassName().equalsIgnoreCase(BLTProperties.CLASS_NAME_SINK) ) {
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
	// Node must be in the nav-tree. Include project name.
	public String pathForNode(UUID nodeId) {
		String path = "";
		ProcessNode node = nodesByUUID.get(nodeId);
		if( node!=null) {
			// treePath includes "root", replace this with project.
			path = node.getTreePath(nodesByUUID);
			path = path.substring(5); // Strip off :root
			String projectName = root.getProjectName();
			path = projectName+path;
		}
		return path.toString();
		
	}
	/**
	 * Remove a diagram that is not associated with a project resource,
	 * nor with the folder hierarchy.
	 * 
	 * @param Id the UUID of the diagram to be removed
	 */
	public void removeTemporaryDiagram(UUID Id) {
		ProcessDiagram diagram = (ProcessDiagram)nodesByUUID.get(Id);
		if( diagram!=null ) {
			nodesByUUID.remove(diagram.getSelf());
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
		for( Long projectId:root.allProjects() ) {
			for(ProcessNode node: root.allNodesForProject(projectId)) {
				SerializableResourceDescriptor sd = new SerializableResourceDescriptor();
				sd.setName(node.getName());
				sd.setProjectId(projectId.longValue());
				sd.setResourceId(node.getResourceId());
				if( node instanceof ProcessApplication ) sd.setType(BLTProperties.APPLICATION_RESOURCE_TYPE);
				else if( node instanceof ProcessFamily ) sd.setType(BLTProperties.FAMILY_RESOURCE_TYPE);
				else if( node instanceof ProcessDiagram )sd.setType(BLTProperties.DIAGRAM_RESOURCE_TYPE);
				else sd.setType(BLTProperties.FOLDER_RESOURCE_TYPE);
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
		nodesByKey.clear();
		orphansByUUID.clear();
		nodesByUUID.clear();
		root = new RootNode(context);
		log.infof("%s.removeAllDiagrams ... complete",TAG);
	}
	/**
	 * Start all blocks in diagrams known to this manager. Note that, even if a diagram is
	 * DISABLED, its blocks are started. It's just that their results are not propagated.
	 */
	public void startBlocks() {
		for( ProcessNode node:nodesByKey.values() ) {
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
		for( ProcessNode node:nodesByKey.values() ) {
			if( node instanceof ProcessDiagram ) {
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
	public void projectAdded(Project staging, Project published) {
		if( staging!=null ) {
			if( staging.isEnabled() && staging.getId()!=-1 ) {
				long projectId = staging.getId();
				uuidByProjectId.put(new Long(projectId), staging.getUuid());
				List<ProjectResource> resources = staging.getResources();
				for( ProjectResource res:resources ) {
					log.infof("%s.projectAdded: resource %d.%d %s (%s)", TAG,projectId,res.getResourceId(),res.getName(),
							res.getResourceType());
					analyzeResource(projectId,res);
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
	public void projectDeleted(long projectId) {
		log.infof("%s.projectDeleted: (id=%d)",TAG,projectId);
		if( projectId<0 ) return;
		deleteProjectResources(projectId);
		
	}
	/**
	 * Handle project resource updates of type model. NOTE: The Ignition gateway interface does not
	 * allow the designer to be open if enabling/disabling project from Gateway page.
	 * @param diff represents differences to the updated project. That is any updated, dirty or deleted resources.
	 * @param vers a value of "Staging" means is a result of a "Save". A value of "Published" occurs when a 
	 *        project is published. For our purposes both actions are equivalent(??).
	 */
	/* (non-Javadoc)
	 * @see com.inductiveautomation.ignition.gateway.project.ProjectListener#projectUpdated(com.inductiveautomation.ignition.common.project.Project, com.inductiveautomation.ignition.common.project.ProjectVersion)
	 */
	@Override
	public void projectUpdated(Project diff, ProjectVersion vers) { 
		if( vers!=ProjectVersion.Staging ) return;  // Consider only the "Staging" version
		log.infof("%s.projectUpdated: %s (%d)  %s",TAG,diff.getName(),diff.getId(),vers.toString());
		long projectId = diff.getId();
		if( projectId<0 ) return;                   // Ignore global project
		
		UUID olduuid = uuidByProjectId.get(new Long(projectId));
		if( olduuid==null ) {
			log.warnf("%s.projectUpdated: No existing project (%d) found",TAG,projectId);
		}
		else if( !olduuid.equals(diff.getUuid()) ) {
			log.warnf("%s.projectUpdated: Replacing project (%d)",TAG,projectId);
			deleteProjectResources(projectId);
		}
		
		if( diff.isEnabled() ) {
			int countOfInteresting = 0;
			List<ProjectResource> resources = diff.getResources();
			Long pid = new Long(projectId);
			for( ProjectResource res:resources ) {
				//if( res.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) continue;
				log.infof("%s.projectUpdated: add/update resource %d.%d %s (%s) %s", TAG,projectId,res.getResourceId(),res.getName(),
						res.getResourceType(),(diff.isResourceDirty(res)?"dirty":"clean"));
				analyzeResource(pid,res);
				if( isBLTResource(res.getResourceType()) || res.getResourceType().equalsIgnoreCase("Window") ) countOfInteresting++;
			}

			Set<Long> deleted = diff.getDeletedResources();
			for (Long  rid : deleted) {
				long resid = rid.longValue();
				log.infof("%s.projectUpdated: delete resource %d:%d", TAG,projectId,resid);
				countOfInteresting++;
				deleteResource(projectId,resid);
			}
			
			// If there haven't been any interesting resources, then we've probably
			// just changed the enabled status of the project. Synchronize resources.
			if( countOfInteresting==0) {
				Project project = context.getProjectManager().getProject(projectId, ApplicationScope.GATEWAY,ProjectVersion.Staging);
				log.info("============================== ENABLED =================================");
				for( ProjectResource res: project.getResources() ) {
					log.infof("%s.projectUpdated: re-instating %d:%d %s",TAG,projectId,res.getResourceId(),res.getName());
				}
				
				for( ProjectResource res: project.getResources() ) {
					analyzeResource(pid,res,true);
				}
			}
		}
		// Delete the BLT resources of projects that are disabled. There is nothing displayable in the Designer
		else {     
			deleteProjectResources(projectId);
			
			log.info("============================== DISABLED =================================");
			Project project = context.getProjectManager().getProject(projectId, ApplicationScope.GATEWAY,ProjectVersion.Staging);
			for( ProjectResource res: project.getResources() ) {
				log.infof("%s.projectUpdated: removing  %d:%d %s",TAG,projectId,res.getResourceId(),res.getName());
			}
		}
	}
	
	// ===================================== Private Methods ==========================================
	/**
	 * Add or update an application in the model from a ProjectResource.
	 * This is essentially just a tree node. Use presence in the node map
	 * to determine whether or not this is a new resource or an update.
	 * @param projectId the identity of a project
	 * @param res the project resource containing the diagram
	 */
	private void addModifyApplicationResource(long projectId,ProjectResource res) {
		log.debugf("%s.addModifyApplicationResource: %s(%d)",TAG,res.getName(),res.getResourceId());
		ProcessApplication application = deserializeApplicationResource(projectId,res);
		if( application!=null ) {
			UUID self = application.getSelf();
			ProcessNode node = nodesByUUID.get(self);
			if( node==null ) {
				ProcessApplication processApp = new ProcessApplication(res.getName(),res.getParentUuid(),self);
				processApp.setResourceId(res.getResourceId());
				processApp.setProjectId(projectId);
				// Add in the new Application
				ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
				nodesByKey.put(key,processApp);
				addToHierarchy(projectId,processApp);
			}
			else if( node.getProjectId() != projectId)  {
				// The same UUID, but a different project, is a different resource
				// Check the node and parent UUIDs:
				//     if they haven't already been migrated, do it here.
				ProjectUUIDKey pukey = new ProjectUUIDKey(projectId,self);
				UUID newId = uuidMigrationMap.get(pukey); 
				if( newId==null ) {
					newId = UUID.randomUUID();
					uuidMigrationMap.put(pukey,newId);
				}
				//Parent should always be root
				ProcessApplication processApp = new ProcessApplication(res.getName(),res.getParentUuid(),newId);
				processApp.setResourceId(res.getResourceId());
				processApp.setProjectId(projectId);
				// Add in the new Application
				ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
				nodesByKey.put(key,processApp);
				addToHierarchy(projectId,processApp);
			}
			else  {
				// Update attributes
				node.setName(res.getName());
				if(node instanceof ProcessApplication )  {
					ProcessApplication processApp = (ProcessApplication)node;
					processApp.setState(application.getState());
				}
			}
		}
		else {
			log.warnf("%s.addModifyApplicationResource: failed to deserialize %s(%d)",TAG,res.getName(),res.getResourceId());
		}
	}
	/**
	 * Add or update a diagram in the model from a ProjectResource.
	 * There is a one-one correspondence 
	 * between a model-project and diagram.
	 * @param projectId the identity of a project
	 * @param res the project resource containing the diagram
	 * @param disable if true, change the diagram state to disabled
	 */
	private void addModifyDiagramResource(long projectId,ProjectResource res,boolean disable) {
		log.debugf("%s.addModifyDiagramResource: %s(%d)",TAG,res.getName(),res.getResourceId());
		SerializableDiagram sd = deserializeDiagramResource(projectId,res);

		if( sd!=null ) {
			ProcessDiagram diagram = (ProcessDiagram)nodesByUUID.get(sd.getId());
			if( diagram==null) {
				// Create a new diagram
				if(DEBUG) log.infof("%s.addModifyDiagramResource: Creating diagram %s(%s)", TAG,res.getName(),sd.getId().toString());
				if( disable ) sd.setState(DiagramState.DISABLED);
				diagram = new ProcessDiagram(sd,res.getParentUuid(),projectId);
				diagram.setResourceId(res.getResourceId());
				diagram.setProjectId(projectId);

				// Add the new diagram to our hierarchy
				ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
				nodesByKey.put(key,diagram);
				addToHierarchy(projectId,diagram);
				diagram.createBlocks(sd.getBlocks());
				diagram.updateConnections(sd.getConnections());
				if(!diagram.getState().equals(sd.getState()) ) {
					diagram.setState(sd.getState()); 
				}
				else {
					diagram.validateSubscriptions();
				}
			}
			else if(diagram.getProjectId() != projectId) {
				// The same UUID, but a different project, is a different resource
				// Check the node and parent UUIDs:
				//     if they haven't already been migrated, do it here.
				if(DEBUG) log.infof("%s.addModifyDiagramResource: Replicating diagram %s in new project", TAG,res.getName());
				ProjectUUIDKey pukey = new ProjectUUIDKey(projectId,sd.getId());
				UUID newId = uuidMigrationMap.get(pukey); 
				if( newId==null ) {
					newId = UUID.randomUUID();
					uuidMigrationMap.put(pukey,newId);
				}
				pukey = new ProjectUUIDKey(projectId,res.getParentUuid());	 		
				UUID parent = uuidMigrationMap.get(pukey);
				if( parent==null ) {
					parent = UUID.randomUUID();
					uuidMigrationMap.put(pukey,parent);
				}
				sd.setId(newId);
				diagram = new ProcessDiagram(sd,parent,res.getResourceId());
				diagram.setResourceId(res.getResourceId());
				diagram.setProjectId(projectId);
				// Add the new diagram to our hierarchy
				ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
				nodesByKey.put(key,diagram);
				addToHierarchy(projectId,diagram);
				// New Diagrams are always disabled
				diagram.createBlocks(sd.getBlocks());
				diagram.updateConnections(sd.getConnections());
				diagram.setState(DiagramState.DISABLED);
				
			}
			// Carefully update the diagram with new features/properties.
			// Leave existing blocks/subscriptions "as-is". 
			else {
				if(DEBUG) log.infof("%s.addModifyDiagramResource: Updating diagram %s", TAG,res.getName());
				diagram.setName(sd.getName());
				// Delete all the old connections
				diagram.clearConnections();
				// Delete blocks in the old that are not present in the new.
				// Stop subscriptions associated with those blocks.
				diagram.removeUnusedBlocks(sd.getBlocks());
				diagram.createBlocks(sd.getBlocks());       // Adds blocks that are new in update
				diagram.updateConnections(sd.getConnections());  // Adds connections that are new in update
				diagram.updateProperties(sd);
				diagram.setState(sd.getState());// Handle state change, if any
			}
			//	Invoke extension script on diagram save
			if( diagram!=null && !diagram.getState().equals(DiagramState.DISABLED) ) {
				extensionManager.runScript(context.getProjectManager().getProjectScriptManager(diagram.getProjectId()), 
											ScriptConstants.DIAGRAM_CLASS_NAME, 
											ScriptConstants.NODE_SAVE_SCRIPT, diagram.getSelf().toString());
			}
		}
		else {
			log.warnf("%s.addModifyDiagramResource - Failed to create diagram from resource (%s)",TAG,res.getName());
		}
	}
	/**
	 * Add or update an application in the model from a ProjectResource.
	 * This is essentially just a tree node.
	 * @param projectId the identity of a project
	 * @param res the project resource containing the diagram
	 */
	private void addModifyFamilyResource(long projectId,ProjectResource res) {
		log.debugf("%s.addModifyFamilyResource: %s(%d)",TAG,res.getName(),res.getResourceId());
		ProcessFamily family = deserializeFamilyResource(projectId,res);
		if( family!=null ) {
			UUID self = family.getSelf();
			ProcessNode node = nodesByUUID.get(self);
			if( node==null ) {
				ProcessFamily processFam = new ProcessFamily(res.getName(),res.getParentUuid(),self);
				processFam.setResourceId(res.getResourceId());
				processFam.setProjectId(projectId);
				// Add in the new Family
				ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
				nodesByKey.put(key,processFam);
				addToHierarchy(projectId,processFam);
			}
			else if(node.getProjectId() != projectId) {
				// The same UUID, but a different project, is a different resource
				// Check the node and parent UUIDs:
				//     if they haven't already been migrated, do it here.
				ProjectUUIDKey pukey = new ProjectUUIDKey(projectId,self);
				UUID newId = uuidMigrationMap.get(pukey); 
				if( newId==null ) {
					newId = UUID.randomUUID();
					uuidMigrationMap.put(pukey,newId);
				}
				pukey = new ProjectUUIDKey(projectId,res.getParentUuid());	 		
				UUID parent = uuidMigrationMap.get(pukey);
				if( parent==null ) {
					parent = UUID.randomUUID();
					uuidMigrationMap.put(pukey,parent);
				}
				ProcessFamily processFam = new ProcessFamily(res.getName(),parent,newId);
				processFam.setResourceId(res.getResourceId());
				processFam.setProjectId(projectId);
				// Add in the new Family
				ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
				nodesByKey.put(key,processFam);
				addToHierarchy(projectId,processFam);
			}
			else {
				// Update attributes
				node.setName(res.getName());
				if( node instanceof ProcessFamily ) {
					ProcessFamily processFam = (ProcessFamily)node;
					processFam.setState(family.getState());
				}
			}
		}
		else {
			log.warnf("%s.addModifyFamilyResource: failed to deserialize %s(%d)",TAG,res.getName(),res.getResourceId());
		}
	}
	/**
	 * Add or update a folder resource from the ProjectResource.
	 * @param projectId the identity of a project
	 * @param resourceId the identity of the model resource
	 * @param model the diagram logic
	 */
	private void addModifyFolderResource(long projectId,ProjectResource res) {
		log.debugf("%s.addFolderResource: %s(%d)",TAG,res.getName(),res.getResourceId());
		UUID self = res.getDataAsUUID();
		ProcessNode node = nodesByUUID.get(self);
		if( node==null ) {
			node = new ProcessNode(res.getName(),res.getParentUuid(),self);
			node.setResourceId(res.getResourceId());
			node.setProjectId(projectId);
			// Add in the new Folder
			ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
			nodesByKey.put(key,node);
			addToHierarchy(projectId,node);
		}
		else if(node.getProjectId() != projectId) {
			// The same UUID, but a different project, is a different resource
			// Check the node and parent UUIDs:
			//     if they haven't already been migrated, do it here.
			ProjectUUIDKey pukey = new ProjectUUIDKey(projectId,self);
			UUID newId = uuidMigrationMap.get(pukey); 
			if( newId==null ) {
				newId = UUID.randomUUID();
				uuidMigrationMap.put(pukey,newId);
			}
			pukey = new ProjectUUIDKey(projectId,res.getParentUuid());	 		
			UUID parent = uuidMigrationMap.get(pukey);
			if( parent==null ) {
				parent = UUID.randomUUID();
				uuidMigrationMap.put(pukey,parent);
			}
			node = new ProcessNode(res.getName(),parent,newId);
			node.setResourceId(res.getResourceId());
			node.setProjectId(projectId);
			// Add in the new Folder
			ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
			nodesByKey.put(key,node);
			addToHierarchy(projectId,node);
		}
		else {
			// The only attribute to update is the name
			node.setName(res.getName());
		}
	}
	
	/**
	 * Add a process node to our hierarchy. 
	 * @param projectId the identity of a project
	 * @param node the node to be added
	 */
	private void addToHierarchy(long projectId,ProcessNode node) {
		log.tracef("%s.addToHierarchy: %s (%d:%s)",TAG,node.getName(),node.getResourceId(),node.getSelf().toString());
		UUID self     = node.getSelf();
		nodesByUUID.put(self, node);
		
		// If the parent is null, then we're the top of the chain for our project
		// Add the node to the root.
		if( node.getParent()==null )  {
			root.addChild(node,projectId);
			log.tracef("%s.addToHierarchy: %s is a ROOT (null parent)",TAG,node.getName());
		}
		else if( node.getParent().equals(BLTProperties.ROOT_FOLDER_UUID) )  {
			root.addChild(node,projectId);
			log.tracef("%s.addToHierarchy: %s is a ROOT (parent is root folder)",TAG,node.getName());
		}
		else {
			// If the parent is already in the tree, simply add the node as a child
			// Otherwise add to our list of orphans
			ProcessNode parent = nodesByUUID.get(node.getParent());
			if( parent!=null && parent.getProjectId()!=node.getProjectId()) {
				// We need to use the migrated parent
				ProjectUUIDKey pukey = new ProjectUUIDKey(node.getProjectId(),parent.getSelf());	 		
				if( uuidMigrationMap.get(pukey) != null ) {
					UUID parentuuid = uuidMigrationMap.get(pukey);
					parent = nodesByUUID.get(parentuuid);
				}
			}
	
			if(parent==null ) {
				log.tracef("%s.addToHierarchy: %s is an ORPHAN (parent is %s)",TAG,node.getName(),node.getParent().toString());
				orphansByUUID.put(self, node);
			}
			else {
				log.tracef("%s.addToHierarchy: %s is a CHILD of %s",TAG,node.getName(),parent.getName());
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
	public void deleteResource(long projectId,long resourceId) {
		log.debugf("%s.deleteResource: %d:%d",TAG,projectId,resourceId);
		ProjectResourceKey key = new ProjectResourceKey(projectId,resourceId);
		ProcessNode head = nodesByKey.get(key);
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
					}
				}
				ProjectResourceKey nodekey = new ProjectResourceKey(projectId,node.getResourceId());
				nodesByKey.remove(nodekey);
				
				if( node.getParent()!=null ) {
					ProcessNode parent = nodesByUUID.get(node.getParent());
					if( parent!=null ) {
						parent.removeChild(node);
						if( parent.getSelf().equals(root.getSelf())) {
							root.removeChildFromProjectRoot(projectId,node);
						}
					}
				}
				// Finally remove from the node maps
				nodesByUUID.remove(node.getSelf());
			}
		}
	}
	
	// Delete all process nodes for a given project.
	private void deleteProjectResources(long projectId) {
		log.debugf("%s.deleteProjectResources: proj = %d",TAG,projectId);
		List<ProcessNode> nodes = root.allNodesForProject(projectId);
		for(ProcessNode node:nodes) {
			deleteResource(projectId,node.getResourceId());
		}
		Long pid = new Long(projectId);
		root.removeProject(pid);
		uuidByProjectId.remove(pid);
	}
	
	/**
	 * We've discovered a changed model resource. Deserialize and convert into a ProcessApplication.
	 * Note that the name is wholly contained in the resource, not its contents. Clear out any nested
	 * resources as these are only used when serializing the application outside of a project.
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	private ProcessApplication deserializeApplicationResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		String json = new String(serializedObj);
		log.debugf("%s.deserializeApplicationResource: json = %s",TAG,json);
		ProcessApplication application = null;
		try{
			ObjectMapper mapper = new ObjectMapper();
			SerializableApplication sa = mapper.readValue(json, SerializableApplication.class);
			if( sa!=null ) {
				sa.setName(res.getName());
				log.debugf("%s.deserializeApplicationResource: Successfully deserialized application %s",TAG,sa.getName());
				application = new ProcessApplication(sa,res.getParentUuid());
				application.setResourceId(res.getResourceId());
				application.setProjectId(projId);
			}
			else {
				log.warnf("%s.deserializeApplicationResource: deserialization failed",TAG);
			}
		}
		// Print stack trace
		catch( Exception ex) {
			log.warnf("%s.deserializeApplicationResource: exception (%s)",TAG,ex.getLocalizedMessage(),ex);
		}
		return application;
	}
	/**
	 *  We've discovered a changed model resource. Deserialize and return.
	 *  Note: We had difficulty with the Ignition XML serializer because it didn't handle Java generics;
	 *        thus the use of JSON. The returned object was not an instanceof...
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	private SerializableDiagram deserializeDiagramResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		String json = new String(serializedObj);
		log.debugf("%s.deserializeDiagramResource: json = %s",TAG,json);
		SerializableDiagram sd = null;
		try{
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			sd = mapper.readValue(json, SerializableDiagram.class);
			if( sd!=null ) {
				sd.setName(res.getName());       // Name comes from the resource
				log.debugf("%s.deserializeDiagramResource: Successfully deserialized diagram %s",TAG,sd.getName());
				sd.setResourceId(res.getResourceId());
			}
			else {
				log.warnf("%s.deserializeDiagramResource: deserialization failed",TAG);
			}
		}
		// Print stack trace
		catch( Exception ex) {
			log.warnf("%s.deserializeDiagramResource: exception (%s)",TAG,ex.getLocalizedMessage(),ex);
		}
		return sd;

	}
	
	/**
	 * We've discovered a changed model resource. Deserialize and convert into a ProcessFamily.
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	private ProcessFamily deserializeFamilyResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		String json = new String(serializedObj);
		log.debugf("%s.deserializeFamilyResource: json = %s",TAG,json);
		ProcessFamily family = null;
		try{
			ObjectMapper mapper = new ObjectMapper();
			SerializableFamily sf = mapper.readValue(json, SerializableFamily.class);
			if( sf!=null ) {
				sf.setName(res.getName());     // Resource is the source of the name.
				log.debugf("%s.deserializeFamilyResource: Successfully deserialized family %s",TAG,sf.getName());
				family = new ProcessFamily(sf,res.getParentUuid());
				family.setResourceId(res.getResourceId());
				family.setProjectId(projId);
			}
			else {
				log.warnf("%s: deserializeFamilyResource: deserialization failed",TAG);
			}
		}
		// Print stack trace
		catch( Exception ex) {
			log.warnf("%s.deserializeFamilyResource: exception (%s)",TAG,ex.getLocalizedMessage(),ex);
		}
		return family;
	}

	private boolean isBLTResource(String type) {
		boolean isBLTType = false;
		if( type.equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE) ||
			type.equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE) ||
			type.equalsIgnoreCase(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
			 isBLTType = true;
		}
		return isBLTType;
	}
	
	/**
	 * Call this method after each node is defined. It has already been 
	 * added to the nodesByUUID and, if appropriate, the orphan list.
	 * Traverse the orphans to see if any parents have been defined.
	 * @param node
	 */
	private void resolveOrphans() {
		List<ProcessNode> reconciledOrphans = new ArrayList<ProcessNode>();
		for( ProcessNode orphan:orphansByUUID.values()) {
			ProcessNode parent = nodesByUUID.get(orphan.getParent());
			// If is now resolved, remove node from orphan list and
			// add as child of parent. Recurse it's children.
			if(parent!=null ) {
				if( parent.getProjectId()!=orphan.getProjectId()) {
					// We need to use the migrated parent
					ProjectUUIDKey pukey = new ProjectUUIDKey(orphan.getProjectId(),parent.getSelf());	 		
					if( uuidMigrationMap.get(pukey) != null ) {
						UUID parentuuid = uuidMigrationMap.get(pukey);
						orphan.setParent(parentuuid);
						parent = nodesByUUID.get(parentuuid);
					}
				}
				
				if( parent!=null ) {
					log.debugf("%s.resolveOrphans: %s RECONCILED with parent (%s)",TAG,orphan.getName(),parent.getName());
					reconciledOrphans.add(orphan);
				}
			}
		}
		for( ProcessNode orphan:reconciledOrphans) {
			ProcessNode parent = nodesByUUID.get(orphan.getParent());
			parent.addChild(orphan);
			orphansByUUID.remove(orphan.getSelf());
		}
	}
}
