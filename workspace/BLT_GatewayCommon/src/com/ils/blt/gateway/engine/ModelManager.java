/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.CoreBlock;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.common.BasicDiagram;
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
public abstract class ModelManager implements ProjectListener  {
	
	private static String TAG = "ModelManager";
	protected final GatewayContext context;
	protected final LoggerEx log;
	/** Access nodes by either UUID or tree path */
	protected RootNode root;
	protected final Map<ProjResKey,ProcessNode> nodesByKey; 
	protected final Map<UUID,ProcessNode> orphansByUUID;
	protected final Map<UUID,ProcessNode> nodesByUUID;
	protected final BlockExecutionController controller = BlockExecutionController.getInstance();
	
	/**
	 * Initially we query the gateway context to discover what resources exists. After that
	 * we rely on notifications of project resource updates. After discovering block resources
	 * we deserialize and inform the BlockExecutionController.
	 * 
	 * @param cntx the gateway context. 
	 */
	public ModelManager(GatewayContext ctx) { 
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		
		nodesByKey = new HashMap<ProjResKey,ProcessNode>();
		orphansByUUID = new HashMap<UUID,ProcessNode>();
		nodesByUUID = new HashMap<UUID,ProcessNode>();
		root = new RootNode(context);
		nodesByUUID.put(root.getSelf(), root);
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
	public void addTemporaryDiagram(BasicDiagram diagram) {
		nodesByUUID.put(diagram.getSelf(),diagram);
	}
	
	/**
	 * Analyze a project resource for its embedded object. If, appropriate, add
	 * to the engine. Handle both additions and updates.
	 * @param projectId the identity of a project
	 * @param res the model resource
	 */
	public abstract void analyzeResource(Long projectId,ProjectResource res) ;
	
	/**
	 * Get a block from an existing diagram. 
	 * @param projectId
	 * @param resourceId
	 * @param blockId identifier of the block.
	 * @return the specified CoreBlock. If not found, return null. 
	 */
	public CoreBlock getBlock(long projectId,long resourceId,UUID blockId) {
		CoreBlock block = null;
		BasicDiagram dm = getDiagram(projectId,resourceId);
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
	public CoreBlock getBlock(BasicDiagram diagram,UUID blockId) {
		CoreBlock node = diagram.getBlock(blockId);
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
		BasicDiagram dm = getDiagram(projectId,resourceId);
		if( dm!=null ) {
			cxn = dm.getConnection(connectionId);
		}
		return cxn;
	}
	
	/**
	 * Get a specified diagram by its Id. 
	 * @param diagramId

	 * @return the specified diagram. If not found, return null. 
	 */
	public BasicDiagram getDiagram(UUID diagramId) {
		BasicDiagram diagram = null;
		ProcessNode node = nodesByUUID.get(diagramId);
		if( node instanceof BasicDiagram ) diagram = (BasicDiagram)node;
		return diagram;
	}
	
	/**
	 * Get a specified diagram by its Id. 
	 * @param id

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
	public BasicDiagram getDiagram(long projectId,long resourceId) {
		BasicDiagram diagram = null;
		ProjResKey key = new ProjResKey(projectId,resourceId);
		ProcessNode node = nodesByKey.get(key);
		if( node instanceof BasicDiagram ) diagram = (BasicDiagram)node;
		return diagram;
	}
	
	/**
	 * @return a list of all known diagrams 
	 */
	public List<BasicDiagram> getDiagrams() {
		List<BasicDiagram> diagrams = new ArrayList<>();
		for(ProcessNode node:nodesByKey.values()) {
			if( node instanceof BasicDiagram ) diagrams.add((BasicDiagram)node);
		}
		return diagrams;
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
				if( node instanceof BasicDiagram ) {
					SerializableResourceDescriptor descriptor = new SerializableResourceDescriptor();
					descriptor.setName(node.getName());
					descriptor.setId(node.getSelf().toString());
					descriptor.setProjectId(projectId);
					descriptor.setResourceId(node.getResourceId());
					descriptor.setPath(node.getTreePath(nodesByUUID));
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
				if( node instanceof BasicDiagram ) {
					SerializableResourceDescriptor descriptor = new SerializableResourceDescriptor();
					descriptor.setName(node.getName());
					descriptor.setId(node.getSelf().toString());
					descriptor.setProjectId(projectId);
					descriptor.setResourceId(node.getResourceId());
					descriptor.setPath(node.getTreePath(nodesByUUID));
					result.add(descriptor);
				}
			}
		}
		return result;	
	}
	/**
	 * @return the root node of the diagram tree
	 */
	public RootNode getRootNode() { return root; }
	
	public List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(UUID diagramId,UUID blockId) {
		List<CoreBlock> blocks = new ArrayList<>();
		BasicDiagram diagram = getDiagram(diagramId);
		CoreBlock start = getBlock(diagram,blockId);
		traverseDownstream(diagram,start,blocks);
		List<SerializableBlockStateDescriptor> results = new ArrayList<>();
		for( CoreBlock block:blocks ) {
			results.add(block.toDescriptor());
		}
		return results;
	}
	
	private void traverseDownstream(BasicDiagram diagram,CoreBlock block,List<CoreBlock> blocks) {
		if( block!=null && !blocks.contains(block)) {
			for(CoreBlock blk:diagram.getDownstreamBlocks(block)) {
				blocks.add(block);
				traverseDownstream(diagram,blk,blocks);
			}
		}
	}
	public List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(UUID diagramId,UUID blockId) {
		List<CoreBlock> blocks = new ArrayList<>();
		BasicDiagram diagram = getDiagram(diagramId);
		CoreBlock start = getBlock(diagram,blockId);
		traverseUpstream(diagram,start,blocks);
		List<SerializableBlockStateDescriptor> results = new ArrayList<>();
		for( CoreBlock block:blocks ) {
			results.add(block.toDescriptor());
		}
		return results;
	}
	private void traverseUpstream(BasicDiagram diagram,CoreBlock block,List<CoreBlock> blocks) {
		if( block!=null && !blocks.contains(block)) {
			for(CoreBlock blk:diagram.getUpstreamBlocks(block)) {
				blocks.add(block);
				traverseUpstream(diagram,blk,blocks);
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
		BasicDiagram diagram = (BasicDiagram)nodesByUUID.get(Id);
		if( diagram!=null ) {
			nodesByUUID.remove(diagram.getSelf());
			// Remove any subscriptions
			for( CoreBlock pb:diagram.getDiagramBlocks()) {
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
				else if( node instanceof BasicDiagram )sd.setType(BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE);
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
			if( node instanceof BasicDiagram ) {
				BasicDiagram diagram = (BasicDiagram)node;
				diagram.startBlocks();
			}
		}
	}
	/**
	 * Stop all blocks in diagrams known to this manager. Presumably the controller has 
	 * been stopped.
	 */
	public void stopBlocks() {
		for( ProcessNode node:nodesByKey.values() ) {
			if( node instanceof BasicDiagram ) {
				BasicDiagram diagram = (BasicDiagram)node;
				for( CoreBlock pb:diagram.getDiagramBlocks()) {
					pb.stop();
				}
			}
		}
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
			log.infof("%s.projectAdded: %s (%d),staging",TAG,staging.getName(),projectId);
			List<ProjectResource> resources = published.getResources();
			for( ProjectResource res:resources ) {
				log.infof("%s.projectAdded: resource %s (%d),type %s", TAG,res.getName(),
						res.getResourceId(),res.getResourceType());
				analyzeResource(projectId,res);
			}
		}
	}
	/**
	 * Assume that the project resources are already gone. This is a cleanup step.
	 */
	@Override
	public void projectDeleted(long projectId) {
		log.infof("%s.projectDeleted: (id=%d)",TAG,projectId);
		deleteProjectResources(new Long(projectId));
		
	}
	/**
	 * Handle project resource updates of type model.
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
		log.infof("%s.projectUpdated: %s (%d)  %s", TAG,diff.getName(),diff.getId(),vers.toString());
		long projectId = diff.getId();
		Set<Long> deleted = diff.getDeletedResources();
		for (Long  resid : deleted) {
			log.infof("%s.projectUpdated: delete resource %d:%d", TAG,projectId,resid);
			deleteResource(new Long(projectId),resid);
		}
		
		List<ProjectResource> resources = diff.getResources();
		for( ProjectResource res:resources ) {
			//if( res.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) continue;
			log.infof("%s.projectUpdated: add/update resource %s (%d),type %s (%s)", TAG,res.getName(),
					res.getResourceId(),res.getResourceType(),(diff.isResourceDirty(res)?"dirty":"clean"));
			analyzeResource(new Long(projectId),res);
		}
	}

	/**
	 * Remove a resource node within a project.
	 * Presumably the diagram has been deleted.
	 * @param projectId the identity of a project.
	 */
	private void deleteResource(Long projectId,Long resourceId) {
		log.debugf("%s.deleteResource: %d:%d",TAG,projectId,resourceId);
		ProjResKey key = new ProjResKey(projectId.longValue(),resourceId.longValue());
		ProcessNode node = nodesByKey.get(key);
		if( node!=null ) {
			if( node instanceof BasicDiagram ) {
				BasicDiagram diagram = (BasicDiagram)node;

				for(CoreBlock block:diagram.getDiagramBlocks()) {
					block.stop();
					for(BlockProperty prop:block.getProperties()) {
						controller.removeSubscription(block, prop);
					}
				}
			}

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
			nodesByKey.remove(key);
			nodesByUUID.remove(node.getSelf());
		}
	}
	// Delete all process nodes for a given project.
	private void deleteProjectResources(Long projectId) {
		log.infof("%s.deleteProjectResources: proj = %d",TAG,projectId);
		List<ProcessNode> nodes = root.allNodesForProject(projectId);
		for(ProcessNode node:nodes) {
			if( node instanceof BasicDiagram ) {
				BasicDiagram diagram = (BasicDiagram)node;
				for(CoreBlock block:diagram.getDiagramBlocks()) {
					block.stop();
					for(BlockProperty prop:block.getProperties()) {
						controller.removeSubscription(block, prop);
					}
				}
				
			}
			ProjResKey key = new ProjResKey(projectId.longValue(),node.getResourceId());
			nodesByKey.remove(key);
			nodesByUUID.remove(node.getSelf());
		}
		root.removeProject(projectId);
	}
	

	/**
	 * Call this method after each node is defined. It has already been 
	 * added to the nodesByUUID and, if appropriate, the orphan list.
	 * Traverse the orphans to see if any parents have been defined.
	 * @param node
	 */
	protected void resolveOrphans() {
		List<ProcessNode> reconciledOrphans = new ArrayList<ProcessNode>();
		for( ProcessNode orphan:orphansByUUID.values()) {
			ProcessNode parent = nodesByUUID.get(orphan.getParent());
			// If is now resolved, remove node from orphan list and
			// add as child of parent. Recurse it's children.
			if(parent!=null ) {
				log.debugf("%s.resolveOrphans: %s RECONCILED with parent (%s)",TAG,orphan.getName(),parent.getName());
				reconciledOrphans.add(orphan);
			}
		}
		for( ProcessNode orphan:reconciledOrphans) {
			ProcessNode parent = nodesByUUID.get(orphan.getParent());
			parent.addChild(orphan);
			orphansByUUID.remove(orphan.getSelf());
		}
	}
	

	
	// ====================================== ProjectResourceKey =================================
	/**
	 * Class for keyed storage by projectId, resourceId
	 */
	protected class ProjResKey {
		private final long projectId;
		private final long resourceId;
		public ProjResKey(long projid,long resid) {
			this.projectId = projid;
			this.resourceId = resid;
		}
		public long getProjectId() { return projectId; }
		public long getResourceId() { return resourceId; }
		
		// So that class may be used as a map key
		// Same projectId and resourceId is sufficient to prove equality
		@Override
		public boolean equals(Object arg) {
			boolean result = false;
			if( arg instanceof ProjResKey) {
				ProjResKey that = (ProjResKey)arg;
				if( (this.getProjectId()==that.getProjectId()) &&
					(this.getResourceId()==that.getResourceId())   ) {
					result = true;
				}
			}
			return result;
		}
		@Override
		public int hashCode() {
			return (int)(this.projectId*100000+this.resourceId);
		}
	}
}
