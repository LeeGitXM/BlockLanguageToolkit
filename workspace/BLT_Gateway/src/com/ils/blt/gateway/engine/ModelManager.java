/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
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
	
	private static String TAG = "ModelManager";
	private final GatewayContext context;
	private final LoggerEx log;
	/** Access nodes by either UUID or tree path */
	private final RootNode root;
	private final Map<ProjResKey,ProcessDiagram> diagramsByKey;  // Diagrams only
	private final Map<UUID,ProcessNode> orphansByUUID;
	private final Map<UUID,ProcessNode> nodesByUUID;
	
	
	/**
	 * Initially we query the gateway context to discover what resources exists. After that
	 * we rely on notifications of project resource updates. After discovering block resources
	 * we deserialize and inform the BlockExecutionController.
	 * 
	 * @param cntx the gateway context. 
	 */
	public ModelManager(GatewayContext ctx) { 
		this.context = ctx;
		log = LogUtil.getLogger(getClass().getPackage().getName());
		
		diagramsByKey = new HashMap<ProjResKey,ProcessDiagram>();
		orphansByUUID = new HashMap<UUID,ProcessNode>();
		nodesByUUID = new HashMap<UUID,ProcessNode>();
		root = new RootNode(context);
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
	public void analyzeResource(Long projectId,ProjectResource res) {
		if( res.getModuleId().equalsIgnoreCase(BLTProperties.MODULE_ID)) {
			String type = res.getResourceType();
			
			if( type.equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
				addApplicationResource(projectId,res);
			}
			else if( type.equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
				addFamilyResource(projectId,res);
			}
			else if( type.equalsIgnoreCase(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				addDiagramResource(projectId,res);	
			}
			else if( type.equalsIgnoreCase(BLTProperties.FOLDER_RESOURCE_TYPE) ) {
				addFolderResource(projectId,res);
			}
			else {
				// Don't care
				log.tracef("%s.analyze: Ignoring %s resource",TAG,type);
			}
		}
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
	
	/**
	 * Get a specified diagram given projectId and resourceId. 
	 * @param projectId
	 * @param resourceId

	 * @return the specified diagram. If not found, return null. 
	 */
	public ProcessDiagram getDiagram(long projectId,long resourceId) {
		ProjResKey key = new ProjResKey(projectId,resourceId);
		ProcessDiagram diagram = diagramsByKey.get(key);
		return diagram;
	}
	
	/**
	 * Get a specified diagram by project name and tree path. Useful for a query from the UI. 
	 * @param projectName
	 * @param treePath path to the diagram from the navigation tree in the Designer
	 * @return the specified diagram. If not found, return null. 
	 */
	public ProcessDiagram getDiagram(String projectName,String treePath) {
		ProcessDiagram diagram = null;
		ProcessNode node = root.findNode(projectName,treePath);
		if( node!=null && node instanceof ProcessDiagram) diagram = (ProcessDiagram)node;
		return diagram;
	}
	
	/**
	 * Get a list of diagram tree paths known to the specified project. 
	 * @param projectName 
	 * @return a list of diagram tree paths. If none found, return null. 
	 */
	public List<String> getDiagramTreePaths(String projectName) {
		List<String> result = new ArrayList<String>();
		// First obtain a list of diagrams by recursively descending the tree
		Long projectId = context.getProjectManager().getProjectId(projectName);
		if( projectId!=null) {
			List<ProcessNode> nodes = root.allNodesForProject(projectId);
			// For each diagram discovered, create a tree path.
			for(ProcessNode node:nodes) {
				if( node instanceof ProcessDiagram ) {
					String path = node.getTreePath(nodesByUUID);
					result.add(path);
				}
			}
		}
		else {
			log.warnf("%s.getDiagramTreePaths: Project %s not found", TAG,projectName);
		}
		return result;	
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
			BlockExecutionController controller = BlockExecutionController.getInstance();
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
		diagramsByKey.clear();
		orphansByUUID.clear();
		nodesByUUID.clear();
	}
	/**
	 * Start all blocks in diagrams known to this manager. Note that, even if a diagram is
	 * DISABLED, its blocks are started. It's just that their results are not propagated.
	 */
	public void startBlocks() {
		for( ProcessDiagram diagram:diagramsByKey.values() ) {
			for( ProcessBlock pb:diagram.getProcessBlocks()) {
				pb.start();
			}
		}
	}
	/**
	 * Stop all blocks in diagrams known to this manager. Presumably the controller has 
	 * been stopped.
	 */
	public void stopBlocks() {
		for( ProcessDiagram diagram:diagramsByKey.values() ) {
			for( ProcessBlock pb:diagram.getProcessBlocks()) {
				pb.stop();
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
			deleteResource(projectId,resid);
		}
		
		List<ProjectResource> resources = diff.getResources();
		for( ProjectResource res:resources ) {
			//if( res.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) continue;
			log.infof("%s.projectUpdated: resource %s (%d),type %s (%s)", TAG,res.getName(),
					res.getResourceId(),res.getResourceType(),(diff.isResourceDirty(res)?"dirty":"clean"));
			analyzeResource(projectId,res);
		}
	}
	
	// ===================================== Private Methods ==========================================
	/**
	 * Add or update an application in the model from a ProjectResource.
	 * This is essentially just a tree node.
	 * @param projectId the identity of a project
	 * @param res the project resource containing the diagram
	 */
	private void addApplicationResource(Long projectId,ProjectResource res) {
		log.infof("%s.addApplicationResource: %d",TAG,res.getResourceId());
		ProcessApplication application = deserializeApplicationResource(projectId,res);
		if( application!=null ) {
			UUID self = application.getSelf();
			ProcessNode node = nodesByUUID.get(self);
			if( node==null ) {
				node = new ProcessApplication(res.getName(),res.getParentUuid(),self);
				node.setResourceId(res.getResourceId());
				addToHierarchy(projectId,node);
			}
			else {
				// The only attribute to update is the name
				node.setName(res.getName());
			}
		}
	}
	/**
	 * Add or update a diagram in the model from a ProjectResource.
	 * There is a one-one correspondence 
	 * between a model-project and diagram.
	 * @param projectId the identity of a project
	 * @param res the project resource containing the diagram
	 */
	private void addDiagramResource(Long projectId,ProjectResource res) {
		log.infof("%s.addDiagramResource: %d",TAG,res.getResourceId());
		ProcessDiagram diagram = deserializeDiagramResource(projectId,res);
		if( diagram!=null) {
			BlockExecutionController controller = BlockExecutionController.getInstance();
			// If this is an existing diagram, we need to remove the old version
			ProcessDiagram oldDiagram = (ProcessDiagram)nodesByUUID.get(diagram.getSelf());
			if( oldDiagram!=null ) {
				nodesByUUID.remove(diagram.getSelf());
				ProcessNode oldParent = nodesByUUID.get(oldDiagram.getParent());
				if( oldParent!=null) oldParent.removeChild(oldDiagram);
				// Remove old subscriptions
				for( ProcessBlock pb:diagram.getProcessBlocks()) {
					for(BlockProperty bp:pb.getProperties()) {
						controller.removeSubscription(pb,bp);
					}
				}
			}
			// Now add in the new Diagram
			ProjResKey key = new ProjResKey(projectId,res.getResourceId());
			diagramsByKey.put(key,diagram);
			addToHierarchy(projectId,diagram);
			log.infof("%s.addDiagramResource: defining tag subscriptions ...%d:%s",TAG,projectId,res.getName());
			for( ProcessBlock pb:diagram.getProcessBlocks()) {
				for(BlockProperty bp:pb.getProperties()) {
					controller.startSubscription(pb,bp);
				}
			}
			if( BlockExecutionController.getExecutionState().equals(BlockExecutionController.CONTROLLER_RUNNING_STATE)) {
				log.infof("%s.addDiagramResource: starting blocks ...%d:%s",TAG,projectId,res.getName());
				for( ProcessBlock pb:diagram.getProcessBlocks()) {
					pb.start();
				}
			}
		}
		else {
			log.warnf("%s.addDiagramResource - Failed to create diagram from resource (%s)",TAG,res.getName());
		}
	}
	/**
	 * Add or update an application in the model from a ProjectResource.
	 * This is essentially just a tree node.
	 * @param projectId the identity of a project
	 * @param res the project resource containing the diagram
	 */
	private void addFamilyResource(Long projectId,ProjectResource res) {
		log.infof("%s.addFamilyResource: %d",TAG,res.getResourceId());
		ProcessFamily family = deserializeFamilyResource(projectId,res);
		if( family!=null ) {
			UUID self = family.getSelf();
			ProcessNode node = nodesByUUID.get(self);
			if( node==null ) {
				node = new ProcessFamily(res.getName(),res.getParentUuid(),self);
				node.setResourceId(res.getResourceId());
				addToHierarchy(projectId,node);
			}
			else {
				// The only attribute to update is the name
				node.setName(res.getName());
			}
		}
	}
	/**
	 * Add or update a folder resource from the ProjectResource.
	 * @param projectId the identity of a project
	 * @param resourceId the identity of the model resource
	 * @param model the diagram logic
	 */
	private void addFolderResource(long projectId,ProjectResource res) {
		log.infof("%s.addFolderResource: %d",TAG,res.getResourceId());
		UUID self = res.getDataAsUUID();
		ProcessNode node = nodesByUUID.get(self);
		if( node==null ) {
			node = new ProcessNode(res.getName(),res.getParentUuid(),self);
			node.setResourceId(res.getResourceId());
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
		log.infof("%s.addToHierarchy: %s",TAG,node.getName());
		UUID self     = node.getSelf();
		nodesByUUID.put(self, node);
		
		// If the parent is null, then we're the top of the chain for our project
		// Add the node to the root.
		if( node.getParent()==null )  {
			root.addChild(node,projectId);
			log.infof("%s.addToHierarchy: %s is a ROOT",TAG,node.getName());
		}
		else if( node.getParent().equals(BLTProperties.ROOT_FOLDER_UUID) )  {
			root.addChild(node,projectId);
			log.infof("%s.addToHierarchy: %s is a ROOT",TAG,node.getName());
		}
		else {
			// If the parent is already in the tree, simply add the node as a child
			// Otherwise add to our list of orphans
			ProcessNode parent = nodesByUUID.get(node.getParent());
			if(parent==null ) {
				log.infof("%s.addToHierarchy: %s is an ORPHAN",TAG,node.getName());
				orphansByUUID.put(self, node);
			}
			else {
				log.infof("%s.addToHierarchy: %s is a CHILD of %s",TAG,node.getName(),parent.getName());
				parent.addChild(node);
			}
		}	
		resolveOrphans();  // See if any orphans are children of new node.
	}
	
	/**
	 * Remove a diagram within a project.
	 * Presumably the diagram has been deleted.
	 * @param projectId the identity of a project.
	 */
	private void deleteResource(Long projectId,Long resourceId) {
		log.infof("%s.deleteResource: %d:%d",TAG,projectId,resourceId);
		ProjResKey key = new ProjResKey(projectId.longValue(),resourceId.longValue());
		ProcessDiagram diagram = diagramsByKey.get(key);
		if( diagram==null ) return;    // Nothing to do
		diagramsByKey.remove(key);
		nodesByUUID.remove(diagram.getSelf());
		BlockExecutionController controller = BlockExecutionController.getInstance();
		for(ProcessBlock block:diagram.getProcessBlocks()) {
			block.stop();
			for(BlockProperty prop:block.getProperties()) {
				controller.removeSubscription(block, prop);
			}
		}
		
		if( diagram.getParent()!=null ) {
			ProcessNode parent = nodesByUUID.get(diagram.getParent());
			if( parent!=null ) parent.removeChild(diagram);
		}
		
	}
	// Delete all process nodes for a given project.
	private void deleteProjectResources(Long projectId) {
		log.infof("%s.deleteProjectResources: proj = %d",TAG,projectId);
		List<ProcessNode> nodes = root.allNodesForProject(projectId);
		BlockExecutionController controller = BlockExecutionController.getInstance();
		for(ProcessNode node:nodes) {
			if( node instanceof ProcessDiagram ) {
				ProcessDiagram diagram = (ProcessDiagram)node;
				for(ProcessBlock block:diagram.getProcessBlocks()) {
					block.stop();
					for(BlockProperty prop:block.getProperties()) {
						controller.removeSubscription(block, prop);
					}
				}
				ProjResKey key = new ProjResKey(projectId.longValue(),diagram.getResourceId());
				diagramsByKey.remove(key);
			}
			nodesByUUID.remove(node.getSelf());
		}
		root.removeProject(projectId);
	}
	
	/**
	 * We've discovered a changed model resource. Deserialize and convert into a ProcessFamily.
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	private ProcessApplication deserializeApplicationResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		String json = new String(serializedObj);
		log.infof("%s.deserializeApplicationResource: json = %s",TAG,json);
		ProcessApplication application = null;
		try{
			ObjectMapper mapper = new ObjectMapper();
			SerializableApplication sa = mapper.readValue(json, SerializableApplication.class);
			if( sa!=null ) {
				log.infof("%s.deserializeApplicationResource: successfully deserialized application %s",TAG,sa.getName());
				application = new ProcessApplication(sa,res.getParentUuid());
				application.setResourceId(res.getResourceId());
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
	 *  We've discovered a changed model resource. Deserialize and convert into a ProcessDiagram.
	 *  Note: We had difficulty with the Ignition XML serializer because it didn't handle Java generics;
	 *        thus the use of JSON. The returned object was not an instanceof...
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	private ProcessDiagram deserializeDiagramResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		String json = new String(serializedObj);
		log.infof("%s.deserializeDiagramResource: json = %s",TAG,json);
		ProcessDiagram diagram = null;
		try{
			ObjectMapper mapper = new ObjectMapper();
			SerializableDiagram sd = mapper.readValue(json, SerializableDiagram.class);
			if( sd!=null ) {
				sd.setResourceId(res.getResourceId());
				log.infof("%s.deserializeDiagramResource: successfully deserialized diagram %s",TAG,sd.getName());
				diagram = new ProcessDiagram(sd,res.getParentUuid());
				diagram.setResourceId(res.getResourceId());
			}
			else {
				log.warnf("%s.deserializeDiagramResource: deserialization failed",TAG);
			}
		}
		// Print stack trace
		catch( Exception ex) {
			log.warnf("%s.deserializeDiagramResource: exception (%s)",TAG,ex.getLocalizedMessage(),ex);
		}
		return diagram;

	}
	
	/**
	 * We've discovered a changed model resource. Deserialize and convert into a ProcessFamily.
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	private ProcessFamily deserializeFamilyResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		String json = new String(serializedObj);
		log.infof("%s.deserializeFamilyResource: json = %s",TAG,json);
		ProcessFamily family = null;
		try{
			ObjectMapper mapper = new ObjectMapper();
			SerializableFamily sf = mapper.readValue(json, SerializableFamily.class);
			if( sf!=null ) {
				log.infof("%s.deserializeModelResource: successfully deserialized family %s",TAG,sf.getName());
				family = new ProcessFamily(sf,res.getParentUuid());
				family.setResourceId(res.getResourceId());
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
				log.infof("%s.resolveOrphans: %s RECONCILED with parent (%s)",TAG,orphan.getName(),parent.getName());
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
	private class ProjResKey {
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
