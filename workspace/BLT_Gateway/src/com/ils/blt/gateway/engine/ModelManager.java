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

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
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
import com.inductiveautomation.ignition.common.script.ScriptManager;
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
	private RootNode root;
	private final Map<ProjResKey,ProcessNode> nodesByKey; 
	private final Map<UUID,ProcessNode> orphansByUUID;
	private final Map<UUID,ProcessNode> nodesByUUID;
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	
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
				addModifyApplicationResource(projectId,res);
			}
			else if( type.equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
				addModifyFamilyResource(projectId,res);
			}
			else if( type.equalsIgnoreCase(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				addModifyDiagramResource(projectId,res);	
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
	public ProcessDiagram getDiagram(long projectId,long resourceId) {
		ProcessDiagram diagram = null;
		ProjResKey key = new ProjResKey(projectId,resourceId);
		ProcessNode node = nodesByKey.get(key);
		if( node instanceof ProcessDiagram ) diagram = (ProcessDiagram)node;
		return diagram;
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
			analyzeResource(projectId,res);
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
	private void addModifyApplicationResource(Long projectId,ProjectResource res) {
		log.infof("%s.addModifyApplicationResource: %s(%d)",TAG,res.getName(),res.getResourceId());
		ProcessApplication application = deserializeApplicationResource(projectId,res);
		if( application!=null ) {
			UUID self = application.getSelf();
			ProcessNode node = nodesByUUID.get(self);
			if( node==null ) {
				ProcessApplication processApp = new ProcessApplication(res.getName(),res.getParentUuid(),self);
				processApp.setResourceId(res.getResourceId());
				processApp.setProjectId(projectId);
				// Add in the new Application
				ProjResKey key = new ProjResKey(projectId,res.getResourceId());
				nodesByKey.put(key,processApp);
				addToHierarchy(projectId,processApp);
			}
			else {
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
	 */
	private void addModifyDiagramResource(Long projectId,ProjectResource res) {
		log.debugf("%s.addModifyDiagramResource: %s(%d)",TAG,res.getName(),res.getResourceId());
		SerializableDiagram sd = deserializeDiagramResource(projectId,res);
		if( sd!=null ) {
			// If this is an existing diagram, we need to remove the old version
			ProcessDiagram diagram = (ProcessDiagram)nodesByUUID.get(sd.getId());
			if( diagram==null) {
				diagram = new ProcessDiagram(sd,res.getParentUuid(),projectId);
				diagram.setResourceId(res.getResourceId());
				diagram.setProjectId(projectId);
				// Add in the new Diagram
				ProjResKey key = new ProjResKey(projectId,res.getResourceId());
				nodesByKey.put(key,diagram);
				addToHierarchy(projectId,diagram);
				diagram.analyze(sd);   // Determines connections
			}
			// Carefully update the diagram with new features/properties.
			// Leave existing blocks/subscriptions "as-is". 
			else {
				// Delete all the old connections
				diagram.clearConnections();
				// Delete blocks in the old that are not present in the new.
				// Stop subscriptions associated with those blocks.
				diagram.removeBlocksFromList(sd.getBlocks());
				// Add/update blocks, create new connections. Stop blocks, remove old subscriptions
				diagram.analyze(sd);
			}
			if( !diagram.getState().equals(DiagramState.DISABLED) ) {
				diagram.updateBlockTimers();  // Make sure timers are correct for current diagram state.
				log.tracef("%s.addModifyDiagramResource: starting tag subscriptions ...%d:%s",TAG,projectId,res.getName());
				for( ProcessBlock pb:diagram.getProcessBlocks()) {
					for(BlockProperty bp:pb.getProperties()) {
						controller.startSubscription(pb,bp);   // Does nothing for existing subscriptions
					}
					pb.setProjectId(projectId);
				}
				if( BlockExecutionController.getExecutionState().equals(BlockExecutionController.CONTROLLER_RUNNING_STATE)) {
					log.tracef("%s.addModifyDiagramResource: starting blocks ...%d:%s",TAG,projectId,res.getName());
					for( ProcessBlock pb:diagram.getProcessBlocks()) {
						pb.start();
					}
				}
			}
			else {
				log.infof("%s.addModifyDiagramResource: diagram is DISABLED (did not start subscriptions)...%d:%s",TAG,projectId,res.getName());
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
	private void addModifyFamilyResource(Long projectId,ProjectResource res) {
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
				ProjResKey key = new ProjResKey(projectId,res.getResourceId());
				nodesByKey.put(key,family);
				addToHierarchy(projectId,family);
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
			ProjResKey key = new ProjResKey(projectId,res.getResourceId());
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
	 * Remove a resource node within a project.
	 * Presumably the diagram has been deleted.
	 * @param projectId the identity of a project.
	 */
	private void deleteResource(Long projectId,Long resourceId) {
		log.debugf("%s.deleteResource: %d:%d",TAG,projectId,resourceId);
		ProjResKey key = new ProjResKey(projectId.longValue(),resourceId.longValue());
		ProcessNode node = nodesByKey.get(key);
		if( node!=null ) {
			if( node instanceof ProcessDiagram ) {
				ProcessDiagram diagram = (ProcessDiagram)node;

				for(ProcessBlock block:diagram.getProcessBlocks()) {
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
			if( node instanceof ProcessDiagram ) {
				ProcessDiagram diagram = (ProcessDiagram)node;
				for(ProcessBlock block:diagram.getProcessBlocks()) {
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
	 * We've discovered a changed model resource. Deserialize and convert into a ProcessApplication.
	 * Note that the name is wholly contained in the resource, not its contents.
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
