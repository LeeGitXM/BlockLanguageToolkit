/**
 *   (c) 2014-2021 ILS Automation. All rights reserved. 
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
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.connection.Connection;
import com.ils.blt.common.script.Script;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.BlockTagSynchronizer;
import com.ils.common.persistence.ToolkitProperties;
import com.ils.common.persistence.ToolkitRecordHandler;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectListener;
import com.inductiveautomation.ignition.common.project.RuntimeProject;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;
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
	private RootNode root;
	private final Map<String,ProcessNode> nodesByResourceId; 
	private final Map<ResourcePath,ProcessNode> orphansByResourcePath;
	private final Map<ResourcePath,ProcessNode> nodesByResourcePath;
	private final BlockExecutionController controller = BlockExecutionController.getInstance();
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	private final ToolkitRecordHandler toolkitHandler;
	
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
		this.toolkitHandler = new ToolkitRecordHandler(context);
		
		nodesByResourceId = new HashMap<>();
		orphansByResourcePath = new HashMap<>();
		nodesByResourcePath = new HashMap<>();
		ResourceType rtype = BLTProperties.FOLDER_RESOURCE_TYPE;
		ProjectResourceId resourceId = new ProjectResourceId(BLTProperties.UNDEFINED,rtype,BLTProperties.ROOT_FOLDER_NAME);
		root = new RootNode(context,resourceId);
		nodesByResourcePath.put(root.getResourceId().getResourcePath(), root);
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
		if( resourceId.getResourceType().getModuleId()!=null && resourceId.getResourceType().getModuleId().equalsIgnoreCase(BLTProperties.MODULE_ID)) {
			ResourceType type = res.getResourceType();
			
			if( type.equals(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
				if(DEBUG) log.infof("%s.analyzeResource: adding an application = %s %s", CLSS, res.getResourceName(), (startup?"(STARTUP)":""));
				addModifyApplicationResource(res,startup);
			}
			else if( type.equals(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
				if(DEBUG) log.infof("%s.analyzeResource: adding a family = %s %s", CLSS, res.getResourceName(), (startup?"(STARTUP)":""));
				addModifyFamilyResource(res,startup);
			}
			else if( type.equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				if(DEBUG) log.infof("%s.analyzeResource: adding a diagram = %s %s", CLSS, res.getResourceName(), (startup?"(STARTUP)":""));
				addModifyDiagramResource(res,startup);
				if(DEBUG) log.infof("%s.analyzeResource: diagram %s successfully added!", CLSS, res.getResourceName() );
			}
			else if( type.equals(BLTProperties.FOLDER_RESOURCE_TYPE) ) {
				if(DEBUG) log.infof("%s.analyzeResource: adding a folder = %s %s", CLSS, res.getResourceName(), (startup?"(STARTUP)":""));
				addModifyFolderResource(res);
			}
			else {
				// Don't care
				if(DEBUG) log.infof("%s.analyzeResource: Ignoring %s resource",CLSS,type);
			}
		}
	}
	/**
	 * @return a list of all known diagrams 
	 */
	public List<ProcessDiagram> getDiagrams() {
		List<ProcessDiagram> diagrams = new ArrayList<>();
		for(ProcessNode node:nodesByResourceId.values()) {
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
	
	public Map<String,ProcessNode> getNodesById() { return nodesByResourceId; }
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
	 * Get a specified diagram given projectId and resourceId. 
	 * @param projectId
	 * @param resourceId

	 * @return the specified diagram. If not found, return null. 
	 */
	public ProcessDiagram getDiagram(ProjectResourceId resourceId) {
		ProcessDiagram diagram = null;
		ProcessNode node = nodesByResourceId.get(ResourceKey.keyForResource(resourceId));
		if( node instanceof ProcessDiagram ) diagram = (ProcessDiagram)node;
		return diagram;
	}
	
	/**
	 * @return the named application
	 */
	public ProcessApplication getApplication(String name) {
		ProcessApplication app = null;
		for(ProcessNode node:nodesByResourceId.values()) {
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
	public synchronized List<SerializableResourceDescriptor> getDiagramDescriptors(String projectName) {
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
					descriptor.setType(BLTProperties.DIAGRAM_RESOURCE_TYPE.getTypeId());
					result.add(descriptor);
				}
			}
		}
		else {
			log.warnf("%s.getDiagramDescriptors: Project %s not found", CLSS,projectName);
		}
		return result;	
	}
	
	/**
	 * Get a list of all diagram resource paths
	 * @return a list of diagram resource paths. If none found, return null. 
	 */
	public synchronized List<SerializableResourceDescriptor> getDiagramDescriptors() {
		if(DEBUG) log.infof("%s.getDiagramDescriptors", CLSS);
		List<SerializableResourceDescriptor> result = new ArrayList<>();
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
					descriptor.setType(BLTProperties.DIAGRAM_RESOURCE_TYPE.getTypeId());
					result.add(descriptor);
				}
			}
		}
		if(DEBUG) log.infof("%s.getDiagramDescriptors: found %d", CLSS,result.size());
		return result;	
	}
	/**
	 * @return the named family under the named application
	 */
	public ProcessFamily getFamily(String appName,String famName) {
		ProcessFamily fam = null;
		ProcessApplication app = getApplication(appName);
		if( app!=null ) {
			for(ProcessNode node:nodesByResourceId.values()) {
				if( node instanceof ProcessFamily &&
					node.getName().equals(famName)) {
					// Check to see if the parent of the family is the app..
					String parentPath = node.getParentPath().getPath().toString();
					String applicationPath = app.getPath().toString();
					if( applicationPath.equalsIgnoreCase(parentPath)) {
						fam = (ProcessFamily)node;
						break;
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
	// Node must be in the nav-tree. Include project name.
	public String pathForNode(ProjectResourceId nodeId) {
		String path = "";
		ProcessNode node = nodesByResourcePath.get(nodeId.getResourcePath());
		if( node!=null) {
			// treePath includes "root", replace this with project.
			path = node.getPath().toString();
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
				if( node instanceof ProcessApplication ) sd.setType(BLTProperties.APPLICATION_RESOURCE_TYPE.getTypeId());
				else if( node instanceof ProcessFamily ) sd.setType(BLTProperties.FAMILY_RESOURCE_TYPE.getTypeId());
				else if( node instanceof ProcessDiagram )sd.setType(BLTProperties.DIAGRAM_RESOURCE_TYPE.getTypeId());
				else sd.setType(BLTProperties.FOLDER_RESOURCE_TYPE.getTypeId());
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
		nodesByResourceId.clear();
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
		for( ProcessNode node:nodesByResourceId.values() ) {
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
		for( ProcessNode node:nodesByResourceId.values() ) {
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
				if(DEBUG) log.infof("%s.projectAdded: resource %s.%s (%s)", CLSS, projectName,res.getResourceName(), res.getResourceType().getTypeId());
				analyzeResource(res,false);
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
		deleteProjectResources(projectName);
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
	public void projectUpdated(String projectName) {
		Optional<RuntimeProject> optional = context.getProjectManager().getProject(projectName);
		Project diff = optional.get();
		if(DEBUG) log.infof("%s.projectUpdated: %s",CLSS,diff.getName());
		
		if( diff.isEnabled() ) {
			int countOfInteresting = 0;
			List<ProjectResource> resources = diff.getResources();
			for( ProjectResource res:resources ) {
				if( isBLTResource(res.getResourceType().getTypeId()) || res.getResourceType().getTypeId().equalsIgnoreCase("Window") ) {
					if(DEBUG) log.infof("%s.projectUpdated: add/update resource %s.%s %s (%s) %s", CLSS,projectName,res.getResourceName(),
							res.getResourcePath().getPath().toString(),
							res.getResourceType().toString(),(res.isLocked()?"locked":"unlocked"));
					analyzeResource(res,false);  // Not startup
					countOfInteresting++;
				}
			}
			
			// If there haven't been any interesting resources, then we've probably
			// just changed the enabled status of the project. Synchronize resources.
			if( countOfInteresting==0) {

				log.debug("============================== ENABLED =================================");
				for( ProjectResource res: diff.getResources() ) {
					if(DEBUG) log.infof("%s.projectUpdated: enabling %s:%s %s",CLSS,res.getResourceName(),res.getResourceId().getResourcePath().getPath().toString());
				}
				
				for( ProjectResource res: diff.getResources() ) {
					analyzeResource(res,false);
				}
			}
		}
		// Delete the BLT resources of projects that are disabled. There is nothing displayable in the Designer
		else {   
			for( ProjectResource res: diff.getResources() ) {
				if(DEBUG) log.infof("%s.projectUpdated: disabling %s:%s %s",CLSS,res.getResourceName(),res.getResourceId().getResourcePath().getPath().toString());
			}
			deleteProjectResources(projectName);
			
			log.info("============================== DISABLED =================================");
		}
	}
	
	// ===================================== Private Methods ==========================================
	/**
	 * Add or update an application in the model from a ProjectResource.
	 * This is essentially just a tree node. Use presence in the node map
	 * to determine whether or not this is a new resource or an update.
	 * If the startup flag is set, the resource 
	 * is modified to include aux data from the extension script.
	 * @param projectId the identity of a project
	 * @param res the project resource containing the diagram
	 * @param startup true if called from the gateway hook
	 */
	private void addModifyApplicationResource(ProjectResource res,boolean startup) {
		ProjectResourceId resId = res.getResourceId();
		if(DEBUG) log.infof("%s.addModifyApplicationResource: %s(%s)",CLSS,resId.getProjectName(),resId.getResourcePath().getPath().toString());
		SerializableApplication sa = deserializeApplicationResource(res);
		if(sa!=null ) {
			ProcessApplication application = new ProcessApplication(sa.getName(),resId.getResourcePath().getParent(),resId);
			application.setAuxiliaryData(sa.getAuxiliaryData());
			ProcessNode node = nodesByResourcePath.get(resId.getResourcePath());
			if( node==null ) {
				// Add in the new Application
				nodesByResourceId.put(ResourceKey.keyForResource(application.getResourceId()),application);
				addToHierarchy(application);
			}
			else  {
				// Update attributes for application stored in tree.
				node.setName(res.getResourceName());
				if(node instanceof ProcessApplication )  {
					ProcessApplication processApp = (ProcessApplication)node;
					processApp.setState(application.getState());
					processApp.setAuxiliaryData(sa.getAuxiliaryData());;
				}
			}
			// Invoke extension script on application save except on startup
			// On startup we need to update the project resource with AuxData
			if( !startup ) {
				String provider = (application.getState().equals(DiagramState.ACTIVE) ? 
						toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER):
							toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER));
				Script script = extensionManager.createExtensionScript(ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.SAVE_OPERATION, provider);
				extensionManager.runScript(context.getProjectManager().getProjectScriptManager(application.getProjectName()), 
						script, application.getPath().toString());
				String db = (application.getState().equals(DiagramState.ACTIVE) ? 
						toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_DATABASE):
							toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE));

				script = extensionManager.createExtensionScript(ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.SET_AUX_OPERATION, provider);
				extensionManager.runScript(context.getScriptManager(), script, application.getPath().toString(),application.getAuxiliaryData(),db);
				
			}
			controller.sendAuxDataNotification(application.getPath().toString(), new BasicQualifiedValue(application.getAuxiliaryData()));
		}
		else {
			log.warnf("%s.addModifyApplicationResource: failed to deserialize %s(%s)",CLSS,resId.getProjectName(),resId.getResourcePath().getPath().toString());
		}
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
				diagram = new ProcessDiagram(sd,res.getResourcePath().getParent(),res.getProjectName());

				// Add the new diagram to our hierarchy
				nodesByResourceId.put(ResourceKey.keyForResource(diagram.getResourceId()),diagram);
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
			// Invoke extension script for the diagram itself
			// The SAVE script is smart enough to do an insert if diagram is new.
			if( !startup ) {
				String provider = (diagram.getState().equals(DiagramState.ACTIVE) ? 
						toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER):
						toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER));
				Script script = extensionManager.createExtensionScript(ScriptConstants.DIAGRAM_CLASS_NAME, ScriptConstants.SAVE_OPERATION, provider);
				extensionManager.runScript(context.getProjectManager().getProjectScriptManager(diagram.getProjectName()), 
						script, diagram.getPath());
			}
		}
		else {
			log.warnf("%s.addModifyDiagramResource - Failed to create diagram from resource (%s)",CLSS,res.getResourceName());
		}
	}	
	
	/**
	 * Add or update a family in the model from a ProjectResource.
	 * This is essentially just a tree node. If the startup flag is set, the resource 
	 * is modified to include aux data from the extension script.
	 * @param projectId the identity of a project
	 * @param res the project resource containing the diagram
	 * @param startup
	 */
	private void addModifyFamilyResource(ProjectResource res,boolean startup) {
		ProjectResourceId resId = res.getResourceId();
		BlockTagSynchronizer bts = new BlockTagSynchronizer(res.getProjectName());
		if(DEBUG) log.infof("%s.addModifyFamilyResource: %s(%s)",CLSS,res.getResourceName(),resId.getResourcePath().getPath().toString());
		SerializableFamily sf = deserializeFamilyResource(res);
		if( sf!=null ) {
			ProcessFamily family = new ProcessFamily(sf.getName(),resId.getResourcePath().getParent(),resId);
			family.setAuxiliaryData(sf.getAuxiliaryData());
			ProcessNode node = nodesByResourcePath.get(resId.getResourcePath());
			if( node==null ) {
				// Add in the new Family
				nodesByResourceId.put(ResourceKey.keyForResource(family.getResourceId()),family);
				addToHierarchy(family);
			}
			else {
				// Update attributes
				node.setName(res.getResourceName());
				if( node instanceof ProcessFamily ) {
					ProcessFamily processFam = (ProcessFamily)node;
					processFam.setState(family.getState());
					processFam.setAuxiliaryData(sf.getAuxiliaryData());
				}
			}
			// Invoke extension script on family save except on startup
			if( !startup ) {
				String provider = (family.getState().equals(DiagramState.ACTIVE) ? 
						toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER):
							toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER));
				Script script = extensionManager.createExtensionScript(ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.SAVE_OPERATION, provider);
				extensionManager.runScript(context.getProjectManager().getProjectScriptManager(family.getProjectName()), 
						script, family.getPath().toString());
				String db = (family.getState().equals(DiagramState.ACTIVE) ? 
						toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_DATABASE):
							toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE));

				script = extensionManager.createExtensionScript(ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.SET_AUX_OPERATION, provider);
				extensionManager.runScript(context.getScriptManager(), script, family.getPath(),family.getAuxiliaryData(),db);
			}
			controller.sendAuxDataNotification(family.getPath(), new BasicQualifiedValue(family.getAuxiliaryData()));
		}
		else {
			log.warnf("%s.addModifyFamilyResource: failed to deserialize %s(%s)",CLSS,res.getResourceName(),
					res.getResourceId().getResourcePath().getPath().toString());
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
		if(DEBUG) log.infof("%s.addFolderResource:  %s(%s)",CLSS,res.getResourceName(),resId.getResourcePath().getPath());
		ProcessNode node = nodesByResourcePath.get(resId.getResourcePath());
		if( node==null ) {
			node = new ProcessNode(res.getResourceName(),resId.getResourcePath().getParent(),resId);
			// Add in the new Folder
			nodesByResourceId.put(ResourceKey.keyForResource(node.getResourceId()),node);
			addToHierarchy(node);
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
		if(DEBUG) log.infof("%s.addToHierarchy: %s (%s)",CLSS,node.getName(),node.getResourceId().getResourcePath().getPath());
		ProjectResourceId self     = node.getResourceId();
		nodesByResourcePath.put(self.getResourcePath(), node);
		
		// If the parent is null, then we're the top of the chain for our project
		// Add the node to the root.
		if( node.getParentPath()==null )  {
			root.addChild(node);
			if(DEBUG) log.infof("%s.addToHierarchy: %s is a ROOT (null parent)",CLSS,node.getName());
		}
		else if( node.getParentPath().isModuleFolder() )  {
			root.addChild(node);
			if(DEBUG) log.infof("%s.addToHierarchy: %s is a ROOT (parent is root folder)",CLSS,node.getName());
		}
		else {
			// If the parent is already in the tree, simply add the node as a child
			// Otherwise add to our list of orphans
			ProcessNode parent = nodesByResourcePath.get(node.getParentPath());
	
			if(parent==null ) {
				if(DEBUG) log.infof("%s.addToHierarchy: %s is an ORPHAN (parent is %s)",CLSS,node.getName(),node.getParentPath().toString());
				orphansByResourcePath.put(self.getResourcePath(), node);
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
		if(DEBUG) log.infof("%s.deleteResource: %s:%s",CLSS,resourceId.getProjectName(),resourceId.getResourcePath().getPath());

		ProcessNode head = nodesByResourceId.get(ResourceKey.keyForResource(resourceId));
		DiagramState nodeState = DiagramState.ACTIVE;
		if( head!=null ) {
			List<ProcessNode> nodesToDelete = new ArrayList<>();
			head.collectDescendants(nodesToDelete);  // "head" is in the list
			for(ProcessNode node:nodesToDelete ) {
				if( node instanceof ProcessDiagram ) {
					ProcessDiagram diagram = (ProcessDiagram)node;
					nodeState = diagram.getState();

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
				nodesByResourceId.remove(ResourceKey.keyForResource(resourceId));
				
				if( node.getParentPath()!=null ) {
					ProcessNode parent = nodesByResourcePath.get(node.getParentPath());
					if( parent!=null ) {
						parent.removeChild(node);
						if( parent.getResourceId().equals(root.getResourceId())) {
							root.removeChildFromProjectRoot(resourceId.getProjectName(),node);
						}
					}
				}
				// Finally remove from the node maps
				nodesByResourcePath.remove(node.getResourceId().getResourcePath());
				// Invoke the proper extension function on a delete
				// NOTE: Need to use keys for class names
				String classKey = ScriptConstants.DIAGRAM_CLASS_NAME;
				if( node instanceof ProcessApplication ) {
					classKey = ScriptConstants.APPLICATION_CLASS_NAME;
					nodeState = ((ProcessApplication)node).getState();
				}
				else if( node instanceof ProcessFamily ) {
					classKey = ScriptConstants.FAMILY_CLASS_NAME;
					nodeState = ((ProcessFamily)node).getState();
				}
							
				// Invoke extension script for the delete
				// Execute for BOTH production and isolation
				if( node!=null ) {
					Script script = extensionManager.createExtensionScript(classKey, ScriptConstants.DELETE_OPERATION, toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER));
					extensionManager.runScript(context.getProjectManager().getProjectScriptManager(node.getProjectName()), 
							script, node.getResourceId().getResourcePath().getPath(),node.getAuxiliaryData());
					script = extensionManager.createExtensionScript(classKey, ScriptConstants.DELETE_OPERATION, toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER));
					extensionManager.runScript(context.getProjectManager().getProjectScriptManager(node.getProjectName()), 
					script, node.getResourceId().getResourcePath().getPath(),node.getAuxiliaryData());
				}
			}
		}
	}
	
	// Delete all process nodes for a given project.
	private void deleteProjectResources(String projectName) {
		if(DEBUG) log.infof("%s.deleteProjectResources: proj = %s",CLSS,projectName);
		List<ProcessNode> nodes = root.allNodesForProject(projectName);
		for(ProcessNode node:nodes) {
			deleteResource(node.getResourceId());
		}
		root.removeProject(projectName);
	}
	
	/**
	 * We've discovered a changed model resource. Deserialize and convert into a ProcessApplication.
	 * Note that the name is wholly contained in the resource, not its contents. Clear out any nested
	 * resources as these are only used when serializing the application outside of a project.
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	public SerializableApplication deserializeApplicationResource(ProjectResource res) {
		SerializableApplication sa = null;
		byte[] serializedObj = res.getData();
		if( serializedObj!=null && serializedObj.length>0 ) {
			String json = new String(serializedObj);
			if(DEBUG) log.infof("%s.deserializeApplicationResource: json = %s",CLSS,json);
			try{
				ObjectMapper mapper = new ObjectMapper();
				sa = mapper.readValue(json, SerializableApplication.class);
				if( sa!=null ) {
					sa.setName(res.getProjectName());
					if(DEBUG) log.infof("%s.deserializeApplicationResource: Successfully deserialized application %s",CLSS,sa.getName());

				}
				else {
					log.warnf("%s.deserializeApplicationResource: deserialization failed",CLSS);
				}
			}
			// Print stack trace
			catch( Exception ex) {
				log.warnf("%s.deserializeApplicationResource: exception (%s)",CLSS,ex.getLocalizedMessage(),ex);
			}
		}
		else {
			log.warnf("%s.deserializeApplicationResource: resource (%s) has no data",CLSS,res.getResourceName());
		}

		return sa;
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
	 * We've discovered a changed model resource. Deserialize and convert into a SerializableFamily.
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	public SerializableFamily deserializeFamilyResource(ProjectResource res) {
		SerializableFamily sf = null;
		byte[] serializedObj = res.getData();
		if( serializedObj!=null && serializedObj.length>0 ) {
			String json = new String(serializedObj);
			if(DEBUG) log.infof("%s.deserializeFamilyResource: json = %s",CLSS,json);

			try{
				ObjectMapper mapper = new ObjectMapper();
				sf = mapper.readValue(json, SerializableFamily.class);
				if( sf!=null ) {
					sf.setName(res.getResourceName());     // Resource is the source of the name.
					if(DEBUG) log.infof("%s.deserializeFamilyResource: Successfully deserialized family %s",CLSS,sf.getName());
				}
				else {
					log.warnf("%s: deserializeFamilyResource: deserialization failed",CLSS);
				}
			}
			// Print stack trace
			catch( Exception ex) {
				log.warnf("%s.deserializeFamilyResource: exception (%s)",CLSS,ex.getLocalizedMessage(),ex);
			}
		}
		else {
			log.warnf("%s.deserializeFamilyResource: resource (%s) has no data",CLSS,res.getResourceName());
		}
		return sf;
	}

	private boolean isBLTResource(String type) {
		boolean isBLTType = false;
		if( type!=null ) {
			if( type.equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE.getTypeId()) ||
				type.equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE.getTypeId()) ||
				type.equalsIgnoreCase(BLTProperties.DIAGRAM_RESOURCE_TYPE.getTypeId()) ) {
				isBLTType = true;
			}
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
		for( ProcessNode orphan:orphansByResourcePath.values()) {
			ProcessNode parent = nodesByResourcePath.get(orphan.getParentPath());
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
			ProcessNode parent = nodesByResourcePath.get(orphan.getParentPath());
			parent.addChild(orphan);
			orphansByResourcePath.remove(orphan.getResourceId().getResourcePath());
		}
	}
}
