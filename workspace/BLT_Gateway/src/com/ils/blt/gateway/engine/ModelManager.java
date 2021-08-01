/**
 *   (c) 2014-2021 ILS Automation. All rights reserved. 
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
import com.ils.blt.common.script.Script;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.common.persistence.ToolkitProperties;
import com.ils.common.persistence.ToolkitRecordHandler;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectListener;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
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
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	private ToolkitRecordHandler toolkitHandler;
	
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
	 * to the engine. Handle both additions and updates. In the case of startup
	 * read the auxiliary data from the database, otherwise we write it.
	 * @param projectId the identity of a project
	 * @param res the model resource
	 * @param startup
	 */
	public void analyzeResource(long projectId,ProjectResource res,boolean startup) {
		
		if( res.getModuleId()!=null && res.getModuleId().equalsIgnoreCase(BLTProperties.MODULE_ID)) {
			String type = res.getResourceType();
			
			if( type.equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
				if(DEBUG) log.infof("%s.analyzeResource: adding an application = %s %s", CLSS, res.getName(), (startup?"(STARTUP)":""));
				addModifyApplicationResource(projectId,res,startup);
			}
			else if( type.equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
				if(DEBUG) log.infof("%s.analyzeResource: adding a family = %s %s", CLSS, res.getName(), (startup?"(STARTUP)":""));
				addModifyFamilyResource(projectId,res,startup);
			}
			else if( type.equalsIgnoreCase(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				if(DEBUG) log.infof("%s.analyzeResource: adding a diagram = %s %s", CLSS, res.getName(), (startup?"(STARTUP)":""));
				addModifyDiagramResource(projectId,res,startup);
				if(DEBUG) log.infof("%s.analyzeResource: diagram %s successfully added!", CLSS, res.getName() );
			}
			else if( type.equalsIgnoreCase(BLTProperties.FOLDER_RESOURCE_TYPE) ) {
				if(DEBUG) log.infof("%s.analyzeResource: adding a folder = %s %s", CLSS, res.getName(), (startup?"(STARTUP)":""));
				addModifyFolderResource(projectId,res);
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
		for(ProcessNode node:nodesByKey.values()) {
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
	 * @param blockId block identifier
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
	public synchronized List<SerializableResourceDescriptor> getDiagramDescriptors(String projectName) {
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
			log.warnf("%s.getDiagramTreePaths: Project %s not found", CLSS,projectName);
		}
		return result;	
	}
	
	/**
	 * Get a list of all diagram tree paths
	 * @return a list of diagram tree paths. If none found, return null. 
	 */
	public synchronized List<SerializableResourceDescriptor> getDiagramDescriptors() {
		if(DEBUG) log.infof("%s.getDiagramDescriptors", CLSS);
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
		if(DEBUG) log.infof("%s.getDiagramDescriptors: found %d", CLSS,result.size());
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
	
	public synchronized List<SerializableBlockStateDescriptor> listBlocksDownstreamOf(UUID diagramId,UUID blockId,boolean spanDiagrams) {
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
			log.warnf("%s.listBlocksDownstreamOf: diagram %s not found", CLSS,diagramId.toString());
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
	public synchronized List<SerializableBlockStateDescriptor> listBlocksUpstreamOf(UUID diagramId,UUID blockId,boolean spanDiagrams) {
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
			log.warnf("%s.listBlocksUpstreamOf: diagram %s not found", CLSS,diagramId.toString());
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
				if(node==null) continue;
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
		if(DEBUG) log.infof("%s.removeAllDiagrams ... complete",CLSS);
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
				try{ Thread.sleep(5000); } catch(InterruptedException ignore) {}
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
                	if(DEBUG) log.infof("%s.projectAdded: resource %d.%d %s (%s)", CLSS, projectId, res.getResourceId(), res.getName(), res.getResourceType());
                    analyzeResource(projectId,res,false);
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
		log.infof("%s.projectDeleted: (id=%d)",CLSS,projectId);
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
		if(DEBUG) log.infof("%s.projectUpdated: %s (%d)  %s",CLSS,diff.getName(),diff.getId(),vers.toString());
		long projectId = diff.getId();
		if( projectId<0 ) return;                   // Ignore global project
		
		UUID olduuid = uuidByProjectId.get(new Long(projectId));
		if( olduuid==null ) {
			log.warnf("%s.projectUpdated: No existing project (%d) found",CLSS,projectId);
		}
		else if( !olduuid.equals(diff.getUuid()) ) {
			log.warnf("%s.projectUpdated: Replacing project (%d)",CLSS,projectId);
			deleteProjectResources(projectId);
		}
		
		if( diff.isEnabled() ) {
			int countOfInteresting = 0;
			List<ProjectResource> resources = diff.getResources();
			Long pid = new Long(projectId);
			for( ProjectResource res:resources ) {
				if( isBLTResource(res.getResourceType()) || res.getResourceType().equalsIgnoreCase("Window") ) {
					if(DEBUG) log.infof("%s.projectUpdated: add/update resource %d.%d %s (%s) %s %s", CLSS,projectId,res.getResourceId(),res.getName(),
							res.getResourceType(),(diff.isResourceDirty(res)?"dirty":"clean"),(res.isLocked()?"locked":"unlocked"));
					if(res.isLocked()) res.setLocked(false);
					analyzeResource(pid,res,false);  // Not startup
					countOfInteresting++;
				}
			}

			Set<Long> deleted = diff.getDeletedResources();
			for (Long  rid : deleted) {
				long resid = rid.longValue();
				if(DEBUG) log.infof("%s.projectUpdated: delete resource %d:%d", CLSS,projectId,resid);
				countOfInteresting++;
				deleteResource(projectId,resid);
			}
			
			// If there haven't been any interesting resources, then we've probably
			// just changed the enabled status of the project. Synchronize resources.
			if( countOfInteresting==0) {
				Project project = context.getProjectManager().getProject(projectId, ApplicationScope.GATEWAY,ProjectVersion.Staging);
				log.debug("============================== ENABLED =================================");
				for( ProjectResource res: project.getResources() ) {
					if(DEBUG) log.infof("%s.projectUpdated: enabling %d:%d %s",CLSS,projectId,res.getResourceId(),res.getName());
				}
				
				for( ProjectResource res: project.getResources() ) {
					analyzeResource(pid,res,false);
				}
			}
		}
		// Delete the BLT resources of projects that are disabled. There is nothing displayable in the Designer
		else {     
			deleteProjectResources(projectId);
			
			log.info("============================== DISABLED =================================");
			Project project = context.getProjectManager().getProject(projectId, ApplicationScope.GATEWAY,ProjectVersion.Staging);
			for( ProjectResource res: project.getResources() ) {
				log.infof("%s.projectUpdated: disabling  %d:%d %s",CLSS,projectId,res.getResourceId(),res.getName());
			}
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
	private void addModifyApplicationResource(long projectId,ProjectResource res,boolean startup) {
		if(DEBUG) log.infof("%s.addModifyApplicationResource: %s(%d)",CLSS,res.getName(),res.getResourceId());
		SerializableApplication sa = deserializeApplicationResource(projectId,res);
		if(sa!=null ) {
			ProcessApplication application = new ProcessApplication(sa,res.getParentUuid());
			application.setResourceId(res.getResourceId());
			application.setProjectId(projectId);
			application.setAuxiliaryData(sa.getAuxiliaryData());
			UUID self = application.getSelf();
			ProcessNode node = nodesByUUID.get(self);
			if( node==null ) {
				// Add in the new Application
				ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
				nodesByKey.put(key,application);
				addToHierarchy(projectId,application);
			}
			else if( node.getProjectName() != projectId)  {
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
				// Update attributes for application stored in tree.
				node.setName(res.getName());
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
						script, application.getSelf().toString());
				String db = (application.getState().equals(DiagramState.ACTIVE) ? 
						toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_DATABASE):
							toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE));

				script = extensionManager.createExtensionScript(ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.SET_AUX_OPERATION, provider);
				extensionManager.runScript(context.getScriptManager(), script, application.getSelf().toString(),application.getAuxiliaryData(),db);
				
			}
			controller.sendAuxDataNotification(application.getSelf().toString(), new BasicQualifiedValue(application.getAuxiliaryData()));
		}
		else {
			log.warnf("%s.addModifyApplicationResource: failed to deserialize %s(%d)",CLSS,res.getName(),res.getResourceId());
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
	private void addModifyDiagramResource(long projectId,ProjectResource res,boolean startup) {
		if(DEBUG) log.infof("%s.addModifyDiagramResource: adding diagram: %s (%d)", CLSS, res.getName(), res.getResourceId());
		SerializableDiagram sd = deserializeDiagramResource(projectId,res);

		if( sd!=null ) {
			ProcessDiagram diagram = (ProcessDiagram)nodesByUUID.get(sd.getId());
			if( diagram==null) {   // this is usually run during gateway start up.
				// Create a new diagram
				if(DEBUG) log.infof("%s.addModifyDiagramResource: Creating diagram %s(%s) %s", CLSS,res.getName(),
						sd.getId().toString(),sd.getState().name());
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
					diagram.synchronizeSubscriptions();
				}
			}
			else if(diagram.getProjectName() != projectId) {
				// The same UUID, but a different project, is a different resource
				// Check the node and parent UUIDs:
				//     if they haven't already been migrated, do it here.
				if(DEBUG) log.infof("%s.addModifyDiagramResource: Replicating diagram %s in new project", CLSS,res.getName());
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
				diagram.createAttributeDisplays(sd.getAttributeDisplays());
				diagram.updateConnections(sd.getConnections());
				diagram.updateProperties(sd);                    // Fixes subscriptions, as necessary
				diagram.setState(sd.getState());                 // Handle state change, if any

			}
			// Carefully update the diagram with new features/properties.
			// Leave existing blocks/subscriptions "as-is". 
			else {
				if(DEBUG) log.infof("%s.addModifyDiagramResource: Updating diagram %s", CLSS,res.getName());
				diagram.setName(sd.getName());
				// Delete all the old connections
				diagram.clearConnections();
				// Delete blocks in the old that are not present in the new.
				// Stop subscriptions associated with those blocks.
				// Execute "delete" extension function for removed blocks
				List<ProcessBlock> deletedBlocks = diagram.removeUnusedBlocks(sd.getBlocks());
				for(ProcessBlock deletedBlock:deletedBlocks) {
					deletedBlock.onDelete();
				}
				diagram.createBlocks(sd.getBlocks());            // Adds blocks that are new in update
				diagram.createAttributeDisplays(sd.getAttributeDisplays());
				diagram.updateConnections(sd.getConnections());  // Adds connections that are new in update
				diagram.updateProperties(sd);                    // Fixes subscriptions, as necessary
				diagram.setState(sd.getState());// Handle state change, if any
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
						script, diagram.getSelf().toString());
			}
		}
		else {
			log.warnf("%s.addModifyDiagramResource - Failed to create diagram from resource (%s)",CLSS,res.getName());
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
	private void addModifyFamilyResource(long projectId,ProjectResource res,boolean startup) {
		if(DEBUG) log.infof("%s.addModifyFamilyResource: %s(%d)",CLSS,res.getName(),res.getResourceId());
		SerializableFamily sf = deserializeFamilyResource(projectId,res);
		if( sf!=null ) {
			ProcessFamily family = new ProcessFamily(sf,res.getParentUuid());
			family.setResourceId(res.getResourceId());
			family.setProjectId(projectId);
			family.setAuxiliaryData(sf.getAuxiliaryData());
			UUID self = family.getSelf();
			ProcessNode node = nodesByUUID.get(self);
			if( node==null ) {
				// Add in the new Family
				ProjectResourceKey key = new ProjectResourceKey(projectId,res.getResourceId());
				nodesByKey.put(key,family);
				addToHierarchy(projectId,family);
			}
			else if(node.getProjectName() != projectId) {
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
					processFam.setAuxiliaryData(sf.getAuxiliaryData());;
				}
			}
			// Invoke extension script on family save except on startup
			if( !startup ) {
				String provider = (family.getState().equals(DiagramState.ACTIVE) ? 
						toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER):
							toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER));
				Script script = extensionManager.createExtensionScript(ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.SAVE_OPERATION, provider);
				extensionManager.runScript(context.getProjectManager().getProjectScriptManager(family.getProjectName()), 
						script, family.getSelf().toString());
				String db = (family.getState().equals(DiagramState.ACTIVE) ? 
						toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_DATABASE):
							toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE));

				script = extensionManager.createExtensionScript(ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.SET_AUX_OPERATION, provider);
				extensionManager.runScript(context.getScriptManager(), script, family.getSelf().toString(),family.getAuxiliaryData(),db);
			}
			controller.sendAuxDataNotification(family.getSelf().toString(), new BasicQualifiedValue(family.getAuxiliaryData()));
		}
		else {
			log.warnf("%s.addModifyFamilyResource: failed to deserialize %s(%d)",CLSS,res.getName(),res.getResourceId());
		}

	}
	/**
	 * Add or update a folder resource from the ProjectResource.
	 * @param projectId the identity of a project
	 * @param resourceId the identity of the model resource
	 * @param model the diagram logic
	 */
	private void addModifyFolderResource(long projectId,ProjectResource res) {
		if(DEBUG) log.infof("%s.addFolderResource: %s(%d)",CLSS,res.getName(),res.getResourceId());
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
		else if(node.getProjectName() != projectId) {
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
		if(DEBUG) log.infof("%s.addToHierarchy: %s (%d:%s)",CLSS,node.getName(),node.getResourceId(),node.getSelf().toString());
		UUID self     = node.getSelf();
		nodesByUUID.put(self, node);
		
		// If the parent is null, then we're the top of the chain for our project
		// Add the node to the root.
		if( node.getParent()==null )  {
			root.addChild(node,projectId);
			if(DEBUG) log.infof("%s.addToHierarchy: %s is a ROOT (null parent)",CLSS,node.getName());
		}
		else if( node.getParent().equals(BLTProperties.ROOT_FOLDER_UUID) )  {
			root.addChild(node,projectId);
			if(DEBUG) log.infof("%s.addToHierarchy: %s is a ROOT (parent is root folder)",CLSS,node.getName());
		}
		else {
			// If the parent is already in the tree, simply add the node as a child
			// Otherwise add to our list of orphans
			ProcessNode parent = nodesByUUID.get(node.getParent());
			if( parent!=null && parent.getProjectName()!=node.getProjectName()) {
				// We need to use the migrated parent
				ProjectUUIDKey pukey = new ProjectUUIDKey(node.getProjectName(),parent.getSelf());	 		
				if( uuidMigrationMap.get(pukey) != null ) {
					UUID parentuuid = uuidMigrationMap.get(pukey);
					parent = nodesByUUID.get(parentuuid);
				}
			}
	
			if(parent==null ) {
				if(DEBUG) log.infof("%s.addToHierarchy: %s is an ORPHAN (parent is %s)",CLSS,node.getName(),node.getParent().toString());
				orphansByUUID.put(self, node);
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
	public void deleteResource(long projectId,long resourceId) {
		if(DEBUG) log.infof("%s.deleteResource: %d:%d",CLSS,projectId,resourceId);
		ProjectResourceKey key = new ProjectResourceKey(projectId,resourceId);
		ProcessNode head = nodesByKey.get(key);
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
							if(DEBUG) log.infof("%s.deleteResource:Deleting a sink",CLSS,projectId,resourceId);
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
							script, node.getSelf().toString(),node.getAuxiliaryData());
					script = extensionManager.createExtensionScript(classKey, ScriptConstants.DELETE_OPERATION, toolkitHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER));
					extensionManager.runScript(context.getProjectManager().getProjectScriptManager(node.getProjectName()), 
					script, node.getSelf().toString(),node.getAuxiliaryData());
				}
			}
		}
	}
	
	// Delete all process nodes for a given project.
	private void deleteProjectResources(long projectId) {
		if(DEBUG) log.infof("%s.deleteProjectResources: proj = %d",CLSS,projectId);
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
	public SerializableApplication deserializeApplicationResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		String json = new String(serializedObj);
		if(DEBUG) log.infof("%s.deserializeApplicationResource: json = %s",CLSS,json);
		SerializableApplication sa = null;
		try{
			ObjectMapper mapper = new ObjectMapper();
			sa = mapper.readValue(json, SerializableApplication.class);
			if( sa!=null ) {
				sa.setName(res.getName());
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
		return sa;
	}
	/**
	 *  We've discovered a changed model resource. Deserialize and return.
	 *  Note: We had difficulty with the Ignition XML serializer because it didn't handle Java generics;
	 *        thus the use of JSON. The returned object was not an instanceof...
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	public SerializableDiagram deserializeDiagramResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		SerializableDiagram sd = null;
		try{
			String json = new String(serializedObj);
			if(DEBUG) log.infof("%s.deserializeDiagramResource: json = %s",CLSS,json);
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			sd = mapper.readValue(json, SerializableDiagram.class);
			if( sd!=null ) {
				sd.setName(res.getName());       // Name comes from the resource
				if(DEBUG) log.infof("%s.deserializeDiagramResource: Successfully deserialized diagram %s",CLSS,sd.getName());
				sd.setResourceId(res.getResourceId());
				if( DEBUG ) {
					for(SerializableBlock sb:sd.getBlocks()) {
						log.infof("%s: %s block, name = %s",CLSS,sb.getClassName(),sb.getName());
					}
				}
			}
			else {
				log.warnf("%s.deserializeDiagramResource: deserialization failed",CLSS);
			}
		}
		// Print stack trace
		catch( Exception ex) {
			log.warnf("%s.deserializeDiagramResource: exception (%s)",CLSS,ex.getLocalizedMessage(),ex);
		}
		return sd;
	}
	
	/**
	 * We've discovered a changed model resource. Deserialize and convert into a SerializableFamily.
	 * @param projId the identifier of the project
	 * @param res
	 */ 
	public SerializableFamily deserializeFamilyResource(long projId,ProjectResource res) {
		byte[] serializedObj = res.getData();
		String json = new String(serializedObj);
		if(DEBUG) log.infof("%s.deserializeFamilyResource: json = %s",CLSS,json);
		SerializableFamily sf = null;
		try{
			ObjectMapper mapper = new ObjectMapper();
			sf = mapper.readValue(json, SerializableFamily.class);
			if( sf!=null ) {
				sf.setName(res.getName());     // Resource is the source of the name.
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
		return sf;
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
				if( parent.getProjectName()!=orphan.getProjectName()) {
					// We need to use the migrated parent
					ProjectUUIDKey pukey = new ProjectUUIDKey(orphan.getProjectName(),parent.getSelf());	 		
					if( uuidMigrationMap.get(pukey) != null ) {
						UUID parentuuid = uuidMigrationMap.get(pukey);
						orphan.setParent(parentuuid);
						parent = nodesByUUID.get(parentuuid);
					}
				}
				
				if( parent!=null ) {
					if(DEBUG) log.infof("%s.resolveOrphans: %s RECONCILED with parent (%s)",CLSS,orphan.getName(),parent.getName());
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
