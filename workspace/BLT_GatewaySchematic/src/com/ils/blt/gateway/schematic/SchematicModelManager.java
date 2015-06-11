/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.schematic;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.common.BasicDiagram;
import com.ils.blt.gateway.engine.ModelManager;
import com.ils.blt.gateway.engine.ProcessNode;
import com.inductiveautomation.ignition.common.project.ProjectResource;
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
public class SchematicModelManager extends ModelManager  {
	
	private static String TAG = "ModelManager";
	
	/**
	 * Initially we query the gateway context to discover what resources exists. After that
	 * we rely on notifications of project resource updates. After discovering block resources
	 * we deserialize and inform the BlockExecutionController.
	 * 
	 * @param cntx the gateway context. 
	 */
	public SchematicModelManager(GatewayContext ctx) { 
		super(ctx);
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
			
			if( type.equalsIgnoreCase(BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE) ) {
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
	 * Walk the node tree and create a list of all resources currently being manned by
	 * the model manager.
	 * 
	 * @return
	 */
	@Override
	public List<SerializableResourceDescriptor> queryControllerResources() {
		List<SerializableResourceDescriptor> result = new ArrayList<SerializableResourceDescriptor>();
		for( Long projectId:root.allProjects() ) {
			for(ProcessNode node: root.allNodesForProject(projectId)) {
				SerializableResourceDescriptor sd = new SerializableResourceDescriptor();
				sd.setName(node.getName());
				sd.setProjectId(projectId.longValue());
				sd.setResourceId(node.getResourceId());
				if( node instanceof SchematicDiagram )sd.setType(BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE);
				else sd.setType(BLTProperties.FOLDER_RESOURCE_TYPE);
				result.add(sd);
			}
		}
		return result;
	}


	

	// ===================================== Private Methods ==========================================
	
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
			BasicDiagram diagram = (BasicDiagram)nodesByUUID.get(sd.getId());
			if( diagram==null) {
				// Create a new diagram
				diagram = new SchematicDiagram(sd,res.getParentUuid(),projectId);
				diagram.setResourceId(res.getResourceId());
				diagram.setProjectId(projectId);
				// Add the new diagram to our hierarchy
				ProjResKey key = new ProjResKey(projectId,res.getResourceId());
				nodesByKey.put(key,diagram);
				addToHierarchy(projectId,diagram);
				diagram.createBlocks(sd);
				diagram.updateConnections(sd);
				diagram.startSubscriptions();
			}
			// Carefully update the diagram with new features/properties.
			// Leave existing blocks/subscriptions "as-is". 
			else {
				diagram.setName(sd.getName());
				// Delete all the old connections
				diagram.clearConnections();
				// Delete blocks in the old that are not present in the new.
				// Stop subscriptions associated with those blocks.
				diagram.removeUnusedBlocks(sd.getBlocks());
				diagram.createBlocks(sd);       // Adds blocks that are new in update
				diagram.updateConnections(sd);  // Adds connections that are new in update
				diagram.updateProperties(sd);
				diagram.setState(sd.getState());// Handle state change, if any
			}
			/*
			if( !diagram.getState().equals(DiagramState.DISABLED) ) {
				diagram.updateBlockTimers();  // Make sure timers are correct for current diagram state.
				log.tracef("%s.addModifyDiagramResource: starting tag subscriptions ...%d:%s",TAG,projectId,res.getName());
				for( CoreBlock pb:diagram.getCoreBlocks()) {
					for(BlockProperty bp:pb.getProperties()) {
						controller.startSubscription(pb,bp);   // Does nothing for existing subscriptions
					}
					pb.setProjectId(projectId);
				}
				if( BlockExecutionController.getExecutionState().equals(BlockExecutionController.CONTROLLER_RUNNING_STATE)) {
					log.tracef("%s.addModifyDiagramResource: starting blocks ...%d:%s",TAG,projectId,res.getName());
					for( CoreBlock pb:diagram.getCoreBlocks()) {
						pb.start();
					}
				}
			}
			else {
				log.infof("%s.addModifyDiagramResource: diagram is DISABLED (did not start subscriptions)...%d:%s",TAG,projectId,res.getName());
			}
			*/
		}
		else {
			log.warnf("%s.addModifyDiagramResource - Failed to create diagram from resource (%s)",TAG,res.getName());
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
}
