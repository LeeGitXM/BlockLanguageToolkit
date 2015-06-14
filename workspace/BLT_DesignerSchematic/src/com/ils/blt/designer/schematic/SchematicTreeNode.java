/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *  Based on sample code provided by Inductive Automation.
 */
package com.ils.blt.designer.schematic;

import java.awt.Component;
import java.awt.EventQueue;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Enumeration;
import java.util.List;
import java.util.UUID;

import javax.swing.JPopupMenu;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFolder;
import com.ils.blt.common.serializable.UUIDResetHandler;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.ResourceCreateManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.navtree.ImportDialog;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
/**
 * A folder in the designer scope to support schematic diagrams.
 * The tree contains only folders and diagrams.
 * 
 * Leaf nodes are of type DiagramTreeNode.
 */
public class SchematicTreeNode extends GeneralPurposeTreeNode  {


	/** 
	 * Create a new folder node representing the root folder. The root folder does
	 * not worry about cleanliness.
	 * @param ctx the designer context
	 */
	public SchematicTreeNode(DesignerContext ctx,DiagramWorkspace wksp,ToolkitRequestHandler handler,NodeStatusManager sm) {
		super(ctx,BLTProperties.SCHEMATIC_ROOT_FOLDER_UUID,wksp,handler,sm);
		this.setName(BLTProperties.SCHEMATIC_ROOT_FOLDER_NAME);
		setText(BundleUtil.get().getString(PREFIX+".RootFolderSchematicName"));
	}
	/**
	 * This version of the constructor is used for all except the root. Create
	 * either a simple folder, an application or family container or a diagram holder.
	 * This all depends on the resource type.
	 * 
	 * @param context the designer context
	 * @param resource the project resource
	 * @param self UUID of the node itself
	 */
	public SchematicTreeNode(DesignerContext context,ProjectResource resource,UUID self,DiagramWorkspace wksp,
								  ToolkitRequestHandler handler, NodeStatusManager sm) {
		super(context,resource,self,wksp,handler,sm);

		// Simple folder
		closedIcon = IconUtil.getIcon("folder_closed");
		openIcon = IconUtil.getIcon("folder");
		setIcon(closedIcon);
	}


	/**
	 * Create a child node because we've discovered a resource that matches this instance as a parent
	 * based on its content matching the our UUID. If the node had been previously created, then 
	 * reuse it.
	 */
	@Override
	protected AbstractNavTreeNode createChildNode(ProjectResource res) {
		logger.debugf("%s.createChildNode: %s(%d) type:%s, depth=%d", TAG,getName(),resourceId,res.getResourceType(),getDepth());
		AbstractResourceNavTreeNode node = statusManager.findNode(res.getResourceId());
		if( node==null ) {
			if (    ProjectResource.FOLDER_RESOURCE_TYPE.equals(res.getResourceType()))       {
				node = new SchematicTreeNode(context, res, res.getDataAsUUID(),workspace,requestHandler,statusManager);
				logger.tracef("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
			}
			else if (BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE.equals(res.getResourceType())) {
				node = new SchematicTreeNode(context, res, res.getDataAsUUID(),workspace,requestHandler,statusManager);
				logger.tracef("%s.createChildDiagram: %s->%s",TAG,this.getName(),node.getName());
			} 
			else {
				logger.warnf("%s: Attempted to create a child of type %s (ignored)",TAG,res.getResourceType());
				throw new IllegalArgumentException();
			}
			statusManager.createResourceStatus(node,resourceId, res.getResourceId());
			executionEngine.executeOnce(new ResourceUpdateManager(workspace,res));   /// Creates, syncs resource
		}
		else {
			logger.debugf("%s.createChildNode: REUSE %s->%s",TAG,this.getName(),node.getName());
			if( node instanceof DiagramTreeNode ) context.addProjectChangeListener((DiagramTreeNode)node);
		}
		node.install(this);
		if( node.getParent()==null) {
			logger.errorf("%s.createChildNode: ERROR parent is null %s(%d)",TAG,node.getName(),res.getResourceId());
		}
		node.setItalic(statusManager.isResourceDirtyOrHasDirtyChidren(res.getResourceId()));
		return node;
	}
	
	/**
	 * Define the menu used for popups. This appears to be called each time a menu is called for ..
	 */
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		if( this.getParent()==null ) {
			logger.errorf("%s.initPopupMenu: ERROR: Diagram (%d) has no parent",TAG,hashCode());
		}
		context.addProjectChangeListener(this);
		if (isRootFolder()) { 
			ClearAction clearAction = new ClearAction();
			DebugAction debugAction = new DebugAction();
			SaveAllAction saveAllAction = new SaveAllAction(this);
			if( requestHandler.isControllerRunning() ) {
				startAction.setEnabled(false);
				clearAction.setEnabled(false);
			}
			else {
				stopAction.setEnabled(false);
				clearAction.setEnabled(true);
			}
			menu.add(saveAllAction);
			menu.add(startAction);
			menu.add(stopAction);
			menu.addSeparator();
			menu.add(clearAction);
			menu.add(debugAction);
		}
		else if( getProjectResource()==null ) {
			logger.warnf("%s.initPopupMenu: ERROR: node %s(%d) has no project resource",TAG,this.getName(),resourceId);
		}
		
		else if(getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.FOLDER_RESOURCE_TYPE)) {

			DiagramCreateAction diagramAction = new DiagramCreateAction(this);
			menu.add(diagramAction);
			ImportDiagramAction importAction = new ImportDiagramAction(menu.getRootPane(),this);
			CloneDiagramAction cloneAction = new CloneDiagramAction(menu.getRootPane(),this);
			menu.add(importAction);
			menu.add(cloneAction);
			menu.add(folderCreateAction);
			treeSaveAction = new TreeSaveAction(this,PREFIX+".SaveFolder");
			menu.add(treeSaveAction);
			menu.addSeparator();
			addEditActions(menu);	
		}
		else {   
			addEditActions(menu);
		}
	}


	protected boolean isRootFolder() {
		return getFolderId().equals(BLTProperties.SCHEMATIC_ROOT_FOLDER_UUID);
	}

	
	// Recursively descend the node tree, gathering up associated resources.
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to an application.
	private SerializableFolder recursivelyDeserializeFolder(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		SerializableFolder sfold = null;
		if( res!=null ) {
			logger.infof("%s.recursivelyDeserializeFolder: %s (%d)",TAG,res.getName(),res.getResourceId());
			sfold = new SerializableFolder();
			sfold.setId(res.getDataAsUUID());
			sfold.setName(res.getName());
			sfold.setParentId(res.getParentUuid());

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				ProjectResource cres = child.getProjectResource();
				if(cres==null) continue;
				if( cres.getResourceType().equals(BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE)) {
					SerializableDiagram sd = recursivelyDeserializeDiagram(child);
					if( sd!=null ) sfold.addDiagram(sd);
				}
				else if(cres.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
					SerializableFolder sf = recursivelyDeserializeFolder(child);
					if( sf!=null ) sfold.addFolder(sf);
				}
				else {
					logger.infof("%s.recursivelyDeserializeFolder: %s unexpected child resource type (%s)",TAG,res.getName(),cres.getName(),cres.getResourceType());
				}
			}
		}
		return sfold;
	}

	// ============================================= private action classes ===========================================================


	// Create a new diagram
	private class DiagramCreateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode currentNode;
		public DiagramCreateAction(AbstractResourceNavTreeNode parentNode)  {
			super(PREFIX+".NewDiagram",IconUtil.getIcon("folder_new"));  // preferences
			this.currentNode = parentNode;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".NewDiagram.Default.Name");
				if( newName==null) newName = "New Diag";  // Missing string resource
				SerializableDiagram diagram = new SerializableDiagram();
				diagram.setName(newName);
				diagram.setResourceId(newId);
				diagram.setId(UUID.randomUUID());
				diagram.setDirty(false);    // Will become dirty as soon as we add a block

				String json = serializeDiagram(diagram);	
				logger.debugf("%s.DiagramCreateAction. json=%s",TAG,json);
				byte[] bytes = json.getBytes();
				logger.infof("%s.DiagramCreateAction. create new %s(%d), %s (%d bytes)",TAG,BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE,newId,
						newName,bytes.length);
				ProjectResource resource = new ProjectResource(newId,BLTProperties.MODULE_ID, BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				logger.infof("%s: parent: %s",TAG,getFolderId().toString());
				new ResourceCreateManager(resource).run();	
				currentNode.selectChild(new long[] {newId} );
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						workspace.open(newId);
					}
				});

			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception creating diagram",err);
			}
		}
	}
	
	private class ImportDiagramAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode parentNode;
		private final Component anchor;
		private final static String POPUP_TITLE = "Import Diagram";
		public ImportDiagramAction(Component c,AbstractResourceNavTreeNode pNode)  {
			super(PREFIX+".ImportDiagram",IconUtil.getIcon("import1"));  // preferences
			this.parentNode = pNode;
			this.anchor = c;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						long newId;

						try {
							newId = context.newResourceId();
							String title = BundleUtil.get().getString(PREFIX+".Import.Application.DialogTitle");
							String label = BundleUtil.get().getString(PREFIX+".Import.Application.NameLabel");
							ImportDialog dialog = new ImportDialog(context.getFrame(),label,title);
							dialog.setLocationRelativeTo(anchor);
							Point p = dialog.getLocation();
	    					dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
							dialog.pack();
							dialog.setVisible(true);   // Returns when dialog is closed
							File input = dialog.getFilePath();

							if( input!=null ) {
								if( input.exists() && input.canRead()) {
									try {
										// Note: Requires Java 1.7
										byte[] bytes = Files.readAllBytes(input.toPath());
										// It would be nice to simply convert to a resource.
										// Unfortunately we have to replace all UUIDs with new ones
										ObjectMapper mapper = new ObjectMapper();
										SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
										if( sd!=null ) {
											logger.infof("%s:ImportDiagramAction imported diagram:\n%s", TAG,sd.getName());
											UUIDResetHandler uuidHandler = new UUIDResetHandler(sd);
											uuidHandler.convertUUIDs();
											sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
											String json = mapper.writeValueAsString(sd);
											logger.debugf("%s:ImportDiagramAction saved resource as:\n%s", TAG,json);
											ProjectResource resource = new ProjectResource(newId,
													BLTProperties.MODULE_ID, BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE,
													sd.getName(), ApplicationScope.GATEWAY, json.getBytes());
											resource.setParentUuid(getFolderId());
											new ResourceCreateManager(resource).run();	
											parentNode.selectChild(new long[] {newId} );
											setDirty(true);
										}
										else {
											ErrorUtil.showWarning(String.format("Failed to deserialize file (%s)",input.getAbsolutePath()),POPUP_TITLE);
										}
									}
									catch( FileNotFoundException fnfe) {
										// Should never happen, we just picked this off a chooser
										ErrorUtil.showWarning(String.format("File %s not found",input.getAbsolutePath()),POPUP_TITLE); 
									}
									catch( IOException ioe) {
										ErrorUtil.showWarning(String.format("IOException (%s)",ioe.getLocalizedMessage()),POPUP_TITLE); 
									}
									catch(Exception ex) {
										ErrorUtil.showError(String.format("Deserialization exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
									}

								}
								else {
									ErrorUtil.showWarning(String.format("Selected file does not exist or is not readable: %s",input.getAbsolutePath()),POPUP_TITLE);
								}
							}  // Cancel
						} 
						catch (Exception ex) {
							ErrorUtil.showError(String.format("Unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
						}
						// No need to inform of success, we'll see the new diagram
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception importing diagram",err);
			}
		}
	}
	
	protected void importDiagram(UUID parentId,SerializableDiagram sd) {
		ObjectMapper mapper = new ObjectMapper();
		try{
			long newId = context.newResourceId();
			String json = mapper.writeValueAsString(sd);
			if(logger.isTraceEnabled() ) logger.trace(json);
			ProjectResource resource = new ProjectResource(newId,
					BLTProperties.MODULE_ID, BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE,
					sd.getName(), ApplicationScope.GATEWAY, json.getBytes());
			resource.setParentUuid(parentId);
			executionEngine.executeOnce(new ResourceCreateManager(resource));	
		} 
		catch (Exception ex) {
			ErrorUtil.showError(String.format("importDiagram: importing diagrm, unhandled Exception (%s)",ex.getMessage()),
					"Import Dialog",ex,true);
		}
	}


}
