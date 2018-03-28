/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *  Based on sample code provided by Inductive Automation.
 */
package com.ils.blt.designer.navtree;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Image;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.UUID;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.client.ClientScriptExtensionManager;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.serializable.ApplicationUUIDResetHandler;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableFolder;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.common.serializable.UUIDResetHandler;
import com.ils.blt.designer.AuxiliaryDataRestoreManager;
import com.ils.blt.designer.AuxiliaryDataSaveManager;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.ResourceCreateManager;
import com.ils.blt.designer.ResourceDeleteManager;
import com.ils.blt.designer.ResourceSaveManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.ThreadCounter;
import com.ils.blt.designer.config.ApplicationConfigurationDialog;
import com.ils.blt.designer.config.FamilyConfigurationDialog;
import com.ils.blt.designer.config.ScriptExtensionsDialog;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.execution.ExecutionManager;
import com.inductiveautomation.ignition.common.execution.impl.BasicExecutionEngine;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectChangeListener;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.FolderNode;
/**
 * A folder in the designer scope to support the diagnostics toolkit diagram
 * layout. In addition to standard folders, folders can be of type "Application" or
 * "Family". These hold properties special to the Diagnostics Toolkit.  Menu options 
 * vary depending on folder type. Labels are likewise dependent.
 * 
 * Leaf nodes are of type DiagramNode.
 */
public class GeneralPurposeTreeNode extends FolderNode implements NavTreeNodeInterface, ProjectChangeListener {
	private static final String TAG = "GeneralPurposeTreeNode";
	private static final int OFFSET = 100;
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final LoggerEx logger = LogUtil.getLogger(getClass().getPackage().getName());
	private boolean dirty = false;
	private DiagramState state = DiagramState.ACTIVE;  // Used for Applications and Families
	private final DeleteNodeAction deleteNodeAction;
	private final StartAction startAction = new StartAction();
	private final StopAction stopAction = new StopAction();
	private final DiagramWorkspace workspace;
	private final NodeStatusManager statusManager;
	private final FolderCreateAction folderCreateAction;
	private TreeSaveAction treeSaveAction = null;
	private final ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
	private final ExecutionManager executionEngine;
	private final ThreadCounter threadCounter = ThreadCounter.getInstance();
	protected final ImageIcon alertBadge;
	private final ImageIcon defaultIcon = IconUtil.getIcon("folder_closed");
	private final ImageIcon openIcon;
	private final ImageIcon closedIcon;

	/** 
	 * Create a new folder node representing the root folder. The root folder does
	 * not worry about cleanliness.
	 * @param ctx the designer context
	 */
	public GeneralPurposeTreeNode(DesignerContext ctx) {
		super(ctx, BLTProperties.MODULE_ID, ApplicationScope.GATEWAY,BLTProperties.ROOT_FOLDER_UUID);
		this.setName(BLTProperties.ROOT_FOLDER_NAME);
		this.resourceId = BLTProperties.ROOT_RESOURCE_ID;
		this.executionEngine = new BasicExecutionEngine(1,TAG);
		deleteNodeAction = null;
		folderCreateAction = new FolderCreateAction(this);
		workspace = ((BLTDesignerHook)ctx.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		setText(BundleUtil.get().getString(PREFIX+".RootFolderName"));
		alertBadge =iconFromPath("Block/icons/badges/bell.png");
		if( context.getProject().isEnabled() ) {
			closedIcon = IconUtil.getIcon("folder_closed");
		}
		else {
			closedIcon = iconFromPath("Block/icons/navtree/disabled_folder.png");  // Project is disabled
		}
		setIcon(closedIcon);
		openIcon = IconUtil.getIcon("folder");
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
	public GeneralPurposeTreeNode(DesignerContext context,ProjectResource resource,UUID self) {
		super(context,resource.getModuleId(),resource.getApplicationScope(),self);
		this.resourceId = resource.getResourceId();
		this.executionEngine = new BasicExecutionEngine(1,TAG);
		setName(resource.getName());      // Also sets text for tree
		
		deleteNodeAction = new DeleteNodeAction(this);
		folderCreateAction = new FolderCreateAction(this);
		workspace = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		alertBadge =iconFromPath("Block/icons/badges/bell.png");
		if(resource.getResourceType().equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			closedIcon = iconFromPath("Block/icons/navtree/application_folder_closed.png");
			openIcon = iconFromPath("Block/icons/navtree/application_folder.png");
		} 
		else if(resource.getResourceType().equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE)) {
			closedIcon = iconFromPath("Block/icons/navtree/family_folder_closed.png");
			openIcon = iconFromPath("Block/icons/navtree/family_folder.png");
		}
		else {
			// Simple folder
			closedIcon = IconUtil.getIcon("folder_closed");
			openIcon = IconUtil.getIcon("folder");
		}
		setIcon(closedIcon);
	}
	@Override
	public boolean confirmDelete(List<? extends AbstractNavTreeNode> selections) {
		// We only care about the first
		boolean result = false;
		if( selections.size()>0 ) {
			AbstractNavTreeNode selected = selections.get(0);
			result = ErrorUtil.showConfirm(String.format(BundleUtil.get().getString(PREFIX+".Delete.Confirmation.Question"), selected.getName()), BundleUtil.get().getString(PREFIX+".Delete.Confirmation.Title"));
		}
		return result;
	}
	@Override
	public Icon getExpandedIcon() { 
		Icon ike  = openIcon;
		if(statusManager.getAlertState(resourceId)) {
			ike = IconUtil.applyBadge(ike, alertBadge);
		}
		return ike;
	}
	
	@Override
	public Icon getIcon() {
		Icon ike  = closedIcon;
		if(statusManager.getAlertState(resourceId)) {
			ike = IconUtil.applyBadge(ike, alertBadge);
		}
		return ike;
	}
	@Override
	public long getResourceId() { return this.resourceId; }
	
	@Override
	public String getWorkspaceName() {
		return DiagramWorkspace.key;
	}
	// Dirtiness refers to internal state, independent of children.
	public boolean isDirty() { return this.dirty; }
	public void setDirty(boolean flag) { this.dirty = flag; }
	// Use the state for Applications and Families to remember whether to 
	// configure production or isolation datbases
	public DiagramState getState() { return this.state; }
	public void setState(DiagramState ds) { this.state = ds; }
	/**
	 * Query the block controller in the Gateway. The resources that it knows
	 * about may, or may not, coincide with those in the Designer. 
	 */
	public void listControllerResources() {
		try {
			List <SerializableResourceDescriptor> descriptors = handler.listResourceNodes();
			for( SerializableResourceDescriptor descriptor : descriptors ) {
				logger.info("Res: "+descriptor.getProjectId()+":"+descriptor.getResourceId()+" "+
						descriptor.getType()+" ("+descriptor.getName()+":"+descriptor.getId()+")");
			}
		} 
		catch (Exception ex) {
			logger.warnf("%s. startAction: ERROR: %s",TAG,ex.getMessage(),ex);
			ErrorUtil.showError(TAG+" Exception listing controller resources",ex);
		}
	}
	/**
	 * Search the project for all resources. This is for debugging.
	 * We filter out those that are global (have no module) as these
	 * are system things that we don't care about for the moment.
	 */
	public void listProjectBLTResources() {
		List <ProjectResource> resources = context.getProject().getResources();
		for( ProjectResource res : resources ) {
			if( res.getModuleId()==null || res.getModuleId().length()==0) continue;
			if( !res.getResourceType().startsWith("blt")) continue;     // List only diagram resources
			logger.info("Res: "+res.getResourceId()+" "+res.getResourceType()+" "+res.getModuleId()+" ("+res.getName()+
					":"+res.getParentUuid()+")");
		}
	}
	/**
	 *  Note: We ignore locking.Previous attempts to use the superior version of this method
	 *        fail to acquire locks (without first doing a project save from the main menu).
	 */
	@Override
	public void onEdit(String newTextValue) {
		// Sanitize name
		if (!NAME_PATTERN.matcher(newTextValue).matches()) {
			ErrorUtil.showError(BundleUtil.get().getString(PREFIX+".InvalidName", newTextValue));
			return;
		}
		String oldName = getProjectResource().getName();
		try {
			logger.infof("%s.onEdit: alterName from %s to %s",TAG,oldName,newTextValue);
			context.structuredRename(resourceId, newTextValue);
			executionEngine.executeOnce(new ResourceUpdateManager(workspace,getProjectResource()));
		}
		catch (IllegalArgumentException ex) {
			ErrorUtil.showError(TAG+".onEdit: "+ex.getMessage());
		}
	}
	
	@Override
	public void onSelected() {
		UndoManager.getInstance().setSelectedContext(GeneralPurposeTreeNode.class);
	}

	// Unsubscribe as a project change listener
	public void prepareForDeletion() { uninstall(); }

	// Note: dirtiness for deletes is taken care of in the delete action
	@Override
	public void projectResourceModified(ProjectResource res, ProjectChangeListener.ResourceModification changeType) {
		// Take care of our special status before invoking the super-class method.
		//logger.infof("%s.projectResourceModified.%s: %s(%d), res %s(%d)",TAG,changeType.name(),getName(),this.resourceId,res.getName(),res.getResourceId());
		if (res.getResourceId() == this.resourceId) {
			if( res.getName()==null || !res.getName().equals(getName()) ) {
				logger.infof("%s.projectResourceModified(%d), setting name %s to %s",TAG,this.resourceId,getName(),res.getName());
				ClientScriptExtensionManager extensionManager = ClientScriptExtensionManager.getInstance();
				// For application or family name changes, we need to synchronize the database
				if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
					try {
						SerializableApplication sa = deserializeApplication(res);
						extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.NODE_RENAME_SCRIPT, 
								sa.getId().toString(),getName(),res.getName());
					}
					catch( Exception ex ) {
						log.errorf("ApplicationConfigurationController.save: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
					}
				}
				else if( res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
					try {
						SerializableFamily sf = deserializeFamily(res);
						extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.NODE_RENAME_SCRIPT, 
								sf.getId().toString(),getName(),res.getName());
					}
					catch( Exception ex ) {
						log.errorf("ApplicationConfigurationController.save: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
					}
				}	
			}
		}  
		super.projectResourceModified(res, changeType);
	}

	@Override
	public void projectUpdated(Project diff) {
		logger.infof("%s.projectUpdated ...",TAG);
		super.projectUpdated(diff);
	}

	/**
	 * Either our state or the state of another node changed, annotate our state. 
	 * Dirtiness for a nav tree node means that one or more of its descendent diagrams
	 * is dirty.
	 * Note: This method should ONLY be called from the node status manager.
	 */
	public void updateUI(boolean dty) {
		logger.debugf("%s.updateUI: %d dirty = %s",TAG,resourceId,(dty?"true":"false"));
		setItalic(dty);
		if( treeSaveAction!=null ) treeSaveAction.setEnabled(dty);
		refresh();  // Update the UI
	}

	/**
	 * Exclude cut and paste which are currently not supported.
	 */
	@Override
	protected void addEditActions(JPopupMenu menu) {
		menu.add(renameAction);
		menu.add(deleteNodeAction);
	}
	/**
	 * Create a child node because we've discovered a resource that matches this instance as a parent
	 * based on its content matching the our UUID. If the node had been previously created, then 
	 * reuse it.
	 */
	@Override
	protected AbstractNavTreeNode createChildNode(ProjectResource res) {
		// If the project is disabled, then don't do anything
		if( !context.getProject().isEnabled()) return null;
		
		logger.debugf("%s.createChildNode: %s(%d) type:%s, depth=%d", TAG,getName(),resourceId,res.getResourceType(),getDepth());
		AbstractResourceNavTreeNode node = statusManager.findNode(res.getResourceId());
		if( node==null ) {
			if (    ProjectResource.FOLDER_RESOURCE_TYPE.equals(res.getResourceType()))       {
				node = new GeneralPurposeTreeNode(context, res, res.getDataAsUUID());
				logger.tracef("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
			}
			else if ( BLTProperties.APPLICATION_RESOURCE_TYPE.equals(res.getResourceType()) )       {
				SerializableApplication sa = deserializeApplication(res);
				node = new GeneralPurposeTreeNode(context, res, sa.getId());
				logger.tracef("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
			}
			else if ( BLTProperties.FAMILY_RESOURCE_TYPE.equals(res.getResourceType()) )       {
				SerializableFamily fa = deserializeFamily(res); 
				node = new GeneralPurposeTreeNode(context, res, fa.getId());
				logger.tracef("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
			}
			else if (BLTProperties.DIAGRAM_RESOURCE_TYPE.equals(res.getResourceType())) {
				node = new DiagramTreeNode(context,res,workspace);
				logger.tracef("%s.createChildDiagram: %s->%s",TAG,this.getName(),node.getName());
			} 
			else {
				logger.warnf("%s: Attempted to create a child of type %s (ignored)",TAG,res.getResourceType());
				throw new IllegalArgumentException();
			}
			statusManager.createResourceStatus(node,resourceId, res.getResourceId());
			// Note: This shouldn't be necessary - plus it causes problems on a delete (when we search for resources to delete)
			//executionEngine.executeOnce(new ResourceUpdateManager(workspace,res));   /// Creates, syncs resource
		}
		else {
			logger.debugf("%s.createChildNode: REUSE %s->%s",TAG,this.getName(),node.getName());
			if( node instanceof DiagramTreeNode ) context.addProjectChangeListener((DiagramTreeNode)node);
		}
		node.install(this);
		if( node.getParent()==null) {
			logger.errorf("%s.createChildNode: ERROR parent is null %s(%d)",TAG,node.getName(),res.getResourceId());
		}
		node.setItalic(context.getProject().isResourceDirty(res.getResourceId()));
		return node;
	}
	// For DiagramNode.delete
	public void recreate() { super.recreate(); }
	
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
			if( context.getProject().isEnabled()) {
				ApplicationCreateAction applicationCreateAction = new ApplicationCreateAction(this);
				ApplicationImportAction applicationImportAction = new ApplicationImportAction(context.getFrame(),this);
				ToolkitConfigureAction configureAction = new ToolkitConfigureAction(menu.getRootPane());
				ClearAction clearAction = new ClearAction();
				DebugAction debugAction = new DebugAction();
				RefreshAction refreshAction = new RefreshAction(this);
				SaveAllAction saveAllAction = new SaveAllAction(this);
				if( handler.isControllerRunning() ) {
					startAction.setEnabled(false);
					clearAction.setEnabled(false);
				}
				else {
					stopAction.setEnabled(false);
					clearAction.setEnabled(true);
				}
				menu.add(applicationCreateAction);
				menu.add(applicationImportAction);
				menu.add(configureAction);
				menu.add(folderCreateAction);
				menu.add(refreshAction);
				menu.add(saveAllAction);
				menu.add(startAction);
				menu.add(stopAction);
				menu.addSeparator();
				menu.add(clearAction);
				menu.add(debugAction);
			}
		}
		else if( getProjectResource()==null ) {
			logger.warnf("%s.initPopupMenu: ERROR: node %s(%d) has no project resource",TAG,this.getName(),resourceId);
		}
		else if(getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			ApplicationExportAction applicationExportAction = new ApplicationExportAction(menu.getRootPane(),this);
			FamilyCreateAction familyAction = new FamilyCreateAction(this);

			ApplicationConfigureAction applicationConfigureAction = new ApplicationConfigureAction(menu.getRootPane(),getProjectResource());
			RestoreAuxiliaryDataAction restoreAuxDataAction = new RestoreAuxiliaryDataAction(this);
			SaveAuxiliaryDataAction saveAuxDataAction = new SaveAuxiliaryDataAction(this);
			treeSaveAction = new TreeSaveAction(this,PREFIX+".SaveApplication");

			menu.add(familyAction);
			menu.add(folderCreateAction);
			SetApplicationStateAction ssaActive = new SetApplicationStateAction(this,DiagramState.ACTIVE);
			SetApplicationStateAction ssaDisable = new SetApplicationStateAction(this,DiagramState.DISABLED);
			SetApplicationStateAction ssaIsolated = new SetApplicationStateAction(this,DiagramState.ISOLATED);
			JMenu setStateMenu = new JMenu(BundleUtil.get().getString(PREFIX+".SetApplicationState"));
			setStateMenu.add(ssaActive);
			setStateMenu.add(ssaDisable);
			setStateMenu.add(ssaIsolated);
			menu.add(setStateMenu);
			menu.addSeparator();
			menu.add(saveAuxDataAction);
			menu.add(restoreAuxDataAction);
			menu.add(applicationConfigureAction);
			menu.add(applicationExportAction);
			menu.add(treeSaveAction);
			addEditActions(menu);
		}
		else if(getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE)) {
			DiagramCreateAction newDiagramAction = new DiagramCreateAction(this);
			FamilyConfigureAction familyConfigureAction = new FamilyConfigureAction(menu.getRootPane(),getProjectResource());
			treeSaveAction = new TreeSaveAction(this,PREFIX+".SaveFamily");


			ImportDiagramAction importAction = new ImportDiagramAction(menu.getRootPane(),this);
			menu.add(newDiagramAction);
			menu.add(importAction);
			menu.add(folderCreateAction);
			menu.addSeparator();
			menu.add(familyConfigureAction);
			menu.add(treeSaveAction);
			addEditActions(menu);

		}
		else if(getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.FOLDER_RESOURCE_TYPE)) {
			if( hasApplication() ) {
				if( hasFamily() ) {
					DiagramCreateAction diagramAction = new DiagramCreateAction(this);
					menu.add(diagramAction);
					ImportDiagramAction importAction = new ImportDiagramAction(menu.getRootPane(),this);
					CloneDiagramAction cloneAction = new CloneDiagramAction(menu.getRootPane(),this);
					menu.add(importAction);
					menu.add(cloneAction);
				}
				else {
					FamilyCreateAction familyAction = new FamilyCreateAction(this);
					menu.add(familyAction);
				}
			}
			// Unaffiliated 
			else {
				DiagramCreateAction diagramAction = new DiagramCreateAction(this);
				menu.add(diagramAction);
				ImportDiagramAction importAction = new ImportDiagramAction(menu.getRootPane(),this);
				CloneDiagramAction cloneAction = new CloneDiagramAction(menu.getRootPane(),this);
				menu.add(importAction);
				menu.add(cloneAction);
			}
			
			menu.add(folderCreateAction);
			treeSaveAction = new TreeSaveAction(this,PREFIX+".SaveFolder");
			menu.add(treeSaveAction);
			menu.addSeparator();
			addEditActions(menu);	
		}
		else {   
			FamilyCreateAction familyAction = new FamilyCreateAction(this);
			menu.add(familyAction);
			menu.addSeparator();
			addEditActions(menu);
		}
	}
	/**
	 * Convert the resource data into a SerializableApplication
	 * @param res
	 * @return
	 */
	private SerializableApplication deserializeApplication(ProjectResource res) {
		SerializableApplication sa = null;
		try{
			byte[] bytes = res.getData();
			ObjectMapper mapper = new ObjectMapper();
			sa = mapper.readValue(new String(bytes), SerializableApplication.class);
			sa.setName(res.getName());   // Sync the SerializableApplication name w/ res
		}
		catch(Exception ex) {
			logger.warnf("%s.deserializeApplication: Deserialization exception (%s)",TAG,ex.getMessage());
		}
		return sa;
	}

	/**
	 * Convert the resource data into a deserializeDiagram.
	 * @param res
	 * @return
	 */
	private SerializableDiagram deserializeDiagram(ProjectResource res) {
		SerializableDiagram sd = null;
		try{
			byte[] bytes = res.getData();
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
			sd.setName(res.getName());   // Sync the SerializableApplication name w/ res
			statusManager.setResourceState(resourceId, sd.getState(),false);
		}
		catch(Exception ex) {
			logger.warnf("%s.SerializableDiagram: Deserialization exception (%s)",TAG,ex.getMessage());
		}
		return sd;
	}

	/**
	 * Convert the resource data into a SerializableFamily
	 * @param res
	 * @return
	 */
	private SerializableFamily deserializeFamily(ProjectResource res) {
		SerializableFamily sf = null;
		try{
			byte[] bytes = res.getData();
			ObjectMapper mapper = new ObjectMapper();
			sf = mapper.readValue(new String(bytes), SerializableFamily.class);
			sf.setName(res.getName());  // ???
		}
		catch(Exception ex) {
			logger.warnf("%s.deserializeFamily: Deserialization exception (%s)",ex.getMessage());
		}
		return sf;
	}
	// Return true if there is a "application" in the ancestral hierarchy of this folder node
	private boolean hasApplication() {
		boolean answer = false;

		AbstractNavTreeNode parentNode = getParent();
		while( parentNode!=null ) {
			if( parentNode instanceof GeneralPurposeTreeNode ) {
				GeneralPurposeTreeNode node = (GeneralPurposeTreeNode)parentNode;
				if( node.getProjectResource()==null ) {
					;  // Folder node
				}
				else if( node.getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
					answer = true;
					break;
				}
			}
			parentNode = parentNode.getParent();
		}
		return answer;
	}
	// Return true if there is a "family" in the ancestral hierarchy of this folder node
	private boolean hasFamily() {
		boolean answer = false;

		AbstractNavTreeNode parentNode = getParent();
		while( parentNode!=null ) {
			if( parentNode instanceof GeneralPurposeTreeNode ) {
				GeneralPurposeTreeNode node = (GeneralPurposeTreeNode)parentNode;
				if( node.getProjectResource()==null ) {
					;  // Folder node
				}
				else if( node.getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE)) {
					answer = true;
					break;
				}
				else if( node.getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
					break;  // false
				}
			}
			parentNode = parentNode.getParent();
		}
		return answer;
	}
	/**
	 * Create an ImageIcon from the resource path. If it doesn't exist, return the default.
	 * @param path
	 * @return
	 */
	private ImageIcon iconFromPath(String path) {
		Dimension iconSize = new Dimension(20,20);
		ImageIcon result = defaultIcon;
		Image img = ImageLoader.getInstance().loadImage(path,iconSize);
		if( img!=null ) result = new ImageIcon(img);
		return result;
	}
	private boolean isRootFolder() {
		return getFolderId().equals(BLTProperties.ROOT_FOLDER_UUID);
	}
/*
 *   WARNING: When an application is serialized, all resources are nested and serialized as such.
 *            A project is saved differently. All resources are saved separately in a flat namespace.
 */            
	// Recursively descend the node tree, gathering up associated resources.
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to an application.
	private SerializableApplication recursivelyDeserializeApplication(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		SerializableApplication sa = null;
		if( res!=null ) {
			logger.infof("%s.recursivelyDeserializeApplication: %s (%d)",TAG,res.getName(),res.getResourceId());
			sa = deserializeApplication(res);
			sa.setFamilies(new SerializableFamily[0]);
			sa.setFolders(new SerializableFolder[0]);

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				ProjectResource cres = child.getProjectResource();
				if(cres==null) continue;
				if(cres.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)){
					SerializableFamily sfam = recursivelyDeserializeFamily(child);
					if( sfam!=null ) sa.addFamily(sfam);
				}
				else if(cres.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
					SerializableFolder sf = recursivelyDeserializeFolder(child);
					if( sf!=null ) sa.addFolder(sf);
				}
				else {
					logger.infof("%s.recursivelyDeserializeApplication: %s unexpected child resource type (%s)",TAG,res.getName(),cres.getName(),cres.getResourceType());
				}
			}
		}
		return sa;
	}
	// This nodes of the tree is associated with a diagram. It's only other possible children
	// are other diagrams which are children of encapsulation blocks. At present these are not handled
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to a diagram.
	private SerializableDiagram recursivelyDeserializeDiagram(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		SerializableDiagram sdiag = null;
		if( res!=null ) {
			logger.infof("%s.recursivelyDeserializeDiagram: %s (%d)",TAG,res.getName(),res.getResourceId());
			sdiag = deserializeDiagram(res);
		}
		return sdiag;
	}
	// Recursively descend the node tree, gathering up associated resources.
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to an application.
	private SerializableFamily recursivelyDeserializeFamily(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		SerializableFamily sfam = null;
		if( res!=null ) {
			logger.infof("%s.recursivelyDeserializeFamily: %s (%d)",TAG,res.getName(),res.getResourceId());
			sfam = deserializeFamily(res);
			sfam.setFolders(new SerializableFolder[0]);
			sfam.setDiagrams(new SerializableDiagram[0]);

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				ProjectResource cres = child.getProjectResource();
				if(cres==null) continue;
				if( cres.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
					SerializableDiagram sd = recursivelyDeserializeDiagram(child);
					if( sd!=null ) sfam.addDiagram(sd);
				}
				else if(cres.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
					SerializableFolder sf = recursivelyDeserializeFolder(child);
					if( sf!=null ) sfam.addFolder(sf);
				}
				else {
					logger.infof("%s.recursivelyDeserializeFamily: %s unexpected child resource type (%s)",TAG,res.getName(),cres.getName(),cres.getResourceType());
				}
			}
		}
		return sfam;
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
			sfold.setFolders(new SerializableFolder[0]);
			sfold.setDiagrams(new SerializableDiagram[0]);

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				ProjectResource cres = child.getProjectResource();
				if(cres==null) continue;
				if( cres.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
					SerializableDiagram sd = recursivelyDeserializeDiagram(child);
					if( sd!=null ) sfold.addDiagram(sd);
				}
				else if(cres.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)){
					SerializableFamily sfam = recursivelyDeserializeFamily(child);
					if( sfam!=null ) sfold.addFamily(sfam);
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

	/**
	 *  Serialize an Application into JSON.
	 * @param application to be serialized
	 */ 
	private String serializeApplication(SerializableApplication application) {
		String json = "";
		ObjectMapper mapper = new ObjectMapper();
		logger.debugf("%s: serializeApplication creating json ... %s",TAG,(mapper.canSerialize(SerializableApplication.class)?"true":"false"));
		try{ 
			json = mapper.writeValueAsString(application);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize application (%s)",TAG,jpe.getMessage());
		}
		logger.debugf("%s: serializeApplication created json ... %s",TAG,json);
		return json;
	}


	// ============================================== Project Change Listener =======================================

	/**
	 *  Serialize a diagram into JSON. 
	 * @param diagram to be serialized
	 */ 
	private String serializeDiagram(SerializableDiagram diagram) {
		String json = "";
		ObjectMapper mapper = new ObjectMapper();
		logger.debugf("%s: serializeDiagram creating json ... %s",TAG,(mapper.canSerialize(SerializableDiagram.class)?"true":"false"));
		try{ 
			json = mapper.writeValueAsString(diagram);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize diagram (%s)",TAG,jpe.getMessage());
		}
		logger.infof("%s: serializeDiagram created json ... %s",TAG,json);
		return json;
	}

	/**
	 *  Serialize a Family into JSON.
	 * @param family to be serialized
	 */ 
	private String serializeFamily(SerializableFamily family) {
		String json = "";
		ObjectMapper mapper = new ObjectMapper();
		logger.debugf("%s: serializeFamily creating json ... %s",TAG,(mapper.canSerialize(SerializableFamily.class)?"true":"false"));
		try{ 
			json = mapper.writeValueAsString(family);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize family (%s)",TAG,jpe.getMessage());
		}
		logger.debugf("%s: serializeFamily created json ... %s",TAG,json);
		return json;
	}

	// ============================================= private action classes ===========================================================
	// Launch a dialog that configures application attributes.
	private class ApplicationConfigureAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final static String POPUP_TITLE = "Configure Application";
		private final Component anchor;
		private final ProjectResource res;

		public ApplicationConfigureAction(Component c,ProjectResource resource)  {
			super(PREFIX+".ConfigureApplication",IconUtil.getIcon("gear"));  // preferences
			anchor = c;
			res = resource;
		}

		public void actionPerformed(ActionEvent e) {
			if( resourceId<0 ) return;   // Do nothing
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						// Unmarshall the resource
						SerializableApplication sa = deserializeApplication(res);
						if( sa!=null ) {
							ApplicationConfigurationDialog dialog = new ApplicationConfigurationDialog(context.getFrame(),context,sa,
																									  GeneralPurposeTreeNode.this);
							dialog.setLocationRelativeTo(anchor);
							Point p = dialog.getLocation();
	    					dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
							dialog.pack();
							dialog.setVisible(true);   // Returns when dialog is closed
							if( !dialog.isCancelled() ) {
								sa = dialog.getApplication();
								String json = serializeApplication(sa);
								res.setName(sa.getName());
								res.setData(json.getBytes());
								new ResourceUpdateManager(workspace,res).run();
							}
						}
						else {
							ErrorUtil.showWarning(String.format("ApplicationConfigurationAction: Failed to deserialize resource"),POPUP_TITLE);
						}


					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception configuring application",err);
			}
		}
	}
	// From the root node, create a folder for diagrams belonging to a application
	private class ApplicationCreateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode currentNode;
		public ApplicationCreateAction(AbstractResourceNavTreeNode parent)  {
			super(PREFIX+".NewApplication",IconUtil.getIcon("folder_new"));
			this.currentNode = parent;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				final long newResId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".NewApplication.Default.Name");
				if( newName==null) newName = "New App";  // Missing Resource
				SerializableApplication app = new SerializableApplication();
				app.setName(newName);
				String json = serializeApplication(app);

				logger.debugf("%s.ApplicationCreateAction. json=%s",TAG,json);
				byte[] bytes = json.getBytes();
				logger.infof("%s.ApplicationCreateAction. create new %s(%d), %s (%d bytes)",TAG,BLTProperties.APPLICATION_RESOURCE_TYPE,newResId,
						newName,bytes.length);
				ProjectResource resource = new ProjectResource(newResId,BLTProperties.MODULE_ID, BLTProperties.APPLICATION_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				logger.infof("%s: parent: %s",TAG,getFolderId().toString());
				new ResourceCreateManager(resource).run();	// Must be synchronous for child to show
				currentNode.selectChild(new long[] {newResId} );
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception creating application",err);
			}
		}
	}
	private class ApplicationExportAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final static String POPUP_TITLE = "Export Application Tree";
		private final GeneralPurposeTreeNode node;
		private final Component anchor;
		public ApplicationExportAction(Component c,GeneralPurposeTreeNode gptn)  {
			super(PREFIX+".ApplicationExport",IconUtil.getIcon("export1")); 
			node=gptn;
			anchor=c;
		}

		public void actionPerformed(ActionEvent e) {

			if( node==null ) return;   // Do nothing
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						ExportDialog dialog = new ExportDialog(context.getFrame());
						dialog.setLocationRelativeTo(anchor);
						Point p = dialog.getLocation();
    					dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
						dialog.pack();
						dialog.setVisible(true);   // Returns when dialog is closed
						File output = dialog.getFilePath();
						boolean success = false;
						if( output!=null ) {
							logger.debugf("%s.actionPerformed: dialog returned %s",TAG,output.getAbsolutePath());
							try {
								if(output.exists()) {
									//output.delete();           // Remove existing file
									//output.createNewFile();
									output.setWritable(true);  // This doesn't seem to work (??)
								}
								else {
									output.createNewFile();
								}

								if( output.canWrite() ) {
									ObjectMapper mapper = new ObjectMapper();
									if(logger.isDebugEnabled()) logger.debugf("%s.actionPerformed: creating json ... %s",TAG,(mapper.canSerialize(SerializableApplication.class)?"true":"false"));
									try{ 
										// Convert the view into a serializable object. Here we reject any nesting that might
										// have been saved in the project resource, and substitute what we know from the nav tree.
										SerializableApplication sap = recursivelyDeserializeApplication(node);
										String json = mapper.writeValueAsString(sap);
										FileWriter fw = new FileWriter(output,false);  // Do not append
										try {
											fw.write(json);
											success = true;
										}
										catch(IOException ioe) {
											ErrorUtil.showWarning(String.format("Error writing file %s (%s)",output.getAbsolutePath(),
													ioe.getMessage()),POPUP_TITLE,false);
										}
										finally {
											fw.close();
										}
									}
									catch(JsonProcessingException jpe) {
										ErrorUtil.showError("Unable to serialize diagram",POPUP_TITLE,jpe,true);
									}
								}
								else {
									ErrorUtil.showWarning(String.format("selected file (%s) is not writable.",output.getAbsolutePath()),POPUP_TITLE,false);
								}
							}
							catch (IOException ioe) {
								ErrorUtil.showWarning(String.format("Error creating or closing file %s (%s)",output.getAbsolutePath(),
										ioe.getMessage()),POPUP_TITLE,false);
							}
						}
						// If there's an error, then the user will be informed
						if( success ) ErrorUtil.showInfo(anchor, "Export complete", POPUP_TITLE);
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception exporting application",err);
			}
		}
	}
	private class ApplicationImportAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final static String POPUP_TITLE = "Import Application";
		private final Component anchor;
		private final AbstractResourceNavTreeNode root;
		public ApplicationImportAction(Component c,AbstractResourceNavTreeNode parent)  {
			super(PREFIX+".ImportApplication",IconUtil.getIcon("import1"));  // preferences
			this.anchor = c;
			this.root = parent;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						try {
							long newId = context.newResourceId();
							String title = BundleUtil.get().getString(PREFIX+".Import.Application.DialogTitle");
							String label = BundleUtil.get().getString(PREFIX+".Import.Application.NameLabel");
							ImportDialog dialog = new ImportDialog(context.getFrame(),label,title);
							dialog.setLocationRelativeTo(anchor);
							Point p = dialog.getLocation();
	    					dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
							dialog.pack();
							root.setBold(true);
							root.reload();      // Closes applications
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
										SerializableApplication sa = mapper.readValue(new String(bytes), SerializableApplication.class);
										if( sa!=null ) {
											ApplicationUUIDResetHandler uuidHandler = new ApplicationUUIDResetHandler(sa);
											uuidHandler.convertUUIDs();
											logger.infof("%s:ApplicationImportAction importing application %s(%d) (%s)", TAG,sa.getName(),newId,sa.getId().toString());
											String json = mapper.writeValueAsString(sa);
											if(logger.isTraceEnabled() ) logger.trace(json);
											ProjectResource resource = new ProjectResource(newId,
													BLTProperties.MODULE_ID, BLTProperties.APPLICATION_RESOURCE_TYPE,
													sa.getName(), ApplicationScope.GATEWAY, json.getBytes());
											resource.setParentUuid(getFolderId());
											// Now import families
											for(SerializableFamily fam:sa.getFamilies()) {
												importFamily(sa.getId(),fam);
											}
											// Create after the children -- else sometimes folders are not populated.
											new ResourceCreateManager(resource).run();   // In-line
											root.selectChild(new long[] {newId} );
										}
										else {
											ErrorUtil.showWarning(String.format("ApplicationImportAction: Failed to deserialize file (%s)",input.getAbsolutePath()),POPUP_TITLE);
										}
									}
									catch( FileNotFoundException fnfe) {
										// Should never happen, we just picked this off a chooser
										ErrorUtil.showWarning(String.format("ApplicationImportAction: File %s not found",input.getAbsolutePath()),POPUP_TITLE); 
									}
									catch( IOException ioe) {
										ErrorUtil.showWarning(String.format("ApplicationImportAction: IOException (%s)",ioe.getLocalizedMessage()),POPUP_TITLE); 
									}
									catch(Exception ex) {
										ErrorUtil.showError(String.format("ApplicationImportAction: Deserialization exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
									}
								}
								else {
									ErrorUtil.showWarning(String.format("ApplicationImportAction: Selected file does not exist or is not readable: %s",input.getAbsolutePath()),POPUP_TITLE);
								}
								
							}  
							// No thread - we should just be done at this point
							SwingUtilities.invokeLater(new Runnable() {
								public void run() {
									root.setBold(false);
									@SuppressWarnings("unchecked")
									Enumeration<AbstractNavTreeNode> kids = root.children(); // Applications
									while( kids.hasMoreElements() ) {
										AbstractNavTreeNode kid = kids.nextElement();
										kid.reload();
										kid.refresh();
										logger.infof("%s:ApplicationImportAction reloading (%s)", TAG,kid.getName());
									}
									root.refresh();
								}
							});
						} 
						catch (Exception ex) {
							ErrorUtil.showError(String.format("ApplicationImportAction: Unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
						}
					}
		
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception importing application",err);
			}
		}
		// Run in foreground to avoid synchronization issues with display.
		private synchronized void importDiagram(UUID parentId,SerializableDiagram sd) {
			ObjectMapper mapper = new ObjectMapper();
			try{
				sd.setState(DiagramState.DISABLED);
				sd.setDirty(true);
				long newId = context.newResourceId();
				String json = mapper.writeValueAsString(sd);
				if(logger.isTraceEnabled() ) logger.trace(json);
				ProjectResource resource = new ProjectResource(newId,
						BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
						sd.getName(), ApplicationScope.GATEWAY, json.getBytes());
				resource.setParentUuid(parentId);
				logger.infof("%s:ApplicationImportAction importing diagram %s(%d) (%s)", TAG,sd.getName(),newId,sd.getId().toString());
				statusManager.setResourceState(newId, sd.getState(),false);
				new ResourceCreateManager(resource).run();
			} 
			catch (Exception ex) {
				ErrorUtil.showError(String.format("ApplicationImportAction: importing diagrm, unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
			}
		}
		// Run in foreground to avoid synchronization issues with display.
		private synchronized void importFamily(UUID parentId,SerializableFamily sf) {
			ObjectMapper mapper = new ObjectMapper();
			try{
				long newId = context.newResourceId();
				String json = mapper.writeValueAsString(sf);
				if(logger.isTraceEnabled() ) logger.trace(json);
				ProjectResource resource = new ProjectResource(newId,
						BLTProperties.MODULE_ID, BLTProperties.FAMILY_RESOURCE_TYPE,
						sf.getName(), ApplicationScope.GATEWAY, json.getBytes());
				resource.setParentUuid(parentId);
				logger.infof("%s:ApplicationImportAction importing family %s(%s) (%s/%s)", TAG,sf.getName(),newId,parentId.toString(),sf.getId().toString());
				new ResourceCreateManager(resource).run();   // in-line
				// Now import the diagrams
				for(SerializableDiagram diagram:sf.getDiagrams()) {
					importDiagram(sf.getId(),diagram);
				}
			} 
			catch (Exception ex) {
				ErrorUtil.showError(String.format("ApplicationImportAction: Unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
			}
		}
	}
	// From the root node, tell the gateway controller to clear all resources
	private class ClearAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public ClearAction()  {
			super(PREFIX+".Clear",IconUtil.getIcon("delete_all"));
		}

		public void actionPerformed(ActionEvent e) {
			handler.clearController();
		}
	}
	private class CloneDiagramAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode parentNode;
		private final Component anchor;
		public CloneDiagramAction(Component c,AbstractResourceNavTreeNode pNode)  {
			super(PREFIX+".CloneDiagram",IconUtil.getIcon("copy"));  // preferences
			this.anchor = c;
			this.parentNode = pNode;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						long newId;

						try {	
							newId = context.newResourceId();

							workspace.open(newId);
							String title = BundleUtil.get().getString(PREFIX+".Import.Diagram.DialogTitle");
							String label = BundleUtil.get().getString(PREFIX+".Import.Diagram.NameLabel");
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
										ProjectResource resource = new ProjectResource(newId,
												BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
												"CLONE", ApplicationScope.GATEWAY, bytes);
										resource.setParentUuid(getFolderId());
										new ResourceCreateManager(resource).run();	
										parentNode.selectChild(new long[] {newId} );

									}
									catch( FileNotFoundException fnfe) {
										// Should never happen, we just picked this off a chooser
										logger.warnf("%s: actionPerformed, File not found %s (%s)",TAG,input.getAbsolutePath(),fnfe.getLocalizedMessage()); 
									}
									catch( IOException ioe) {
										// Should never happen, we just picked this off a chooser
										logger.warnf("%s: actionPerformed, IOException %s (%s)",TAG,input.getAbsolutePath(),ioe.getLocalizedMessage()); 
									}

								}
								else {
									logger.warnf("%s: actionPerformed, selected file does not exist of is not readable: %s",TAG,input.getAbsolutePath());
								}
							}  // Cancel
						} 
						catch (Exception ex) {
							logger.errorf("%s: actionPerformed: Unhandled Exception (%s)",TAG,ex.getMessage());
						}
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception cloning diagram",err);
			}
		}
	} 
	// From the root node, recursively log the contents of the tree
	private class DebugAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public DebugAction()  {
			super(PREFIX+".Debug",IconUtil.getIcon("bug_yellow"));
		}

		public void actionPerformed(ActionEvent e) {
			logger.info("============================ BLT Resources (Designer) =========================");
			listProjectBLTResources();
			logger.info("============================ BLT Resources (Gateway)  =========================");
			listControllerResources();
			logger.infof("================================ (proj = %d )==============================",context.getProject().getId());
		}
	}
	// Delete the this node and all its descendants. 
	// Note: On a "save" action, the descendants are removed also.
	private class DeleteNodeAction extends BaseAction implements UndoManager.UndoAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode node;
		ResourceDeleteManager deleter;
		private String bundleString;
		private long resid = -1;    // for the root

		public DeleteNodeAction(AbstractResourceNavTreeNode resourceNode)  {
			super(PREFIX+".DeleteNode",IconUtil.getIcon("delete"));
			this.node = resourceNode;
			this.bundleString = PREFIX+".NodeNoun";
			this.deleter = new ResourceDeleteManager(node);
		}

		public void actionPerformed(ActionEvent e) {
			AbstractNavTreeNode p = node.getParent();
			ProjectResource res = node.getProjectResource();
			resid = res.getResourceId();
			logger.infof("%s.DeleteNodeAction: %s, resource %d.",TAG,node.getName(),resid);
			List<AbstractResourceNavTreeNode>selected = new ArrayList<>();
			selected.add(node);
			if(confirmDelete(selected)) {
				if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
					bundleString = PREFIX+".ApplicationNoun";
				}
				else if( res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
					bundleString = PREFIX+".FamilyNoun";
				}
				else if( res.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE) ) {
					bundleString = PREFIX+".FolderNoun";
				}
				deleter.acquireResourcesToDelete();
				if( execute() ) {
					UndoManager.getInstance().add(this,GeneralPurposeTreeNode.class);
					
					if( p instanceof GeneralPurposeTreeNode )  {
						GeneralPurposeTreeNode parentNode = (GeneralPurposeTreeNode)p;
						parentNode.recreate();
						parentNode.expand();
					}
					deleter.deleteInProject();
				}
				else {
					ErrorUtil.showError("Node locked, delete failed");
				}
			}
		}

		// Marks the project resources for deletion.
		@Override
		public boolean execute() {
			return deleter.deleteResources();
		}

		@Override
		public boolean isGroupSequenceIndependent() {return false;}

		@Override
		public boolean undo() {
			return deleter.undo();
		}

		@Override
		public String getDescription() { return BundleUtil.get().getStringLenient(bundleString); }

	}

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
				diagram.setState(DiagramState.DISABLED);

				String json = serializeDiagram(diagram);	
				logger.debugf("%s.DiagramCreateAction. json=%s",TAG,json);
				byte[] bytes = json.getBytes();
				logger.infof("%s.DiagramCreateAction. create new %s(%d), %s (%d bytes)",TAG,BLTProperties.DIAGRAM_RESOURCE_TYPE,newId,
						newName,bytes.length);
				ProjectResource resource = new ProjectResource(newId,BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
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
	// This really ought to launch a dialog that configures family attributes.
	private class FamilyConfigureAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final static String POPUP_TITLE = "Configure Family";
		private final Component anchor;
		private ProjectResource res;
		public FamilyConfigureAction(Component c,ProjectResource resource)  {
			super(PREFIX+".ConfigureFamily",IconUtil.getIcon("gear"));  // preferences
			this.anchor = c;
			this.res = resource;
		}

		public void actionPerformed(ActionEvent e) {

			if( resourceId<0 ) return;   // Do nothing
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						// Unmarshall the resource
						logger.infof("%s.actionPerformed: deserializing ...%d",TAG,resourceId);
						SerializableFamily sf = deserializeFamily(res);
						if( sf!=null ) {
							FamilyConfigurationDialog dialog = new FamilyConfigurationDialog(context.getFrame(),context,sf,GeneralPurposeTreeNode.this);
							dialog.setLocationRelativeTo(anchor);
							Point p = dialog.getLocation();
	    					dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
							dialog.pack();
							dialog.setVisible(true);   // Returns when dialog is closed
							if( !dialog.isCancelled()) {
								sf = dialog.getFamily();
								String json = serializeFamily(sf);
								res.setName(sf.getName());
								res.setData(json.getBytes());
								new ResourceUpdateManager(workspace,res).run();	
							}
						}
						else {
							ErrorUtil.showWarning(String.format("FamilyConfigurationAction: Failed to deserialize resource"),POPUP_TITLE);
						}
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception configuring family",err);
			}
		}
	}
	// From the root node, create a folder for diagrams belonging to a family
	private class FamilyCreateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode currentNode;
		public FamilyCreateAction(AbstractResourceNavTreeNode parentNode)  {
			super(PREFIX+".NewFamily",IconUtil.getIcon("folder_new"));
			this.currentNode = parentNode;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".NewFamily.Default.Name");
				if( newName==null) newName = "New Folks";  // Missing Resource
				SerializableFamily fam = new SerializableFamily();
				fam.setName(newName);

				String json = serializeFamily(fam);

				logger.debugf("%s.FamilyCreateAction. json=%s",TAG,json);
				byte[] bytes = json.getBytes();
				logger.infof("%s.FamilyCreateAction. create new %s(%d), %s (%d bytes)",TAG,BLTProperties.FAMILY_RESOURCE_TYPE,newId,
						newName,bytes.length);
				ProjectResource resource = new ProjectResource(newId,BLTProperties.MODULE_ID, BLTProperties.FAMILY_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				logger.infof("%s.familyCreateAction: res(%d) %s, fam %s, parent: %s",TAG,newId,resource.getName(),fam.getName(),getFolderId().toString());
				new ResourceCreateManager(resource).run();	
				//recreate();
				currentNode.selectChild(new long[] {newId} );
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception creating family",err);
			}
		}
	}
	// From the root node, create a folder for diagrams belonging to a family
	private class FolderCreateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode currentNode;
		public FolderCreateAction(AbstractResourceNavTreeNode parentNode)  {
			super(PREFIX+".NewFolder",IconUtil.getIcon("folder_new"));
			currentNode = parentNode;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				final long newResId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".NewFolder.Default.Name");
				if( newName==null) newName = "New Folks";  // Missing Resource
				UUID newId = context.addFolder(newResId, BLTProperties.MODULE_ID, ApplicationScope.GATEWAY, newName, getFolderId());
				logger.infof("%s.FolderCreateAction. create new %s(%d), %s (%s, parent %s)",TAG,BLTProperties.FOLDER_RESOURCE_TYPE,newResId,newName,
						newId.toString(),getFolderId().toString());
				//recreate();
				currentNode.selectChild(new long[] {newResId} );
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception creating folder",err);
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
										mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
										SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
										if( sd!=null ) {
											logger.infof("%s:ImportDiagramAction imported diagram:\n%s", TAG,sd.getName());
											UUIDResetHandler uuidHandler = new UUIDResetHandler(sd);
											uuidHandler.convertUUIDs();
											sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
											sd.setState(DiagramState.DISABLED);
											String json = mapper.writeValueAsString(sd);
											logger.debugf("%s:ImportDiagramAction saved resource as:\n%s", TAG,json);
											ProjectResource resource = new ProjectResource(newId,
													BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
													sd.getName(), ApplicationScope.GATEWAY, json.getBytes());
											resource.setParentUuid(getFolderId());
											new ResourceCreateManager(resource).run();	
											parentNode.selectChild(new long[] {newId} );
											statusManager.setResourceState(newId, sd.getState(),false);
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
	// Re-compute the nav tree.
	private class RefreshAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode node;

		public RefreshAction(AbstractResourceNavTreeNode treeNode)  {
			super(PREFIX+".Refresh",IconUtil.getIcon("refresh")); 
			this.node = treeNode;
		}

		public void actionPerformed(ActionEvent e) {
			// 
			if( !isRootFolder() ) return;
			node.setBold(true);
			threadCounter.reset();
			statusManager.updateAll();
			node.reload();
			ThreadCompletionDetector detector = new ThreadCompletionDetector(node);
			new Thread(detector).start();
		}
	}
	// Launch a dialog that recursively saves auxiliary data from the application
	// into the current database.
	private class RestoreAuxiliaryDataAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final GeneralPurposeTreeNode node;
		public RestoreAuxiliaryDataAction(GeneralPurposeTreeNode treeNode)  {
			super(PREFIX+".RestoreAuxiliaryData",IconUtil.getIcon("data_replace"));  // preferences
			node = treeNode;
		}

		public void actionPerformed(ActionEvent e) {
			// Traverse the hierarchy of the application, saving auxiliary data at each step
			executionEngine.executeOnce(new AuxiliaryDataRestoreManager(workspace,node));
		}
	}
	// Save the entire Application hierarchy.
	private class SaveAllAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode node;

		public SaveAllAction(AbstractResourceNavTreeNode treeNode)  {
			super(PREFIX+".SaveAll",IconUtil.getIcon("add2")); 
			this.node = treeNode;
		}

		public void actionPerformed(ActionEvent e) {
			// Traverse the entire hierarchy, saving each step
			if( !isRootFolder() ) return;
			node.setBold(true);
			threadCounter.reset();
			statusManager.updateAll();
			executionEngine.executeOnce(new ResourceSaveManager(workspace,node));
			ThreadCompletionDetector detector = new ThreadCompletionDetector(node);
			new Thread(detector).start();
		}
	}
	
	// Recursively save auxiliary data from the application and its descendants
	// into the current database.
	private class SaveAuxiliaryDataAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final GeneralPurposeTreeNode node;

		public SaveAuxiliaryDataAction(GeneralPurposeTreeNode treeNode)  {
			super(PREFIX+".SaveAuxiliaryData",IconUtil.getIcon("data_out"));  // preferences
			node = treeNode;
		}

		public void actionPerformed(ActionEvent e) {
			// Traverse the hierarchy of the application, saving auxiliary data at each step
			executionEngine.executeOnce(new AuxiliaryDataSaveManager(node));
		}
	}
	/**
	 * Recursively set the state of every diagram under the application to the selected value.
	 * If the selected value is ISOLATED, then we also update the external database from
	 * the project resources. If there is a state change, we save the diagram to keep
	 * project resource/gateway/designer all in sync.
	 */
	private class SetApplicationStateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final GeneralPurposeTreeNode app;
		private final DiagramState treeState;
		public SetApplicationStateAction(GeneralPurposeTreeNode applicationNode,DiagramState s)  {
			super(PREFIX+".SetApplicationState."+s.name());
			this.app = applicationNode;
			this.treeState = s;
		}

		// We need to set the state both locally and in the gateway
		public void actionPerformed(ActionEvent e) {
			// We don't know which state we're coming from for the various diagrams.
			// Save off the aux data before we change state.
			executionEngine.executeOnce(new AuxiliaryDataSaveManager(app));
			// Tell the gateway to set the state of all diagrams under the application
			handler.setApplicationState(app.getName(), treeState.name());
			
			// Set the nodes in the navtree.
			recursivelyUpdateNodeState(app,treeState);
		}
		// Set the state locally to match and save changes to disk. It's redundant to inform the Gateway.
		public void recursivelyUpdateNodeState(AbstractNavTreeNode node,DiagramState diagramState) {
			if( node==null) return;
			if( node instanceof DiagramTreeNode ) {
				DiagramTreeNode dtn = (DiagramTreeNode)node;
				dtn.setDiagramState(diagramState);
			}
			@SuppressWarnings("unchecked")
			Enumeration<AbstractNavTreeNode>  childWalker = node.children();
			while(childWalker.hasMoreElements()) {
				recursivelyUpdateNodeState(childWalker.nextElement(),diagramState);
			}
		}
	}

	// Start refers to a global startup of the Execution controller in the Gateway
	private class StartAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public StartAction()  {
			super(PREFIX+".StartExecution",IconUtil.getIcon("disk_play"));  // preferences
		}

		public void actionPerformed(ActionEvent e) {
			try{
				handler.startController();
				this.setEnabled(false);
				stopAction.setEnabled(true);
			} 
			catch (Exception ex) {
				logger.warnf("%s: startAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(TAG+" Exception starting the controller",ex);
			}
		}
	}

	private class StopAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public StopAction()  {
			super(PREFIX+".StopExecution",IconUtil.getIcon("disk_forbidden"));  // preferences
		}
		public void actionPerformed(ActionEvent e) {
			try {
				handler.stopController();
				this.setEnabled(false);
				startAction.setEnabled(true);
			}
			catch(Exception ex) {
				logger.warnf("%s: stopAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(TAG+" Exception stopping the controller",ex);
			}
		}
	}
	// Launch a dialog to configure toolkit-wide attributes.
	private class ToolkitConfigureAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final Component anchor;

		public ToolkitConfigureAction(Component c)  {
			super(PREFIX+".ConfigureToolkit",IconUtil.getIcon("gear"));  // preferences
			anchor = c;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						ScriptExtensionsDialog dialog = new ScriptExtensionsDialog(context.getFrame(),context);
						dialog.setLocationRelativeTo(anchor);
						Point p = dialog.getLocation();
						dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
						dialog.pack();
						dialog.setVisible(true);   // Returns when dialog is closed
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception configuring toolkit",err);
			}
		}
	}

	// Wait until the background threads are quiescent
	private class ThreadCompletionDetector implements Runnable {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode node;

		public ThreadCompletionDetector(AbstractResourceNavTreeNode treeNode)  {
			node = treeNode;
		}

		public void run() {
			// Wait for all the threads to complete
			for(;;) {
				try {
					Thread.sleep(250);
					if( threadCounter.isQuiescent()) break;
				}
				catch(InterruptedException ie) {
					break;
				}
			}
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					node.setBold(false);
				}
			});
		}
	}

	// Save this node and all its descendants.
	private class TreeSaveAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final GeneralPurposeTreeNode node;

		public TreeSaveAction(GeneralPurposeTreeNode treeNode, String bundleString)  {
			super(bundleString,IconUtil.getIcon("add2")); 
			node = treeNode;
		}

		public void actionPerformed(ActionEvent e) {
			executionEngine.executeOnce(new ResourceSaveManager(workspace,node));
		}
	}
}
