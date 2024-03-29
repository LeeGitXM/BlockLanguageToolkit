/**
 *   (c) 2013-2018  ILS Automation. All rights reserved.
 *  
 *  Based on sample code provided by Inductive Automation.
 */
package com.ils.blt.designer.navtree;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Image;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Random;
import java.util.UUID;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.script.CommonScriptExtensionManager;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.serializable.ApplicationUUIDResetHandler;
import com.ils.blt.common.serializable.FamilyUUIDResetHandler;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlock;
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
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.GeneralPurposeDataContainer;
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
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
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
	private static final String CLSS = "GeneralPurposeTreeNode";
	public static final String BLT_CUT_OPERATION = "BLTCUT";
	public static final String BLT_COPY_OPERATION = "BLTCOPY";
	private static final int OFFSET = 100;
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final static Random random = new Random();
	private final LoggerEx logger = LogUtil.getLogger(getClass().getPackage().getName());
	private boolean dirty = false;
	private DiagramState state = DiagramState.ACTIVE;  // Used for Applications and Families
//	private final DuplicateNodeAction duplicateAction;
	private final DeleteNodeAction deleteNodeAction;
//	private final CutAction cutBranchAction;
	private final CopyAction copyBranchAction;
	private final PasteAction pasteBranchAction;
	private final StartAction startAction = new StartAction();
	private final StopAction stopAction = new StopAction();
	private final DiagramWorkspace workspace;
	private final NodeStatusManager statusManager;
	private final FolderCreateAction folderCreateAction;
//	private final FamilyFolderCreateAction familyFolderCreateAction;
	private TreeSaveAction treeSaveAction = null;
	private final ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
	private final ExecutionManager executionEngine;
	private final ThreadCounter threadCounter = ThreadCounter.getInstance();
	protected final ImageIcon alertBadge;
	private final ImageIcon defaultIcon = IconUtil.getIcon("folder_closed");
	private final ImageIcon openIcon;
	private final ImageIcon closedIcon;
	private final ImageIcon familyIcon;
	private final ImageIcon diagramIcon;
	private HashMap<UUID,UUID> bigLookup;
	private ApplicationRequestHandler requestHandler;


	/** 
	 * Create a new folder node representing the root folder. The root folder does
	 * not worry about cleanliness.
	 * @param ctx the designer context
	 */
	public GeneralPurposeTreeNode(DesignerContext ctx) {
		super(ctx, BLTProperties.MODULE_ID, ApplicationScope.GATEWAY,BLTProperties.ROOT_FOLDER_UUID);
		this.setName(BLTProperties.ROOT_FOLDER_NAME);
		this.resourceId = BLTProperties.ROOT_RESOURCE_ID;
		this.executionEngine = new BasicExecutionEngine(1,CLSS);
//		duplicateAction = new DuplicateNodeAction(this);
		deleteNodeAction = null;
//		cutBranchAction = new CutAction(this);
		copyBranchAction = new CopyAction(this);
		pasteBranchAction = new PasteAction(this);
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
//		familyIcon = iconFromPath("Block/icons/navtree/family_folder.png");  
		familyIcon = iconFromPath("Block/icons/navtree/family24.png");  
		diagramIcon = iconFromPath("Block/icons/navtree/diagram.png");  
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
		this.executionEngine = new BasicExecutionEngine(1,CLSS);
		setName(resource.getName());      // Also sets text for tree
//		duplicateAction = new DuplicateNodeAction(this);
		deleteNodeAction = new DeleteNodeAction(this);
//		cutBranchAction = new CutAction(this);
		copyBranchAction = new CopyAction(this);
		pasteBranchAction = new PasteAction(this);

		folderCreateAction = new FolderCreateAction(this);
		workspace = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		alertBadge =iconFromPath("Block/icons/badges/bell.png");
		if(resource.getResourceType().equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			closedIcon = iconFromPath("Block/icons/navtree/application_folder_closed.png");
			openIcon = iconFromPath("Block/icons/navtree/application_folder.png");
		} 
		else if(resource.getResourceType().equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE)) { 
			closedIcon = iconFromPath("Block/icons/navtree/family24.png");
			openIcon = iconFromPath("Block/icons/navtree/family24.png");
		}
		else if(parentIsApplication()) {  //this is a folder containing families 
			closedIcon = iconFromPath("Block/icons/navtree/family_closed.png");
			openIcon = iconFromPath("Block/icons/navtree/family_.png");
		}
		else {
			// Simple folder
			closedIcon = IconUtil.getIcon("folder_closed");
			openIcon = IconUtil.getIcon("folder");
		}
		familyIcon = iconFromPath("Block/icons/navtree/family24.png"); 
		diagramIcon = iconFromPath("Block/icons/navtree/diagram.png"); 
		setIcon(closedIcon);
	}

	// walk up the tree and see if there is a 
	public boolean parentIsApplication() {
		boolean ret = false;
		GeneralPurposeTreeNode theNode = this;

		AbstractResourceNavTreeNode node = nearestNonFolderNode(theNode);
		if (node != null && node.getProjectResource() != null && node.getProjectResource().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			ret = true;
		}
		return ret;				
	}
	
	protected AbstractResourceNavTreeNode nearestNonFolderNode(GeneralPurposeTreeNode node) {
		AbstractResourceNavTreeNode ret = node;
		while (ret != null && ret.getProjectResource() != null && ret.getProjectResource().getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) { // generic folder, go up
			if (ret != null && ret.getParent() instanceof AbstractResourceNavTreeNode) {
				ret = (AbstractResourceNavTreeNode)ret.getParent();
			} else {
				ret = null;
			}
		}
		return ret;
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
	
	public UUID getUUID() { return this.folderId; }
	
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
	// configure production or isolation databases
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
			logger.warnf("%s. startAction: ERROR: %s",CLSS,ex.getMessage(),ex);
			ErrorUtil.showError(CLSS+" Exception listing controller resources",ex);
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
	public String nextFreeName(AbstractResourceNavTreeNode node,String root) {
		
		int childCount = node.getChildCount();
		if( childCount==0 ) return root;
		
		String newName = root;
		boolean foundMatch = true;
		int index = 0;
		while(foundMatch) {
			foundMatch = false;
			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				ProjectResource cres = child.getProjectResource();
				if( cres.getName().equals(newName)) {
					foundMatch=true;
					break;
				}
			}
			if (foundMatch) {
				index = index+1;
				newName = String.format("%s-%d", root,index);
			}
		}
		return newName;
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
			logger.infof("%s.onEdit: alterName from %s to %s",CLSS,oldName,newTextValue);
			context.structuredRename(resourceId, newTextValue);
			executionEngine.executeOnce(new ResourceUpdateManager(workspace,getProjectResource()));
		}
		catch (IllegalArgumentException ex) {
			ErrorUtil.showError(CLSS+".onEdit: "+ex.getMessage());
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
		//logger.infof("%s.projectResourceModified.%s: %s(%d), res %s(%d)",CLSS,changeType.name(),getName(),this.resourceId,res.getName(),res.getResourceId());
		if (res.getResourceId() == this.resourceId) {
			if( res.getName()==null || !res.getName().equals(getName()) ) {
				logger.infof("%s.projectResourceModified(%d), setting name %s to %s",CLSS,this.resourceId,getName(),res.getName());
				CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
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
		logger.infof("%s.projectUpdated ...",CLSS);
		super.projectUpdated(diff);
	}

	/**
	 * Either our state or the state of another node changed, annotate our state. 
	 * Dirtiness for a nav tree node means that one or more of its descendent diagrams
	 * is dirty.
	 * Note: This method should ONLY be called from the node status manager.
	 */
	public void updateUI(boolean dty) {
		logger.debugf("%s.updateUI: %d dirty = %s",CLSS,resourceId,(dty?"true":"false"));
//		setItalic(dty);    // EREIAM JH - Disabled until italic system fixed
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
		
		logger.debugf("%s.createChildNode: %s(%d) type:%s, depth=%d", CLSS,getName(),resourceId,res.getResourceType(),getDepth());
		AbstractResourceNavTreeNode node = statusManager.findNode(res.getResourceId());
		if( node==null ) {
			if (    ProjectResource.FOLDER_RESOURCE_TYPE.equals(res.getResourceType()))       {
				node = new GeneralPurposeTreeNode(context, res, res.getDataAsUUID());
				logger.tracef("%s.createChildNode: (%s) %s->%s",CLSS,res.getResourceType(),this.getName(),node.getName());
			}
			else if ( BLTProperties.APPLICATION_RESOURCE_TYPE.equals(res.getResourceType()) )       {
				SerializableApplication sa = deserializeApplication(res);
				node = new GeneralPurposeTreeNode(context, res, sa.getId());
				logger.tracef("%s.createChildNode: (%s) %s->%s",CLSS,res.getResourceType(),this.getName(),node.getName());
			}
			else if ( BLTProperties.FAMILY_RESOURCE_TYPE.equals(res.getResourceType()) )       {
				SerializableFamily fa = deserializeFamily(res); 
				node = new GeneralPurposeTreeNode(context, res, fa.getId());
				logger.tracef("%s.createChildNode: (%s) %s->%s",CLSS,res.getResourceType(),this.getName(),node.getName());
			}
			else if (BLTProperties.DIAGRAM_RESOURCE_TYPE.equals(res.getResourceType())) {
				node = new DiagramTreeNode(context,res,workspace);
				logger.tracef("%s.createChildDiagram: %s->%s",CLSS,this.getName(),node.getName());
			} 
			else {
				logger.warnf("%s: Attempted to create a child of type %s (ignored)",CLSS,res.getResourceType());
				throw new IllegalArgumentException();
			}
			statusManager.createResourceStatus(node,resourceId, res.getResourceId());
			// Note: This shouldn't be necessary - plus it causes problems on a delete (when we search for resources to delete)
			//executionEngine.executeOnce(new ResourceUpdateManager(workspace,res));   /// Creates, syncs resource
		}
		else {
			logger.debugf("%s.createChildNode: REUSE %s->%s",CLSS,this.getName(),node.getName());
			if( node instanceof DiagramTreeNode ) context.addProjectChangeListener((DiagramTreeNode)node);
		}
		node.install(this);
		if( node.getParent()==null) {
			logger.errorf("%s.createChildNode: ERROR parent is null %s(%d)",CLSS,node.getName(),res.getResourceId());
		}
//		node.setItalic(context.getProject().isResourceDirty(res.getResourceId()));    // EREIAM JH - Disabled until italic system fixed
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
			logger.errorf("%s.initPopupMenu: ERROR: Diagram (%d) has no parent",CLSS,hashCode());
		}
		context.addProjectChangeListener(this);
		if (isRootFolder()) { 
			if( context.getProject().isEnabled()) {
				ApplicationCreateAction applicationCreateAction = new ApplicationCreateAction(this);
				ApplicationImportAction applicationImportAction = new ApplicationImportAction(context.getFrame(),this);
				ClearAction clearAction = new ClearAction();
				DebugAction debugAction = new DebugAction();
				RefreshAction refreshAction = new RefreshAction(this);
				SaveAllAction saveAllAction = new SaveAllAction(this);
				if( handler.isControllerRunning() ) {
					startAction.setEnabled(false);
					clearAction.setEnabled(false);
				} else {
					stopAction.setEnabled(false);
					clearAction.setEnabled(true);
				}
				menu.add(applicationCreateAction);
				menu.add(applicationImportAction);
// todo - removed.  Configure values now set during installation.  Left on for debug mode just in case someone cares   EREIAM JH
				if (logger.isDebugEnabled()) {
					ToolkitConfigureAction configureAction = new ToolkitConfigureAction(menu.getRootPane());
					menu.add(configureAction);
				}
				menu.add(folderCreateAction);
				menu.add(refreshAction);
				menu.add(saveAllAction);
				menu.add(startAction);
				menu.add(stopAction);
				menu.add(pasteBranchAction);
				menu.addSeparator();
				menu.add(clearAction);
				menu.add(debugAction);
			}
		}
		else if( getProjectResource()==null ) {
			logger.warnf("%s.initPopupMenu: ERROR: node %s(%d) has no project resource",CLSS,this.getName(),resourceId);
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
//			menu.add(familyFolderCreateAction);
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
//			menu.add(duplicateAction);
//			menu.add(cutBranchAction);
			menu.add(copyBranchAction);
			menu.add(pasteBranchAction);
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
//			menu.add(duplicateAction);
//			menu.add(cutBranchAction);
			menu.add(copyBranchAction);
			menu.add(pasteBranchAction);
			menu.add(familyConfigureAction);
			menu.add(treeSaveAction);
			addEditActions(menu);

		}
		else if(getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.FOLDER_RESOURCE_TYPE)) {
			
			SetApplicationStateAction ssaActive = new SetApplicationStateAction(this,DiagramState.ACTIVE);
			SetApplicationStateAction ssaDisable = new SetApplicationStateAction(this,DiagramState.DISABLED);
			SetApplicationStateAction ssaIsolated = new SetApplicationStateAction(this,DiagramState.ISOLATED);
			JMenu setStateMenu = new JMenu(BundleUtil.get().getString(PREFIX+".SetApplicationState"));
			setStateMenu.add(ssaActive);
			setStateMenu.add(ssaDisable);
			setStateMenu.add(ssaIsolated);
			menu.add(setStateMenu);
			
			if( hasApplication() ) {
				if( hasFamily() ) {
					DiagramCreateAction diagramAction = new DiagramCreateAction(this);
					menu.add(diagramAction);
					ImportDiagramAction importAction = new ImportDiagramAction(menu.getRootPane(),this);
					menu.add(importAction);
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
				menu.add(importAction);
			}
			
			menu.add(folderCreateAction);
			treeSaveAction = new TreeSaveAction(this,PREFIX+".SaveFolder");
			menu.add(treeSaveAction);
			menu.addSeparator();

			if( true /*something is selected*/ ) {
				copyBranchAction.setEnabled(true);
//				cutBranchAction.setEnabled(true);
			} else {
				copyBranchAction.setEnabled(false);
//				cutBranchAction.setEnabled(false);
			}
			if( true /*something is in the clipboard*/ ) {
				pasteBranchAction.setEnabled(true);
			} else {
				pasteBranchAction.setEnabled(false);
			}
//			menu.add(cutBranchAction);
			menu.add(copyBranchAction);
			menu.add(pasteBranchAction);
			
			addEditActions(menu);	
		}
		else {   
			FamilyCreateAction familyAction = new FamilyCreateAction(this);
			menu.add(familyAction);
			menu.addSeparator();
//			menu.add(cutBranchAction);

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
			logger.warnf("%s.deserializeApplication: Deserialization exception (%s)",CLSS,ex.getMessage());
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
			logger.warnf("%s.SerializableDiagram: Deserialization exception (%s)",CLSS,ex.getMessage());
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
			logger.infof("%s.recursivelyDeserializeApplication: %s (%d)",CLSS,res.getName(),res.getResourceId());
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
					logger.infof("%s.recursivelyDeserializeApplication: %s unexpected child resource type (%s)",CLSS,res.getName(),cres.getName(),cres.getResourceType());
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
			logger.infof("%s.recursivelyDeserializeDiagram: %s (%d)",CLSS,res.getName(),res.getResourceId());
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
			logger.infof("%s.recursivelyDeserializeFamily: %s (%d)",CLSS,res.getName(),res.getResourceId());
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
					logger.infof("%s.recursivelyDeserializeFamily: %s unexpected child resource type (%s)",CLSS,res.getName(),cres.getName(),cres.getResourceType());
				}
			}
		}
		return sfam;
	}

	/**
	 * Do it.  (Note this will diagnosis names to avoid collisions).
	 * @return true if the conversion was a success
	 */
	public boolean renameDiagnosis(SerializableDiagram sd, ProcessBlockView pbv) {
		boolean success = true;
		
		// As we traverse the blocks, find the matching entry
		// so that we can look them up when we update the name 
		for( SerializableBlock sb:sd.getBlocks()) {
			if (sb.getName().equals(pbv.getName())) {
				pbv.createPseudoRandomNameExtension();
				sb.setName(pbv.getName());
			}
		}
		//  update the name now so it doens't cause duplicate name problems on save
		return success;
	}
	

	// Recursively descend the node tree, gathering up associated resources.
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to an application.
	private SerializableFolder recursivelyDeserializeFolder(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		SerializableFolder sfold = null;
		if( res!=null ) {
			logger.infof("%s.recursivelyDeserializeFolder: %s (%d)",CLSS,res.getName(),res.getResourceId());
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
					logger.infof("%s.recursivelyDeserializeFolder: %s unexpected child resource type (%s)",CLSS,res.getName(),cres.getName(),cres.getResourceType());
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
		logger.debugf("%s: serializeApplication creating json ... %s",CLSS,(mapper.canSerialize(SerializableApplication.class)?"true":"false"));
		try{ 
			json = mapper.writeValueAsString(application);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize application (%s)",CLSS,jpe.getMessage());
		}
		logger.debugf("%s: serializeApplication created json ... %s",CLSS,json);
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
		logger.debugf("%s: serializeDiagram creating json ... %s",CLSS,(mapper.canSerialize(SerializableDiagram.class)?"true":"false"));
		try{ 
			json = mapper.writeValueAsString(diagram);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize diagram (%s)",CLSS,jpe.getMessage());
		}
		logger.infof("%s: serializeDiagram created json ... %s",CLSS,json);
		return json;
	}

	/**
	 *  Serialize a Family into JSON.
	 * @param family to be serialized
	 */ 
	private String serializeFamily(SerializableFamily family) {
		String json = "";
		ObjectMapper mapper = new ObjectMapper();
		logger.debugf("%s: serializeFamily creating json ... %s",CLSS,(mapper.canSerialize(SerializableFamily.class)?"true":"false"));
		try{ 
			json = mapper.writeValueAsString(family);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize family (%s)",CLSS,jpe.getMessage());
		}
		logger.debugf("%s: serializeFamily created json ... %s",CLSS,json);
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
				ErrorUtil.showError(CLSS+" Exception configuring application",err);
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

				logger.debugf("%s.ApplicationCreateAction. json=%s",CLSS,json);
				byte[] bytes = json.getBytes();
				logger.infof("%s.ApplicationCreateAction. create new %s(%d), %s (%d bytes)",CLSS,BLTProperties.APPLICATION_RESOURCE_TYPE,newResId,
						newName,bytes.length);
				ProjectResource resource = new ProjectResource(newResId,BLTProperties.MODULE_ID, BLTProperties.APPLICATION_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				logger.infof("%s: parent: %s",CLSS,getFolderId().toString());
				new ResourceCreateManager(resource).run();	// Must be synchronous for child to show
				currentNode.selectChild(new long[] {newResId} );
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception creating application",err);
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
						
						// restore auxiliary data so it gets included in the export
						executionEngine.executeOnce(new AuxiliaryDataRestoreManager(workspace,node));
						
						ExportDialog dialog = new ExportDialog(context.getFrame());
						dialog.setLocationRelativeTo(anchor);
						Point p = dialog.getLocation();
    					dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
						dialog.pack();
						dialog.setVisible(true);   // Returns when dialog is closed
						File output = dialog.getFilePath();
						boolean success = false;
						if( output!=null ) {
							logger.debugf("%s.actionPerformed: dialog returned %s",CLSS,output.getAbsolutePath());
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
									if(logger.isDebugEnabled()) logger.debugf("%s.actionPerformed: creating json ... %s",CLSS,(mapper.canSerialize(SerializableApplication.class)?"true":"false"));
									try{ 
										// Convert the view into a serializable object. Here we reject any nesting that might
										// have been saved in the project resource, and substitute what we know from the nav tree.

										//  executionEngine.executeOnce(new AuxiliaryDataRestoreManager(workspace,node));  nope.
										
										SerializableApplication sap = recursivelyDeserializeApplication(node);
										
										
										restoreAuxData(sap);
										
										
										
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
				ErrorUtil.showError(CLSS+" Exception exporting application",err);
			}
		}
	}

	
	public void restoreAuxData(SerializableApplication sap) {
	
		if (requestHandler == null) {  // only make one if necessary
			requestHandler = new ApplicationRequestHandler();
		}
		String prodDb = requestHandler.getProductionDatabase();
		GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();

		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", sap.getName());   // Use as a key when fetching
	
		CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
		
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				sap.getId().toString(),auxData,prodDb);
		sap.setAuxiliaryData(auxData);

		for (SerializableFamily sf: sap.getFamilies()) {
			restoreAuxData(sf);
		}
	}
	
	public void restoreAuxData(SerializableFamily sf) {
		
		String prodDb = requestHandler.getProductionDatabase();
		GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();

		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", sf.getName());   // Use as a key when fetching
	
		CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
		
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				sf.getId().toString(),auxData,prodDb);
		sf.setAuxiliaryData(auxData);
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
											uuidHandler.convertUUIDs(true);
											logger.infof("%s:ApplicationImportAction importing application %s(%d) (%s)", CLSS,sa.getName(),newId,sa.getId().toString());
											String json = mapper.writeValueAsString(sa);
											if(logger.isTraceEnabled() ) logger.trace(json);
											ProjectResource resource = new ProjectResource(newId,
													BLTProperties.MODULE_ID, BLTProperties.APPLICATION_RESOURCE_TYPE,
													sa.getName(), ApplicationScope.GATEWAY, json.getBytes());


											resource.setParentUuid(getFolderId());
											// Now import families
											for(SerializableFamily fam:sa.getFamilies()) {
												importFamily(sa.getId(),fam);
												saveFamilyAuxData(fam);
											}
											// Create after the children -- else sometimes folders are not populated.
											new ResourceCreateManager(resource).run();   // In-line

											saveApplicationAuxData(sa);
											
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
										logger.infof("%s:ApplicationImportAction reloading (%s)", CLSS,kid.getName());
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
				ErrorUtil.showError(CLSS+" Exception importing application",err);
			}
		}
		
		public void saveApplicationAuxData(SerializableApplication sa) {
			
			ApplicationRequestHandler requestHandler = new ApplicationRequestHandler();
			String prodDb = requestHandler.getProductionDatabase();
			
			CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
			extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
					sa.getId().toString(),sa.getAuxiliaryData(),prodDb);

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
				logger.infof("%s:ApplicationImportAction importing diagram %s(%d) (%s)", CLSS,sd.getName(),newId,sd.getId().toString());
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
				logger.infof("%s:ApplicationImportAction importing family %s(%s) (%s/%s)", CLSS,sf.getName(),newId,parentId.toString(),sf.getId().toString());
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
	// The resource type can be Application or Family
//	private class DuplicateNodeAction extends BaseAction {
//		private static final long serialVersionUID = 1L;
//		private final AbstractResourceNavTreeNode parentNode;
//		public DuplicateNodeAction(AbstractResourceNavTreeNode pNode)  {
//			super(PREFIX+".DuplicateNode",IconUtil.getIcon("copy"));  // preferences
//			this.parentNode = pNode;
//		}
//
//		public void actionPerformed(ActionEvent e) {
//			try {
//				EventQueue.invokeLater(new Runnable() {
//					public void run() {
//						try {	
//							ObjectMapper mapper = new ObjectMapper();
//							mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
//
//							ProjectResource res = parentNode.getProjectResource();
//							long newId = context.newResourceId();
//							UUID newResourceUUID = null;
//							String json = "";
//							if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
//								SerializableApplication sa = mapper.readValue(new String(res.getData()), SerializableApplication.class);
//								if( sa!=null ) {
//									ApplicationUUIDResetHandler uuidHandler = new ApplicationUUIDResetHandler(sa);
//									uuidHandler.convertUUIDs();
//									newResourceUUID = sa.getId();
//									json = mapper.writeValueAsString(sa);
//								}
//								else {
//									ErrorUtil.showWarning(String.format("Failed to deserialize application (%s)",res.getName()),"Duplicate Application");
//									return;
//								}
//							}
//							else if( res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
//								SerializableFamily sf = mapper.readValue(new String(res.getData()), SerializableFamily.class);
//								if( sf!=null ) {
//									FamilyUUIDResetHandler uuidHandler = new FamilyUUIDResetHandler(sf);
//									uuidHandler.convertUUIDs();
//									newResourceUUID = sf.getId();
//									json = mapper.writeValueAsString(sf);
//								}
//								else {
//									ErrorUtil.showWarning(String.format("Failed to deserialize family (%s)",res.getName()),"Duplicate Family");
//									return;
//								}
//							}
//							else {
//								ErrorUtil.showWarning(String.format("Unexpected resource type(%s)",res.getResourceType()),"Duplicate Node");
//								return;
//							}
//							GeneralPurposeTreeNode grandparent = (GeneralPurposeTreeNode)parentNode.getParent();
//							ProjectResource resource = new ProjectResource(newId,BLTProperties.MODULE_ID, res.getResourceType(),
//									nextFreeName(grandparent,res.getName()), ApplicationScope.GATEWAY, json.getBytes());
//
//							resource.setParentUuid(grandparent.getUUID());
//							
//							copyChildren(parentNode,newResourceUUID);
//							
//							// Finally display the parent node
//							new ResourceCreateManager(resource).run();
//							grandparent.selectChild(new long[] {newId} );	
//						}
//						catch( IOException ioe) {
//							// Should never happen, we just picked this off a chooser
//							logger.warnf("%s: actionPerformed, IOException(%s)",CLSS,ioe.getLocalizedMessage()); 
//						}
//						catch (Exception ex) {
//							logger.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
//						}
//					}
//				});
//			} 
//			catch (Exception err) {
//				ErrorUtil.showError(CLSS+" Exception cloning diagram",err);
//			}
//		}
//	}
		// Recursively clone children of the parent node onto the new tree
		// It would be nice to simply convert to a resource, but
		// unfortunately, we have to replace all UUIDs with new ones
	
	
	
		protected boolean copyChildren(AbstractResourceNavTreeNode fromNode, UUID toUUID) throws Exception {
//			boolean ret = true;
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			
			if (fromNode == null) {
				ErrorUtil.showWarning("copy children with NULL node!");
				return false;
			}
			// Iterate over the children of the original node
			@SuppressWarnings("rawtypes")
			Enumeration walker = fromNode.children();
//			boolean seagullsStopItNow = false;  // flag to stop it from looping on a diagram

//			// special handling for diagram nodes.  No children, but has blocks that need updating
//			if (!walker.hasMoreElements() && fromNode.getProjectResource().getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
//				Vector<AbstractResourceNavTreeNode> walkerV = new Vector<AbstractResourceNavTreeNode> ();
//				walkerV.add(fromNode);
//				walker = walkerV.elements();
//				seagullsStopItNow = true;
//			}
				
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				long newId = context.newResourceId();
				ProjectResource res = child.getProjectResource();
				byte[] bytes = res.getData();
				String json = "";
				UUID newResourceUUID = null;
				
				if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
					SerializableApplication sa = mapper.readValue(new String(bytes), SerializableApplication.class);
					if( sa!=null ) {
						ApplicationUUIDResetHandler uuidHandler = new ApplicationUUIDResetHandler(sa);
						uuidHandler.convertUUIDs(false);
						HashMap <UUID, UUID> lookup = uuidHandler.getIdLookup();
						for (UUID key : lookup.keySet()) {
							bigLookup.put(lookup.get(key), key);  // reverse it so we can use the current UUID to find the original
						}
						newResourceUUID = sa.getId();
						json = mapper.writeValueAsString(sa);
					}
					else {
						ErrorUtil.showWarning(String.format("Failed to deserialize child application (%s)",res.getName()),"Copy Application");
						return false;
					}
				}
				else if( res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
					SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
					if( sd!=null ) {
						UUIDResetHandler uuidHandler = new UUIDResetHandler(sd);
						uuidHandler.convertUUIDs();
						HashMap <UUID, UUID> lookup = uuidHandler.getBlockLookup();
						for (UUID key : lookup.keySet()) {
							bigLookup.put(key, lookup.get(key));  // DON'T *** reverse it so we can use the current UUID to find the original
						}
						ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
						for( Block blk:diagram.getBlocks()) {
							ProcessBlockView pbv = (ProcessBlockView)blk;
							if (pbv.isDiagnosis()) {
								renameDiagnosis(sd, pbv);
								
								continue;
							}
						}
						
						sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
						sd.setState(DiagramState.DISABLED);
						newResourceUUID = sd.getId();
						json = mapper.writeValueAsString(sd);
					}
					else {
						ErrorUtil.showWarning(String.format("Failed to deserialize child diagram (%s)",res.getName()),"Copy Diagram");
						return false;
					}
				}
				else if( res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
					SerializableFamily sf = mapper.readValue(new String(bytes), SerializableFamily.class);
					if( sf!=null ) {
						FamilyUUIDResetHandler uuidHandler = new FamilyUUIDResetHandler(sf);
						uuidHandler.convertUUIDs(false);
						newResourceUUID = sf.getId();
						HashMap <UUID, UUID> lookup = uuidHandler.getIdLookup();
						for (UUID key : lookup.keySet()) {
							bigLookup.put(lookup.get(key), key);  // reverse it so we can use the current UUID to find the original
						}
						json = mapper.writeValueAsString(sf);
					}
					else {
						ErrorUtil.showWarning(String.format("Failed to deserialize child family (%s)",res.getName()),"Copy Family");
						return false;
					}
				}
				else if( res.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
					SerializableFolder sf = mapper.readValue(new String(bytes), SerializableFolder.class);
					if( sf!=null ) {
						sf.setId(UUID.randomUUID());
						newResourceUUID = sf.getId();
						json = mapper.writeValueAsString(sf);
					}
					else {
						ErrorUtil.showWarning(String.format("Failed to deserialize child folder (%s)",res.getName()),"Copy Folder");
						return false;
					}
				}
				else {
					ErrorUtil.showWarning(String.format("Unexpected child resource type(%s)",res.getResourceType()),"Copy Node");
					return false;
				}
				
				

				ProjectResource resource = new ProjectResource(newId,BLTProperties.MODULE_ID, res.getResourceType(),
					res.getName(), ApplicationScope.GATEWAY, json.getBytes());

				resource.setParentUuid(toUUID);
				
				
				new ResourceCreateManager(resource).run();
				
//				if (seagullsStopItNow || !copyChildren(child,newResourceUUID)) {
				if (!copyChildren(child,newResourceUUID)) {
					return false;
				}
			}
			return true;
		} 

		
		// type can be ScriptConstants.APPLICATION_CLASS_NAME or similar
		public void copyAuxData(String type, String oldId, String newId, String oldName, String newName) {
			
			String prodDb = requestHandler.getProductionDatabase();
			String isoDb = requestHandler.getIsolationDatabase();
			GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();

			auxData.setProperties(new HashMap<String,String>());
			auxData.setLists(new HashMap<>());
			auxData.setMapLists(new HashMap<>());
			auxData.getProperties().put("Name", oldName);   // Use as a key when fetching
		
			CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
			
			extensionManager.runScript(context.getScriptManager(), type, ScriptConstants.PROPERTY_GET_SCRIPT, 
					oldId,auxData,isoDb);

			auxData.getProperties().put("Name", newName);   // Set new key

			extensionManager.runScript(context.getScriptManager(), type, ScriptConstants.PROPERTY_SET_SCRIPT, 
					newId,auxData,isoDb);
			
			
			auxData = new GeneralPurposeDataContainer();
			
			auxData.setProperties(new HashMap<String,String>());
			auxData.setLists(new HashMap<>());
			auxData.setMapLists(new HashMap<>());
			auxData.getProperties().put("Name", oldName);   // Use as a key when fetching
			
			extensionManager.runScript(context.getScriptManager(), type, ScriptConstants.PROPERTY_GET_SCRIPT, 
					oldId,auxData,prodDb);

			auxData.getProperties().put("Name", newName);   // Set new key
			
			extensionManager.runScript(context.getScriptManager(), type, ScriptConstants.PROPERTY_SET_SCRIPT, 
					newId,auxData,prodDb);
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

	
	// copy the currently selected node UUID to the clipboard
	private class CopyAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode parentNode;

		public CopyAction(AbstractResourceNavTreeNode pNode)  {
			super(PREFIX+".CopyNode",IconUtil.getIcon("copy"));
			this.parentNode = pNode;
		}

		public void actionPerformed(ActionEvent e) {
           final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

           ProjectResource res = parentNode.getProjectResource();
           
           RestoreAuxiliaryDataAction restoreAuxDataAction = new RestoreAuxiliaryDataAction((GeneralPurposeTreeNode)parentNode);
           restoreAuxDataAction.actionPerformed(e);
           String data = ""+res.getResourceId();
           Transferable t =  new StringSelection(BLT_COPY_OPERATION + data);
				   
		   if (t != null) {
			   try { 
				   clipboard.setContents(t, null); 
			   } catch (Exception ex) {
					logger.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
			   }
		   }
				   
		}
	}

//	// copy the currently selected node UUID to the clipboard
//	private class CutAction extends BaseAction {
//		private static final long serialVersionUID = 1L;
//		private final AbstractResourceNavTreeNode parentNode;
//
//		public CutAction(GeneralPurposeTreeNode pNode)  {
//			super(PREFIX+".CutNode",IconUtil.getIcon("cut"));
//			this.parentNode = pNode;
//		}
//
//		public void actionPerformed(ActionEvent e) {
//	       final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
//
//	       ProjectResource res = parentNode.getProjectResource();
//           String data = ""+res.getResourceId();
//           Transferable t =  new StringSelection(BLT_CUT_OPERATION + data);
//				   
//		   if (t != null) {
//			   try { 
//				   clipboard.setContents(t, null); 
//			   } catch (Exception ex) {
//					logger.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
//			   }
//		   }
//				   
//		}
//	}

	// paste the nodes from the clipboard into the tree at the selected location
	private class PasteAction extends BaseAction  implements UndoManager.UndoAction{
		private static final long serialVersionUID = 1L;
		private final GeneralPurposeTreeNode parentNode;  // Bad naming, it isn't really a parent node, it's this one
		private String bundleString;
		private ProjectResource pasted = null;

		public PasteAction(GeneralPurposeTreeNode pNode)  {
			super(PREFIX+".PasteNode",IconUtil.getIcon("paste"));
			this.bundleString = PREFIX+".PasteNode";
			this.parentNode = pNode;
		}

		@Override
		public boolean isGroupSequenceIndependent() {return false;}

		@Override
		public boolean undo() {
			
			context.deleteResource(pasted.getResourceId());

			return true;
		}
		
		@Override
		public String getDescription() { return " paste"; }  // This could be made more descriptive

		@Override
		public boolean execute() {
			boolean result = true;
			final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

	        Transferable t = clipboard.getContents(null);
     		if (t.isDataFlavorSupported(DataFlavor.stringFlavor)) {
     			try { 
     				String clipData = (String)t.getTransferData(DataFlavor.stringFlavor);
     				if (clipData.startsWith(BLT_CUT_OPERATION)) {
     					String data = clipData.substring(BLT_CUT_OPERATION.length());
    			        paste(data, true);
     				}
     				if (clipData.startsWith(BLT_COPY_OPERATION)) {
     					String data = clipData.substring(BLT_COPY_OPERATION.length());
    					paste(data, false);
     				}
     			} catch (Exception ex) {
     				result = false;
					logger.errorf("%s: actionPerformed: Unhandled Exception in PASTE (%s)",CLSS,ex.getMessage());
					ex.printStackTrace();
     			}
     		}
     		return result;
		}



		public void actionPerformed(ActionEvent e) {
			execute();
		}

//		private AbstractResourceNavTreeNode nearestNonFolderNode(GeneralPurposeTreeNode node) {
//			AbstractResourceNavTreeNode ret = node;
//			while (ret != null && ret.getProjectResource() != null && ret.getProjectResource().getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) { // generic folder, go up
//				if (ret != null && ret.getParent() instanceof AbstractResourceNavTreeNode) {
//					ret = (AbstractResourceNavTreeNode)ret.getParent();
//				} else {
//					ret = null;
//				}
//			}
//			return ret;
//		}
		
		
		
		public void paste(String data, boolean deleteOriginal) {
			long clipId = Long.parseLong(data);
			ProjectResource res = context.getProject().getResource(clipId);
			
//			Up to here it works.  You can now cut or copy a resource, paste is able to find the original resource
//				delete old one (cut only)
//			
//				*Note cut disabled for now.  Just copy & delete	
			PasteAction foo = this;
	        
	        ProjectResource dst = parentNode.getProjectResource();
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						try {	
							ObjectMapper mapper = new ObjectMapper();
							mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
							SerializableDiagram sd = null;

							long newId = context.newResourceId();
							UUID newResourceUUID = null;
							String json = "";
							
							SerializableApplication sa = null; 
							SerializableFamily sf = null;
							bigLookup = new HashMap<UUID,UUID>();  // filled by copyChildren.  Used to store all the converted UUIDs so we can reference them to restore aux data
//							HashMap<ProcessBlockView, String> diagnosisChildren = new HashMap<ProcessBlockView, String>(); 
							if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
								 AbstractResourceNavTreeNode closest = nearestNonFolderNode(parentNode);
								if (closest instanceof GeneralPurposeTreeNode && ((GeneralPurposeTreeNode)closest).isRootFolder()) {
									sa = mapper.readValue(new String(res.getData()), SerializableApplication.class);
									if( sa!=null ) {
										ApplicationUUIDResetHandler uuidHandler = new ApplicationUUIDResetHandler(sa);
										uuidHandler.convertUUIDs(false);
										newResourceUUID = sa.getId();
										json = mapper.writeValueAsString(sa);
									} else {
										ErrorUtil.showWarning(String.format("Failed to deserialize application (%s)",res.getName()),"Paste Application");
										return;
									}
								
								} else {
									ErrorUtil.showWarning("Tried to paste an application into an invalid location","Paste Application");
									return;
								}
							} else if(res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
								AbstractResourceNavTreeNode node = nearestNonFolderNode(parentNode);
								if (node.getProjectResource() != null && node.getProjectResource().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
									sf = mapper.readValue(new String(res.getData()), SerializableFamily.class);
									if( sf!=null ) {
										FamilyUUIDResetHandler uuidHandler = new FamilyUUIDResetHandler(sf);
										uuidHandler.convertUUIDs(false);  // this goes deep but doesn't matter.  just getting a new UUID
										newResourceUUID = sf.getId();
										json = mapper.writeValueAsString(sf);
									}
									else {
										ErrorUtil.showWarning(String.format("Failed to deserialize family (%s)",res.getName()),"Paste Family");
										return;
									}
								} else {
									ErrorUtil.showWarning("Tried to paste a family into an invalid location","Paste Application");
									return;
								}
							} else if( res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
								AbstractResourceNavTreeNode node = nearestNonFolderNode(parentNode);
								if (node.getProjectResource() != null && node.getProjectResource().getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
									sd = mapper.readValue(new String(res.getData()), SerializableDiagram.class);
									if( sd!=null ) {
										ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
										UUIDResetHandler rhandler = new UUIDResetHandler(sd);
										rhandler.convertUUIDs();  // converted UUIDs are thrown away because later CopyChildren also does it
										newResourceUUID = sd.getId();
										
										HashMap <UUID, UUID> lookup = rhandler.getBlockLookup();
										for (UUID key : lookup.keySet()) {
											bigLookup.put(key, lookup.get(key));  // DON'T *** reverse it so we can use the current UUID to find the original
										}
										for( Block blk:diagram.getBlocks()) {
											ProcessBlockView pbv = (ProcessBlockView)blk;
											if (pbv.isDiagnosis()) {
												renameDiagnosis(sd, pbv);
												continue;
											}
										}
											
										sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
										sd.setState(DiagramState.DISABLED);
										newResourceUUID = sd.getId();
										json = mapper.writeValueAsString(sd);
										statusManager.setResourceState(newId, sd.getState(),false);
										
										
//										if (diagnosis) {
//											ErrorUtil.showWarning("Diagram contains diagnosis blocks that must be renamed before saving");
//										}
										
									}
									else {
										ErrorUtil.showWarning(String.format("Failed to deserialize diagram (%s)",res.getName()),"Paste Diagram");
										return;
									}
								} else {
									ErrorUtil.showWarning("Tried to paste a diagram into an invalid location","Paste Application");
									return;
								}

							} else {
								ErrorUtil.showWarning(String.format("Unexpected resource type(%s)",res.getResourceType()),"Paste Node");
								return;
							}
							
							ProjectResource resource = new ProjectResource(newId,BLTProperties.MODULE_ID, res.getResourceType(),
									nextFreeName((GeneralPurposeTreeNode)parentNode,res.getName()), ApplicationScope.GATEWAY, json.getBytes());

							resource.setParentUuid(((GeneralPurposeTreeNode)parentNode).getUUID());
							
							AbstractResourceNavTreeNode node = statusManager.findNode(res.getResourceId());  // so basically starting over here
							
//							Copies children and assigns new UUIDs
							if (copyChildren(node,newResourceUUID) && deleteOriginal) { // copy children will rename diagnosis blocks
		    					ResourceDeleteManager deleter;
		    					deleter = new ResourceDeleteManager(node);
		    					deleter.acquireResourcesToDelete();
		    					if( deleter.deleteResources() ) {
	//	    						UndoManager.getInstance().add(this,GeneralPurposeTreeNode.class);
	//	    						
	//	    						if( p instanceof GeneralPurposeTreeNode )  {
	//	    							GeneralPurposeTreeNode parentNode = (GeneralPurposeTreeNode)p;
	//	    							parentNode.recreate();
	//	    							parentNode.expand();
	//	    						}
		    						deleter.deleteInProject();
		    					}
		    					else {
		    						ErrorUtil.showError("Node locked, delete failed");
		    					}
							}
							
							// Finally display the parent node
							new ResourceCreateManager(resource).run();

							AbstractResourceNavTreeNode newNode = statusManager.findNode(resource.getResourceId());  // so basically starting over here
							
							// Not always a GeneralPurposeTree node  DiagramTreeNode

							if( !res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
								restoreAuxData((AbstractNavTreeNode)node, (AbstractNavTreeNode)newNode, bigLookup);
							} else {
								restoreDiagramAuxData((DiagramTreeNode)node, resource, bigLookup);
							}

							UndoManager.getInstance().add(foo,AbstractNavTreeNode.class);
							pasted = resource;
							((GeneralPurposeTreeNode)parentNode).selectChild(new long[] {newId} );
							
							
							
						}
						catch( IOException ioe) {
							// Should never happen, we just picked this off a chooser
							logger.warnf("%s: actionPerformed, IOException(%s)",CLSS,ioe.getLocalizedMessage()); 
							ioe.printStackTrace();
						}
						catch (Exception ex) {
							logger.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
							ex.printStackTrace();
						}
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception cloning diagram",err);
			}
			
		}
		
		
		// Look up the new resource by UUID.  Use the lookup table to cross reference and find the original.
		// Read the aux data from the old one and then copy it to the new one.
		//
		//  Follow up by then recursively calling this routine with any child nodes
		
		public void restoreAuxData(AbstractNavTreeNode sourceInNode, AbstractNavTreeNode newInNode, HashMap<UUID,UUID> lookup) {
			if (requestHandler == null) {  // only make one if necessary
				requestHandler = new ApplicationRequestHandler();
			}
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
//			String prodDb = requestHandler.getProductionDatabase();
//			String isoDb = requestHandler.getIsolationDatabase();
//			GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();

			
			
//			So, the big question is how to get the right UUID for the getapplication function to find it.
			
//			auxData.setProperties(new HashMap<String,String>());
//			auxData.setLists(new HashMap<>());
//			auxData.setMapLists(new HashMap<>());
//			auxData.getProperties().put("Name", sourceNode.getName());   // Use as a key when fetching
		
			CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
			
	
			if(sourceInNode instanceof GeneralPurposeTreeNode) {
				GeneralPurposeTreeNode sourceNode = (GeneralPurposeTreeNode)sourceInNode; 
				GeneralPurposeTreeNode newNode = (GeneralPurposeTreeNode)newInNode; 
				
				if(sourceNode.getProjectResource().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
					copyAuxData(ScriptConstants.APPLICATION_CLASS_NAME, sourceNode.getUUID().toString(), newNode.getUUID().toString(), sourceNode.getName(), newNode.getName());
					for (AbstractNavTreeNode child : sourceNode.children) {
						if (child instanceof GeneralPurposeTreeNode == false) {
							logger.errorf("ERROR!!  Child node is NOT an instance of GeneralPurposeTreeNode!!!!!!!!!! " + child.getClass().getName());
						}
						if (((GeneralPurposeTreeNode)child).getProjectResource().getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
							UUID newUuid = lookup.get(((GeneralPurposeTreeNode)child).getUUID());  // find the matching UUID from the table
							for (AbstractNavTreeNode tChild : newNode.children) {
								if (tChild instanceof GeneralPurposeTreeNode == false) {
									logger.errorf("ERROR!!  tChild node is NOT an instance of GeneralPurposeTreeNode!!!!!!!!!! " + child.getClass().getName());
								}
								if (((GeneralPurposeTreeNode)tChild).getUUID().equals(newUuid)) {  // we got a match!  Copy that sucker
									restoreAuxData((GeneralPurposeTreeNode)child, (GeneralPurposeTreeNode)tChild, lookup);
								}
							}
							
						}
					}
				
				} else if(sourceNode.getProjectResource().getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
					copyAuxData(ScriptConstants.FAMILY_CLASS_NAME, sourceNode.getUUID().toString(), newNode.getUUID().toString(), sourceNode.getName(), newNode.getName());
	
	
					for (AbstractNavTreeNode child : sourceNode.children) {
						if (child instanceof DiagramTreeNode == false) {
							logger.errorf("ERROR!!  Child node is NOT an instance of DiagramTreeNode!!!!!!!!!! " + child.getClass().getName());
						} else {
							
							for (AbstractNavTreeNode tChild : newNode.children) {
								if (tChild instanceof DiagramTreeNode) {
									restoreAuxData(child, tChild, lookup);
								}
							}
						}
							
					}
				}
				
			} else if(sourceInNode instanceof DiagramTreeNode) {
				DiagramTreeNode sourceNode = (DiagramTreeNode)sourceInNode; 
				DiagramTreeNode newNode = (DiagramTreeNode)newInNode;  // Why is this null if it's a diagram copy?  Everything else works

				try {
					SerializableDiagram sourceSd = mapper.readValue(new String(sourceNode.getProjectResource().getData()), SerializableDiagram.class);
					SerializableDiagram newSd = mapper.readValue(new String(newNode.getProjectResource().getData()), SerializableDiagram.class);
					
					
					
					// This is wrong.  Spin through kids and get diagnosis blocks, then make the copy calls.  Note that the source ID to use is the diagram, not the block!
	//				copyAuxData(ScriptConstants.DIAGRAM_CLASS_NAME, sourceSd.getId().toString(), newSd.getId().toString(), sourceNode.getName(), newNode.getName());
					
						
					for( SerializableBlock oldsb:sourceSd.getBlocks()) {
						ProcessBlockView pbv = new ProcessBlockView(oldsb);  // make it a pbv for convenience
						String pbvId = pbv.getId().toString();
						if (pbv.isDiagnosis()) {
							UUID oldUuid = lookup.get(pbv.getId()); 
	
							//Now find the match in the new tree
							for( SerializableBlock newsb:newSd.getBlocks()) {
								if (newsb.getId().equals(oldUuid)) {
									copyAuxData(pbv.getClassName(), sourceSd.getId().toString(), newSd.getId().toString(), pbv.getName(), newsb.getName());
									continue;
								}
							}
						}
					}
				
				}	catch (Exception ex) {
					logger.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
					ex.printStackTrace();
				}

			} else {
				ErrorUtil.showWarning("Bad Node Type (" + sourceInNode.getClass().getName() + ")");
				return;
			}
		
		} 

			//             Call the right one based on the resource type (diagram, application, family)
	}
			
		
	// Look up the new resource by UUID.  Use the lookup table to cross reference and find the original.
	// Read the aux data from the old one and then copy it to the new one.
	//
	//  Follow up by then recursively calling this routine with any child nodes
	
	public void restoreDiagramAuxData(DiagramTreeNode sourceInNode, ProjectResource res, HashMap<UUID,UUID> lookup) {
		if (requestHandler == null) {  // only make one if necessary
			requestHandler = new ApplicationRequestHandler();
		}
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
		
		SerializableDiagram newSd = null;
		try {
			newSd = mapper.readValue(new String(res.getData()), SerializableDiagram.class);
	
			CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
	

				SerializableDiagram sourceSd = mapper.readValue(new String(sourceInNode.getProjectResource().getData()), SerializableDiagram.class);
//				SerializableDiagram newSd = mapper.readValue(new String(newNode.getProjectResource().getData()), SerializableDiagram.class);

				// This is wrong.  Spin through kids and get diagnosis blocks, then make the copy calls.  Note that the source ID to use is the diagram, not the block!
//				copyAuxData(ScriptConstants.DIAGRAM_CLASS_NAME, sourceSd.getId().toString(), newSd.getId().toString(), sourceNode.getName(), newNode.getName());
				
					
				for( SerializableBlock oldsb:sourceSd.getBlocks()) {
					ProcessBlockView pbv = new ProcessBlockView(oldsb);  // make it a pbv for convenience
					String pbvId = pbv.getId().toString();
					if (pbv.isDiagnosis()) {
						UUID oldUuid = lookup.get(pbv.getId()); 

						//Now find the match in the new tree
						for( SerializableBlock newsb:newSd.getBlocks()) {
							if (newsb.getId().equals(oldUuid)) {
								copyAuxData(pbv.getClassName(), sourceSd.getId().toString(), newSd.getId().toString(), pbv.getName(), newsb.getName());
								continue;
							}
						}
					}
				}
			
		}	catch (Exception ex) {
			logger.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
			ex.printStackTrace();
		}
	} 

		
	

	public GeneralPurposeDataContainer getFamilyAuxData(SerializableFamily sf) {
		
		ApplicationRequestHandler requestHandler = new ApplicationRequestHandler();
		String prodDb = requestHandler.getProductionDatabase();
		GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();
		
		CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", sf.getName());   // Use as a key when fetching
	
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				sf.getId().toString(),auxData,prodDb);

		return auxData;

	}

	public void saveFamilyAuxData(SerializableFamily sf) {
		
		ApplicationRequestHandler requestHandler = new ApplicationRequestHandler();
		String prodDb = requestHandler.getProductionDatabase();
		
		CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
		String idStr = sf.getId().toString();
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
				idStr,sf.getAuxiliaryData(),prodDb);

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
			logger.infof("%s.DeleteNodeAction: %s, resource %d.",CLSS,node.getName(),resid);
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
		public String getDescription() { return new String(BundleUtil.get().getStringLenient(bundleString) + " delete"); }

	}

	// Create a new diagram
	private class DiagramCreateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode currentNode;
		public DiagramCreateAction(AbstractResourceNavTreeNode parentNode)  {
			super(PREFIX+".NewDiagram",diagramIcon);  // preferences
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
				logger.debugf("%s.DiagramCreateAction. json=%s",CLSS,json);
				byte[] bytes = json.getBytes();
				logger.infof("%s.DiagramCreateAction. create new %s(%d), %s (%d bytes)",CLSS,BLTProperties.DIAGRAM_RESOURCE_TYPE,newId,
						newName,bytes.length);
				ProjectResource resource = new ProjectResource(newId,BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				logger.infof("%s: parent: %s",CLSS,getFolderId().toString());
				new ResourceCreateManager(resource).run();	
				currentNode.selectChild(new long[] {newId} );
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						workspace.open(newId);
					}
				});

			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception creating diagram",err);
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
						logger.infof("%s.actionPerformed: deserializing ...%d",CLSS,resourceId);
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
				ErrorUtil.showError(CLSS+" Exception configuring family",err);
			}
		}
	}
	// From the root node, create a folder for diagrams belonging to a family
	private class FamilyCreateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode currentNode;
		public FamilyCreateAction(AbstractResourceNavTreeNode parentNode)  {
			super(PREFIX+".NewFamily",familyIcon);
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

				logger.debugf("%s.FamilyCreateAction. json=%s",CLSS,json);
				byte[] bytes = json.getBytes();
				logger.infof("%s.FamilyCreateAction. create new %s(%d), %s (%d bytes)",CLSS,BLTProperties.FAMILY_RESOURCE_TYPE,newId,
						newName,bytes.length);
				ProjectResource resource = new ProjectResource(newId,BLTProperties.MODULE_ID, BLTProperties.FAMILY_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				logger.infof("%s.familyCreateAction: res(%d) %s, fam %s, parent: %s",CLSS,newId,resource.getName(),fam.getName(),getFolderId().toString());
				new ResourceCreateManager(resource).run();	
				//recreate();
				currentNode.selectChild(new long[] {newId} );
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception creating family",err);
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
				logger.infof("%s.FolderCreateAction. create new %s(%d), %s (%s, parent %s)",CLSS,BLTProperties.FOLDER_RESOURCE_TYPE,newResId,newName,
						newId.toString(),getFolderId().toString());
				//recreate();
				currentNode.selectChild(new long[] {newResId} );
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception creating folder",err);
			}

		}
	}

	// TODO EREIAM JH - This seems like a lot of work just to make it have a custom icon.
	
	// From an application node, create a folder for families
//	private class FamilyFolderCreateAction extends BaseAction {
//		private static final long serialVersionUID = 1L;
//		private final AbstractResourceNavTreeNode currentNode;
//		public FamilyFolderCreateAction(AbstractResourceNavTreeNode parentNode)  {
//			super(PREFIX+".NewFolder",IconUtil.getIcon("folder_new"));
//			currentNode = parentNode;
//		}
//
//		public void actionPerformed(ActionEvent e) {
//			try {
//				final long newResId = context.newResourceId();
//				String newName = BundleUtil.get().getString(PREFIX+".NewFolder.Default.Name");
//				if( newName==null) newName = "New Folks";  // Missing Resource
//				UUID newId = context.addFolder(newResId, BLTProperties.MODULE_ID, ApplicationScope.GATEWAY, newName, getFolderId());
//				logger.infof("%s.FolderCreateAction. create new %s(%d), %s (%s, parent %s)",CLSS,BLTProperties.FAMILY_FOLDER_TYPE,newResId,newName,
//						newId.toString(),getFolderId().toString());
//				//recreate();
//				currentNode.selectChild(new long[] {newResId} );
//			} 
//			catch (Exception err) {
//				ErrorUtil.showError(CLSS+" Exception creating folder",err);
//			}
//
//		}
//	}
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
											logger.infof("%s:ImportDiagramAction imported diagram:\n%s", CLSS,sd.getName());
											UUIDResetHandler uuidHandler = new UUIDResetHandler(sd);
											uuidHandler.convertUUIDs();
											sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
											sd.setState(DiagramState.DISABLED);
											String json = mapper.writeValueAsString(sd);
											logger.debugf("%s:ImportDiagramAction saved resource as:\n%s", CLSS,json);
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
				ErrorUtil.showError(CLSS+" Exception importing diagram",err);
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
	protected class RestoreAuxiliaryDataAction extends BaseAction {
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
			ResourceSaveManager rsm = new ResourceSaveManager(workspace,node);
			rsm.saveSynchronously();
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
				logger.warnf("%s: startAction: ERROR: %s",CLSS,ex.getMessage(),ex);
				ErrorUtil.showError(CLSS+" Exception starting the controller",ex);
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
				logger.warnf("%s: stopAction: ERROR: %s",CLSS,ex.getMessage(),ex);
				ErrorUtil.showError(CLSS+" Exception stopping the controller",ex);
			}
		}
	}
	// Launch a dialog to configure toolkit-wide attributes.
// todo - removed for now EREIAM JH

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
				ErrorUtil.showError(CLSS+" Exception configuring toolkit",err);
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
			// Traverse the hierarchy under the selection, saving each step
			node.setBold(true);
			threadCounter.reset();
			statusManager.updateAll();
			ResourceSaveManager rsm = new ResourceSaveManager(workspace,node);
			rsm.saveSynchronously();
			ThreadCompletionDetector detector = new ThreadCompletionDetector(node);
			new Thread(detector).start();
		}
	}
	
	// This feels like it doesn't belong here.  The Node shouldn't know about the tree structure
	public String scanForNameConflicts(AbstractResourceNavTreeNode node) {
		String ret = "";
		
		// check if root node is a Application
		if (node.getProjectResource() != null && node.getProjectResource().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			ret = ((GeneralPurposeTreeNode)node).scanApplicationForDuplicateDiagnosisNames();  // ereiam jh todo come back
		} else {
			Enumeration<AbstractResourceNavTreeNode> enumer = node.children();
			while(enumer.hasMoreElements()) {
				AbstractResourceNavTreeNode theNode = enumer.nextElement();
				ret += scanForNameConflicts(theNode);
			}
		}
		return ret;
	}

	public String scanApplicationForDuplicateDiagnosisNames() {
		String ret = "";
		HashMap<String,String> names = new HashMap<String,String>();
		
		Enumeration<AbstractResourceNavTreeNode> enumer = children();
		while(enumer.hasMoreElements()) {
			AbstractResourceNavTreeNode theNode = enumer.nextElement();
			ret += parseChildForConflicts(names, theNode); 
		}
		return ret;
	}

	public String parseChildForConflicts(HashMap<String, String> names, AbstractResourceNavTreeNode theNode) {
		String ret = "";
		if (theNode.getProjectResource().getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {

			ProjectResource res = context.getProject().getResource(theNode.getProjectResource().getResourceId());	
			String json = new String(res.getData());
			SerializableDiagram sd = null;
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			try {
				sd = mapper.readValue(json,SerializableDiagram.class);
				// Synchronize names as the resource may have been re-named and/or
				// state changed since it was serialized
				sd.setName(res.getName());
				sd.setState(statusManager.getResourceState(resourceId));
			} 
			catch (JsonParseException jpe) {
				logger.warnf("%s: open parse exception (%s)",CLSS,jpe.getLocalizedMessage());
			} 
			catch (JsonMappingException jme) {
				logger.warnf("%s: open mapping exception (%s)",CLSS,jme.getLocalizedMessage());
			} 
			catch (IOException ioe) {
				logger.warnf("%s: open io exception (%s)",CLSS,ioe.getLocalizedMessage());
			}
			ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
			for( Block blk:diagram.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				if (pbv.isDiagnosis()) {
					String key = (pbv.getClassName() + pbv.getName()).toLowerCase();
					if (names.containsKey(key)) {
						String diagramName = names.get(key);
						ret += "Duplicate " + pbv.getClassName() + " block named " + pbv.getName() + " found in diagram " + diagramName + "\r\n";
					} else {
						names.put(key, diagram.getDiagramName());
					}
				}
			
			}
		} else {
			Enumeration<AbstractResourceNavTreeNode> enumer = theNode.children();
			while(enumer.hasMoreElements()) {
				AbstractResourceNavTreeNode aNode = enumer.nextElement();
				ret += parseChildForConflicts(names, aNode);
			}
		}
		return ret;
	}
}
