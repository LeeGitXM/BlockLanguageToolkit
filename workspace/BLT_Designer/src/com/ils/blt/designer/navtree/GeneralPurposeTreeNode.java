/**
 *   (c) 2013-2022  ILS Automation. All rights reserved.
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
import java.util.NoSuchElementException;
import java.util.Optional;

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
import com.ils.blt.common.ApplicationScriptFunctions;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.script.Script;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableFolder;
import com.ils.blt.common.serializable.SerializableNodeRenameHandler;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.ResourceCreateManager;
import com.ils.blt.designer.ResourceDeleteManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.editor.ApplicationPropertyEditor;
import com.ils.blt.designer.editor.FamilyPropertyEditor;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.StringPath;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.ProjectResourceListener;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.FolderNode;
import com.inductiveautomation.ignition.designer.project.DesignableProject;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;
/**
 * Edit a folder in the designer scope to support the diagnostics toolkit diagram
 * layout. In addition to standard folders, folders can be of type "Application" or
 * "Family". These hold properties special to the Diagnostics Toolkit.  Menu options 
 * vary depending on folder type. Labels are likewise dependent.
 * 
 * An OK action calls extension functions.
 * 
 * Leaf nodes are of type DiagramNode. 
 * Note that a FolderNode is an AbstractResourceNavTreeNode
 */
public class GeneralPurposeTreeNode extends FolderNode implements NavTreeNodeInterface, ProjectResourceListener {
	private static final String CLSS = "GeneralPurposeTreeNode";
	public static final String BLT_CUT_OPERATION = "BLTCUT";
	public static final String BLT_COPY_OPERATION = "BLTCOPY";
	private static final int OFFSET = 100;
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final static LoggerEx log = LogUtil.getLogger(GeneralPurposeTreeNode.class.getPackageName());
	private boolean dirty = false;
	private DiagramState state = DiagramState.ACTIVE;  // Used for Applications and Families
	private final DeleteNodeAction deleteNodeAction;
	private final CopyAction copyBranchAction;
	private final PasteAction pasteBranchAction;
	private final StartAction startAction = new StartAction();
	private final StopAction stopAction = new StopAction();
	private final DiagramWorkspace workspace;
	private final SerializableNodeRenameHandler renameHandler;
	private static NodeStatusManager statusManager;
	private final FolderCreateAction folderCreateAction;
	private final ApplicationRequestHandler requestHandler;
	protected final ImageIcon alertBadge;
	private final ImageIcon defaultIcon = IconUtil.getIcon("folder_closed");
	private final ImageIcon openIcon;
	private final ImageIcon closedIcon;
	private final ImageIcon familyIcon;
	private final ImageIcon diagramIcon;

	/** 
	 * Create a new folder node representing the root folder.
	 * @param ctx the designer context
	 */
	public GeneralPurposeTreeNode(DesignerContext ctx) {
		super(ctx, BLTProperties.FOLDER_RESOURCE_TYPE,ApplicationScope.DESIGNER);
		this.setName(BLTProperties.ROOT_FOLDER_NAME);
		this.requestHandler = new ApplicationRequestHandler();
		this.children = null;
		this.childrenLoaded = false;
		this.parent = null;
		deleteNodeAction = null;
		copyBranchAction = new CopyAction(this);
		pasteBranchAction = new PasteAction(this);
		folderCreateAction = new FolderCreateAction(this);
		workspace = ((BLTDesignerHook)ctx.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		renameHandler = new SerializableNodeRenameHandler();
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		setText(BundleUtil.get().getString(PREFIX+".RootFolderName"));
		alertBadge =iconFromPath("Block/icons/badges/bell.png");
		if( context.getProject().isEnabled() ) {
			closedIcon = IconUtil.getIcon("folder_closed");
		}
		else {
			closedIcon = iconFromPath("Block/icons/navtree/disabled_folder.png");  // Project is disabled
		} 
		familyIcon = iconFromPath("Block/icons/navtree/family24.png");  
		diagramIcon = iconFromPath("Block/icons/navtree/diagram.png");  
		setIcon(closedIcon);
		openIcon = IconUtil.getIcon("folder");
		setDirty(true);
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
	public GeneralPurposeTreeNode(DesignerContext context,ProjectResource resource) {
		super(context,resource,ApplicationScope.DESIGNER);
		this.requestHandler = new ApplicationRequestHandler();
		this.renameHandler = new SerializableNodeRenameHandler();
		this.children = null;
		this.childrenLoaded = false;
		this.parent = null;
		setName(resource.getResourceName());      // Also sets text for tree
		deleteNodeAction = new DeleteNodeAction(this);
		copyBranchAction = new CopyAction(this);
		pasteBranchAction = new PasteAction(this);
		folderCreateAction = new FolderCreateAction(this);
		
		workspace = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		alertBadge =iconFromPath("Block/icons/badges/bell.png");
		if(resource.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			closedIcon = iconFromPath("Block/icons/navtree/application_folder_closed.png");
			openIcon = iconFromPath("Block/icons/navtree/application_folder.png");
		} 
		else if(resource.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) { 
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
		setDirty(true);
	}

	// For debugging
	@Override 
	public void checkChildren() {
		if( !isRootFolder() ) {
			log.infof("%s.checkChildren: %s = %d",CLSS,getName(),(children==null?0:this.children.size()));
		}
		super.checkChildren();
	}
	@Override 
	public boolean isLeaf() {
		if( !isRootFolder() ) {
			log.infof("%s.isLeaf: %s = %s ",CLSS,getName(),(this.children==null?"true":"false"));
		}
		return super.isLeaf();
	}
	// walk up the tree and see if there is a 
	public boolean parentIsApplication() {
		boolean ret = false;
		GeneralPurposeTreeNode theNode = this;

		AbstractResourceNavTreeNode node = nearestNonFolderNode(theNode);
		if (node != null && node.getProjectResource() != null && node.getResourceId().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			ret = true;
		}
		return ret;				
	}
	
	protected AbstractResourceNavTreeNode nearestNonFolderNode(GeneralPurposeTreeNode node) {
		AbstractResourceNavTreeNode ret = node;
		while (ret != null && ret.getProjectResource() != null && ret.getResourceId().getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) { // generic folder, go up
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
	
	@Override
	public Icon getIcon() {
		Icon ike  = closedIcon;
		if(statusManager.getAlertState(resourceId)) {
			ike = IconUtil.applyBadge(ike, alertBadge);
		}
		return ike;
	}

	
	@Override
	public String getWorkspaceName() {
		return DiagramWorkspace.key;
	}

	// Use the state for Applications and Families to remember whether to 
	// configure production or isolation databases
	public DiagramState getState() { return this.state; }
	public void setState(DiagramState ds) { 
		this.state = ds;
		statusManager.setResourceState(resourceId, ds);
	}
	/**
	 * Query the block controller in the Gateway. The resources that it knows
	 * about may, or may not, coincide with those in the Designer. 
	 */
	public void listControllerResources() {
		try {
			List <SerializableResourceDescriptor> descriptors = requestHandler.listResourceNodes();
			for( SerializableResourceDescriptor descriptor : descriptors ) {
				log.info("Res: "+descriptor.getName()+" "+descriptor.getResourceId().getProjectName()+":"+descriptor.getResourceId().getResourcePath().getPath().toString()+" "+
						descriptor.getType().toString());
			}
		} 
		catch (Exception ex) {
			log.warnf("%s. startAction: ERROR: %s",CLSS,ex.getMessage(),ex);
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
			if( res.getResourcePath().getModuleId()==null || res.getResourcePath().getModuleId().length()==0) continue;
			if( !res.getResourceType().getTypeId().toString().startsWith("blt")) continue;     // List only diagram resources
			log.info("Res: "+res.getResourceName()+" "+res.getResourceId().getProjectName()+":"+res.getResourceId().getResourcePath().getPath().toString()+" "+
					res.getResourceType().getTypeId().toString());
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
				Optional<ProjectResource> option = child.getProjectResource();
				ProjectResource cres = option.get();
				if( cres.getResourceName().equals(newName)) {
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
	 *  Note: We ignore locking. Previous attempts to use the superior version of this method
	 *        fail to acquire locks (without first doing a project save from the main menu).
	 */
	@Override
	public void onEdit(String newTextValue) {
		// Sanitize name
		if (!isValid(newTextValue) ) {
			ErrorUtil.showError(BundleUtil.get().getString(PREFIX+".InvalidName", newTextValue));
			return;
		}
		String oldName = getResourceId().getResourcePath().getName();
		try {
			log.infof("%s.onEdit: alterName from %s to %s",CLSS,oldName,newTextValue);
			alterName(newTextValue);
			workspace.saveOpenDiagram(resourceId);
		}
		catch (IllegalArgumentException ex) {
			ErrorUtil.showError(CLSS+".onEdit: "+ex.getMessage());
		}
	}
	
	// If an application or family is selected, then display editing panel in the PropertyEditor
	@Override
	public synchronized void onSelected() {
		UndoManager.getInstance().setSelectedContext(GeneralPurposeTreeNode.class);
		Optional<ProjectResource> optional = context.getProject().getResource(resourceId);
		ProjectResource resource = null;
		try {
			resource = optional.get();
		}
		catch(NoSuchElementException nsee) {
			log.infof("%s.onSelected: no such element (%s)",CLSS,nsee.getMessage());
		}
		if( resource==null || resource.getResourceId()==null ||resource.getResourceId().getResourceType()==null ) {
			log.infof("%s.onSelected:(%s) - null resource",CLSS,getName());
		}
		else if(resource.getResourceId().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			SerializableApplication sap = recursivelyDeserializeApplication(this);
			log.infof("%s.onSelected: selected application %s (%d)",CLSS,sap.getName(),resourceId);
			ApplicationPropertyEditor appEditor = new ApplicationPropertyEditor(context,sap,resource);
			workspace.getPropertyEditorFrame().setEditor(appEditor) ;
		} 
		else if(resource.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
			SerializableFamily sfam = recursivelyDeserializeFamily(this);
			log.infof("%s.onSelected: selected family %s (%d)",CLSS,sfam.getName(),resourceId);
			FamilyPropertyEditor famEditor = new FamilyPropertyEditor(context,sfam,resource);
			workspace.getPropertyEditorFrame().setEditor(famEditor) ;
		}
		else {
			log.infof("%s.onSelected:(%s)",CLSS,getName());
		}
	}

	@Override
	public void setName(String name) {
		/*
		if( this.resourceId !=null && name!=null ) {
			try {
				context.getProject().renameResource(getResourceId(), name);
			}
			catch(Exception ex) {
				log.warnf("%s.setName: %s resource naming exception",CLSS,name);
			}
		}
		*/
		super.setName(name);
	}
	/**
	 * Exclude cut and paste which are currently not supported.
	 */
	@Override
	protected void addEditActions(JPopupMenu menu) {
		menu.add(renameAction);
		menu.add(deleteNodeAction);
	}
	@Override 
	public List<AbstractNavTreeNode> loadChildren() {
		log.infof("%s.loadChildren: %s ..................",CLSS,getName());
		List<AbstractNavTreeNode> kids = new ArrayList<>();
		List<ProjectResource> resources = context.getProject().getResources();
		if( isRootFolder()) {
			// Search for children of root
			for(ProjectResource pr:resources) {
				if(isRootChild(pr)) {
					log.infof("%s.loadChildren: root child = %s [%s] (%s)",CLSS,pr.getResourcePath().getPath().toString(),
							pr.getResourcePath().getParentPath(),pr.getResourcePath().getResourceType().toString());
					kids.add(createChildNode(pr));
				}
			}
		}
		else {
			// Search for children of this node
			for(ProjectResource pr:resources) {
				log.infof("%s.loadChildren: resource %s vs %s",CLSS,this.resourceId.getResourcePath().getPath().toString(),
						pr.getResourcePath().getParentPath());
				if(isChildNode(pr)) {
					kids.add(createChildNode(pr));
				}
			}
		}
		return kids;
	}
	/**
	 * Create a child node because we've discovered a resource that matches this instance as a parent
	 * based on its content matching the our UUID. If the node had been previously created, then 
	 * reuse it.
	 */
	@Override
	protected AbstractNavTreeNode createChildNode(ProjectResource res) {
		ResourceType rtype = res.getResourceType();
		log.infof("%s.createChildNode: %s (%s) type:%s, depth=%d", CLSS,getName(),res.getResourcePath().getPath().toString(),rtype,getDepth());
		// If the project is disabled, then don't do anything
		if( !context.getProject().isEnabled()) return null;
		AbstractResourceNavTreeNode node = statusManager.findNode(res.getResourceId());
		if( node==null ) {
			if (    BLTProperties.FOLDER_RESOURCE_TYPE.equals(rtype) )       {
				node = new GeneralPurposeTreeNode(context, res);
				log.tracef("%s.createChildNode: (%s) %s->%s",CLSS,rtype,this.getName(),node.getName());
			}
			else if ( BLTProperties.APPLICATION_RESOURCE_TYPE.equals(rtype) )       {
				SerializableApplication sa = deserializeApplication(res);
				node = new GeneralPurposeTreeNode(context, res);
				log.tracef("%s.createChildNode: (%s) %s->%s",CLSS,rtype,this.getName(),node.getName());
			}
			else if ( BLTProperties.FAMILY_RESOURCE_TYPE.equals(rtype) )       {
				SerializableFamily fa = deserializeFamily(res); 
				node = new GeneralPurposeTreeNode(context, res);
				log.tracef("%s.createChildNode: (%s) %s->%s",CLSS,rtype,this.getName(),node.getName());
			}
			else if (BLTProperties.DIAGRAM_RESOURCE_TYPE.equals(rtype) ) {
				node = new DiagramTreeNode(context,res,workspace);
				log.tracef("%s.createChildDiagram: %s->%s",CLSS,this.getName(),node.getName());
			} 
			else {
				String msg = String.format("%s: Attempted to create a child of type %s (ignored)",CLSS,rtype);
				log.warn(msg);
				throw new IllegalArgumentException(msg);
			}
			statusManager.createResourceStatus(node,resourceId, res.getResourceId());
		}
		else {
			log.debugf("%s.createChildNode: REUSE %s->%s",CLSS,this.getName(),node.getName());
			if( node instanceof DiagramTreeNode ) context.getProject().addProjectResourceListener((DiagramTreeNode)node);
		}
		node.install(this);
		if( node.getParent()==null) {
			log.errorf("%s.createChildNode: ERROR parent is null %s(%d)",CLSS,node.getName(),res.getResourceId());
		}
		return node;
	}

	@Override
	public void expand() { 
		if( !isRootFolder() ) {
			log.infof("%s.expand: %s .....",CLSS,this.getName());
		}
		super.recreate(); 
	}
	// For DiagramNode.delete
	@Override
	public void recreate() { 
		if( !isRootFolder() ) {
			log.infof("%s.recreate: %s .....",CLSS,this.getName());
		}
		super.recreate(); 
	}
	@Override
	public void reload() { 
		if( !isRootFolder() ) {
			log.infof("%s.reload: %s .....",CLSS,this.getName());
		}
		super.reload(); 
	}
	@Override
	public void refresh() { 
		if( !isRootFolder() ) {
			log.infof("%s.refresh: %s .....",CLSS,this.getName());
		}
		super.refresh(); 
	}

	/**
	 * Define the menu used for popups. This appears to be called each time a menu is called for ..
	 */
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		if( this.getParent()==null ) {
			log.errorf("%s.initPopupMenu: ERROR: Diagram (%d) has no parent",CLSS,hashCode());
		}
		context.getProject().addProjectResourceListener(this);
		if (isRootFolder()) { 
			if( context.getProject().isEnabled()) {
				ApplicationCreateAction applicationCreateAction = new ApplicationCreateAction(this);
				ApplicationImportAction applicationImportAction = new ApplicationImportAction(context.getFrame(),this);
				ClearAction clearAction = new ClearAction();
				DebugAction debugAction = new DebugAction();
				SynchronizeAction synchronizeAction = new SynchronizeAction(this);
				if( requestHandler.isControllerRunning() ) {
					startAction.setEnabled(false);
					clearAction.setEnabled(false);
				} else {
					stopAction.setEnabled(false);
					clearAction.setEnabled(true);
				}
				menu.add(applicationCreateAction);
				menu.add(applicationImportAction);
				menu.add(folderCreateAction);
				menu.add(startAction);
				menu.add(stopAction);
				menu.add(pasteBranchAction);
				menu.addSeparator();
				menu.add(clearAction);
				menu.add(debugAction);
			}
		}
		else if( getProjectResource()==null ) {
			log.warnf("%s.initPopupMenu: ERROR: node %s(%d) has no project resource",CLSS,this.getName(),resourceId);
		}
		else if(getResourcePath().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			ApplicationExportAction applicationExportAction = new ApplicationExportAction(menu.getRootPane(),this);
			FamilyCreateAction familyAction = new FamilyCreateAction(this);
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
			//menu.add(refreshAction);
			menu.addSeparator();
			menu.add(copyBranchAction);
			menu.add(pasteBranchAction);
			menu.add(applicationExportAction);
			addEditActions(menu);
		}
		else if(getResourcePath().getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
			DiagramCreateAction newDiagramAction = new DiagramCreateAction(this);


			ImportDiagramAction importAction = new ImportDiagramAction(menu.getRootPane(),this);
			menu.add(newDiagramAction);
			menu.add(importAction);
			menu.add(folderCreateAction);
			menu.addSeparator();
			menu.add(copyBranchAction);
			menu.add(pasteBranchAction);;
			addEditActions(menu);

		}
		else if(getResourcePath().getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
			
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
			menu.addSeparator();

			if( true /*something is selected*/ ) {
				copyBranchAction.setEnabled(true);
			} 
			else {
				copyBranchAction.setEnabled(false);
			}
			if( true /*something is in the clipboard*/ ) {
				pasteBranchAction.setEnabled(true);
			} 
			else {
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
	public SerializableApplication deserializeApplication(ProjectResource res) {
		SerializableApplication sa = null;
		try{
			byte[] bytes = res.getData();
			ObjectMapper mapper = new ObjectMapper();
			sa = mapper.readValue(new String(bytes), SerializableApplication.class);
			sa.setName(res.getResourceName());   // Sync the SerializableApplication name w/ res
		}
		catch(Exception ex) {
			log.warnf("%s.deserializeApplication: Deserialization exception (%s)",CLSS,ex.getMessage());
		}
		return sa;
	}

	/**
	 * Convert the resource data into a deserializeDiagram.
	 * Set the state from that stored in the NotificationStatusManager
	 * @param res
	 * @return
	 */
	public static SerializableDiagram deserializeDiagram(ProjectResource res) {
		SerializableDiagram sd = null;
		try{
			byte[] bytes = res.getData();
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
			sd.setName(res.getResourceName());   // Sync the SerializableDiagram name w/ res
			statusManager.setResourceState(res.getResourceId(), sd.getState());
		}
		catch(Exception ex) {
			log.warnf("%s.SerializableDiagram: Deserialization exception (%s)",CLSS,ex.getMessage());
		}
		return sd;
	}

	/**
	 * Convert the resource data into a SerializableFamily
	 * @param res
	 * @return
	 */
	public SerializableFamily deserializeFamily(ProjectResource res) {
		SerializableFamily sf = null;
		try{
			byte[] bytes = res.getData();
			ObjectMapper mapper = new ObjectMapper();
			sf = mapper.readValue(new String(bytes), SerializableFamily.class);
			sf.setName(res.getResourceName());  // ???
		}
		catch(Exception ex) {
			log.warnf("%s.deserializeFamily: Deserialization exception (%s)",CLSS,ex.getMessage());
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
				else if( node.getResourcePath().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
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
				else if( node.getResourcePath().getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
					answer = true;
					break;
				}
				else if( node.getResourcePath().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
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
		return getName().equals(BLTProperties.ROOT_FOLDER_NAME);
	}
/*
 *   WARNING: When an application is serialized, all resources are nested and serialized as such.
 *            A project is saved differently. All resources are saved separately in a flat namespace.
 */            
	// Recursively descend the node tree, gathering up associated resources.
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to an application.
	private SerializableApplication recursivelyDeserializeApplication(AbstractResourceNavTreeNode node) {
		Optional<ProjectResource>optional = node.getProjectResource();
		ProjectResource res = optional.get();
		SerializableApplication sa = null;
		if( res!=null ) {
			log.infof("%s.recursivelyDeserializeApplication: %s (%s)",CLSS,res.getResourceName(),res.getResourceId().getResourcePath().getPath().toString());
			sa = deserializeApplication(res);
			sa.setFamilies(new SerializableFamily[0]);
			sa.setFolders(new SerializableFolder[0]);

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				ProjectResourceId rid = child.getResourceId();
				if(rid.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)){
					SerializableFamily sfam = recursivelyDeserializeFamily(child);
					if( sfam!=null ) sa.addFamily(sfam);
				}
				else if(rid.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
					SerializableFolder sf = recursivelyDeserializeFolder(child);
					if( sf!=null ) sa.addFolder(sf);
				}
				else {
					log.warnf("%s.recursivelyDeserializeApplication: %s unexpected child resource type (%s)",CLSS,res.getResourceName(),rid.getResourceType().getTypeId());
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
		Optional<ProjectResource>optional = node.getProjectResource();
		ProjectResource res = optional.get();
		SerializableDiagram sdiag = null;
		if( res!=null ) {
			log.infof("%s.recursivelyDeserializeDiagram: %s (%d)",CLSS,res.getProjectName(),res.getResourceId());
			sdiag = deserializeDiagram(res);
		}
		return sdiag;
	}
	// Recursively descend the node tree, gathering up associated resources.
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to an application.
	private SerializableFamily recursivelyDeserializeFamily(AbstractResourceNavTreeNode node) {
		Optional<ProjectResource>optional = node.getProjectResource();
		ProjectResource res = optional.get();
		SerializableFamily sfam = null;
		if( res!=null ) {
			log.infof("%s.recursivelyDeserializeFamily: %s (%d)",CLSS,res.getResourceName(),res.getResourceId());
			sfam = deserializeFamily(res);
			sfam.setFolders(new SerializableFolder[0]);
			sfam.setDiagrams(new SerializableDiagram[0]);

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				ProjectResourceId rid = child.getResourceId();
				if( rid.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
					SerializableDiagram sd = recursivelyDeserializeDiagram(child);
					if( sd!=null ) sfam.addDiagram(sd);
				}
				else if(rid.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
					SerializableFolder sf = recursivelyDeserializeFolder(child);
					if( sf!=null ) sfam.addFolder(sf);
				}
				else {
					log.warnf("%s.recursivelyDeserializeFamily: %s unexpected child resource type (%s)",CLSS,res.getResourceName(),
							rid.getResourceType().toString());
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
		Optional<ProjectResource>optional = node.getProjectResource();
		ProjectResource res = optional.get();
		SerializableFolder sfold = null;
		if( res!=null ) {
			log.infof("%s.recursivelyDeserializeFolder: %s (%s)",CLSS,res.getResourceName(),res.getResourceId().getResourcePath().getPath().toString());
			sfold = new SerializableFolder();
			sfold.setName(res.getResourceName());
			sfold.setParentPath(StringPath.parse(res.getFolderPath()));
			sfold.setFolders(new SerializableFolder[0]);
			sfold.setDiagrams(new SerializableDiagram[0]);

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				optional = child.getProjectResource();
				ProjectResourceId rid = child.getResourceId();
				if( rid.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
					SerializableDiagram sd = recursivelyDeserializeDiagram(child);
					if( sd!=null ) sfold.addDiagram(sd);
				}
				else if(rid.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)){
					SerializableFamily sfam = recursivelyDeserializeFamily(child);
					if( sfam!=null ) sfold.addFamily(sfam);
				}
				else if(rid.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
					SerializableFolder sf = recursivelyDeserializeFolder(child);
					if( sf!=null ) sfold.addFolder(sf);
				}
				else {
					log.infof("%s.recursivelyDeserializeFolder: %s unexpected child resource type (%s)",CLSS,res.getResourceName(),rid.getResourceType().getTypeId());
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
		log.debugf("%s: serializeApplication creating json ... %s",CLSS,(mapper.canSerialize(SerializableApplication.class)?"true":"false"));
		try{ 
			json = mapper.writeValueAsString(application);
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s: Unable to serialize application (%s)",CLSS,jpe.getMessage());
		}
		log.debugf("%s: serializeApplication created json ... %s",CLSS,json);
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
		log.debugf("%s: serializeDiagram creating json ... %s",CLSS,(mapper.canSerialize(SerializableDiagram.class)?"true":"false"));
		try{ 
			json = mapper.writeValueAsString(diagram);
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s: Unable to serialize diagram (%s)",CLSS,jpe.getMessage());
		}
		log.infof("%s: serializeDiagram created json ... %s",CLSS,json);
		return json;
	}

	/**
	 *  Serialize a Family into JSON.
	 * @param family to be serialized
	 */ 
	private String serializeFamily(SerializableFamily family) {
		String json = "";
		ObjectMapper mapper = new ObjectMapper();
		log.debugf("%s: serializeFamily creating json ... %s",CLSS,(mapper.canSerialize(SerializableFamily.class)?"true":"false"));
		try{ 
			json = mapper.writeValueAsString(family);
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s: Unable to serialize family (%s)",CLSS,jpe.getMessage());
		}
		log.debugf("%s: serializeFamily created json ... %s",CLSS,json);
		return json;
	}

	// ============================================= private action classes ===========================================================
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
				String newName = BundleUtil.get().getString(PREFIX+".NewApplication.Default.Name");
				if( newName==null) newName = "New App";  // Missing Resource
				SerializableApplication app = new SerializableApplication();
				app.setName(newName);
				Optional<ProjectResource> ores = getProjectResource();
				ProjectResource pr = ores.get();
				String json = serializeApplication(app);
				log.debugf("%s.ApplicationCreateAction. json=%s",CLSS,json);
				byte[] bytes = json.getBytes();
				log.infof("%s.ApplicationCreateAction. create %s(%s),(%d bytes)",CLSS,newName,BLTProperties.APPLICATION_RESOURCE_TYPE.toString(),bytes.length);
				ProjectResourceId resid = requestHandler.createResourceId(pr.getProjectName(), getResourceId().getResourcePath().getFolderPath(), 
						BLTProperties.APPLICATION_RESOURCE_TYPE);
				ProjectResourceBuilder builder = ProjectResource.newBuilder();
				builder.setResourceId(resid);
				builder.setVersion(pr.getVersion()+1);
				builder.setApplicationScope(ApplicationScope.GATEWAY);
				new ResourceCreateManager(builder.build(),newName).run();	// Must be synchronous for child to show
				currentNode.selectChild(new ResourcePath[] {getResourceId().getResourcePath()});
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception creating application",err);
			}
		}
	}
	// Note: At the time of export the application is assumed to be completely up-to-date, including with external databases.
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
							log.debugf("%s.actionPerformed: dialog returned %s",CLSS,output.getAbsolutePath());
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
									if(log.isDebugEnabled()) log.debugf("%s.actionPerformed: creating json ... %s",CLSS,(mapper.canSerialize(SerializableApplication.class)?"true":"false"));
									try{ 
										// Convert the view into a serializable object. Here we reject any nesting that might
										// have been saved in the project resource, and substitute what we know from the nav tree.

										//  executionEngine.executeOnce(new AuxiliaryDataRestoreManager(workspace,node));  nope.
										
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
				ErrorUtil.showError(CLSS+" Exception exporting application",err);
			}
		}
	}

	
	public void restoreAuxData(SerializableApplication sap) {
		GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();
		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", sap.getName());   // Use as a key when fetching
		String db       = (sap.getState().equals(DiagramState.ISOLATED)?requestHandler.getProjectIsolationDatabase(context.getProjectName()):requestHandler.getProjectProductionDatabase(context.getProjectName()));
		String provider = (sap.getState().equals(DiagramState.ISOLATED)?requestHandler.getProjectIsolationTagProvider(context.getProjectName()):requestHandler.getProjectProductionTagProvider(context.getProjectName()));
		ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
		Script script = extensionManager.createExtensionScript(ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.GET_AUX_OPERATION, provider);
		extensionManager.runScript(context.getScriptManager(), script, sap.getPath().toString(),auxData,db);
		sap.setAuxiliaryData(auxData);

		for (SerializableFamily sf: sap.getFamilies()) {
			restoreAuxData(sf);
		}
	}
	
	public void restoreAuxData(SerializableFamily sf) {
		
		GeneralPurposeDataContainer auxData = new GeneralPurposeDataContainer();
		auxData.setProperties(new HashMap<String,String>());
		auxData.setLists(new HashMap<>());
		auxData.setMapLists(new HashMap<>());
		auxData.getProperties().put("Name", sf.getName());   // Use as a key when fetching
	
		String db       = (sf.getState().equals(DiagramState.ISOLATED)?requestHandler.getProjectIsolationDatabase(context.getProjectName()):requestHandler.getProjectProductionDatabase(context.getProjectName()));
		String provider = (sf.getState().equals(DiagramState.ISOLATED)?requestHandler.getProjectIsolationTagProvider(context.getProjectName()):requestHandler.getProjectProductionTagProvider(context.getProjectName()));
		ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
		Script script = extensionManager.createExtensionScript(ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.GET_AUX_OPERATION, provider);
		extensionManager.runScript(context.getScriptManager(), script, sf.getPath().toString(),auxData,db);
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
										// Unfortunately we have to replace all paths with new ones
										ObjectMapper mapper = new ObjectMapper();
										SerializableApplication sa = mapper.readValue(new String(bytes), SerializableApplication.class);
										if( sa!=null ) {
											renameHandler.convertPaths(sa,getResourceId().getResourcePath().getPath());
											log.infof("%s.s:ApplicationImportAction. create %s(%s),(%d bytes)",CLSS,sa.getName(),BLTProperties.APPLICATION_RESOURCE_TYPE.toString(),bytes.length);
											String json = mapper.writeValueAsString(sa);
											if(log.isTraceEnabled() ) log.trace(json);
											ProjectResourceId resid = requestHandler.createResourceId(getResourceId().getProjectName(), sa.getPath().toString(), BLTProperties.APPLICATION_RESOURCE_TYPE);
											ProjectResourceBuilder builder = ProjectResource.newBuilder();
											builder.putData(json.getBytes());
											builder.setResourceId(resid);
											builder.setApplicationScope(ApplicationScope.GATEWAY);
											builder.setVersion(0);
											ProjectResource resource = builder.build();

											// Now import families
											for(SerializableFamily fam:sa.getFamilies()) {
												importFamily(resid.getResourcePath().getPath(),fam);
											}
											// Create after the children -- else sometimes folders are not populated.
											new ResourceCreateManager(resource,resource.getResourceName()).run();   // In-line
											saveApplicationAuxData(sa);
											
											root.selectChild(new ResourcePath[] {resid.getResourcePath()} );
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
										log.infof("%s:ApplicationImportAction reloading (%s)", CLSS,kid.getName());
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
			ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
			String db       = (sa.getState().equals(DiagramState.ISOLATED)?requestHandler.getProjectIsolationDatabase(context.getProjectName()):requestHandler.getProjectProductionDatabase(context.getProjectName()));
			String provider = (sa.getState().equals(DiagramState.ISOLATED)?requestHandler.getProjectIsolationTagProvider(context.getProjectName()):requestHandler.getProjectProductionTagProvider(context.getProjectName()));
			Script script = extensionManager.createExtensionScript(ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.GET_AUX_OPERATION, provider);
			extensionManager.runScript(context.getScriptManager(), script, sa.getPath().toString(),sa.getAuxiliaryData(),db);

		}


		// Run in foreground to avoid synchronization issues with display.
		private synchronized void importDiagram(StringPath parentPath,SerializableDiagram sd) {
			ObjectMapper mapper = new ObjectMapper();
			try{
				sd.setState(DiagramState.DISABLED);
				sd.setDirty(true);

				byte[] bytes = mapper.writeValueAsBytes(sd);
				if(log.isTraceEnabled() ) log.trace(bytes.toString());
				ProjectResourceId resid = requestHandler.createResourceId(getResourceId().getProjectName(), sd.getPath().toString(), BLTProperties.DIAGRAM_RESOURCE_TYPE);
				ProjectResourceBuilder builder = ProjectResource.newBuilder();
				builder.putData(bytes);
				builder.setResourceId(resid);
				builder.setApplicationScope(ApplicationScope.GATEWAY);
				builder.setVersion(0);
				log.infof("%s.s:ApplicationImportAction. create %s(%s),(%d bytes)",CLSS,sd.getName(),BLTProperties.APPLICATION_RESOURCE_TYPE.toString(),bytes.length);
				statusManager.setResourceState(resid, sd.getState());
				ProjectResource pr = builder.build();
				new ResourceCreateManager(pr,pr.getResourceName()).run();
			} 
			catch (Exception ex) {
				ErrorUtil.showError(String.format("ApplicationImportAction: importing diagrm, unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
			}
		}
		// Run in foreground to avoid synchronization issues with display.
		private synchronized void importFamily(StringPath parentPath,SerializableFamily sf) {
			ObjectMapper mapper = new ObjectMapper();
			try{
				String json = mapper.writeValueAsString(sf);
				if(log.isTraceEnabled() ) log.trace(json);
				ProjectResourceId resid = requestHandler.createResourceId(getResourceId().getProjectName(), sf.getPath().toString(), BLTProperties.FAMILY_RESOURCE_TYPE);
				ProjectResourceBuilder builder = ProjectResource.newBuilder();
				builder.putData(json.getBytes());
				builder.setResourceId(resid);
				builder.setApplicationScope(ApplicationScope.GATEWAY);
				builder.setVersion(0);
				ProjectResource resource = builder.build();
				log.infof("%s.s:FamilyImportAction. create %s(%s),(%d bytes)",CLSS,sf.getName(),BLTProperties.FAMILY_RESOURCE_TYPE.toString(),resource.getData().length);
				new ResourceCreateManager(resource,resource.getResourceName()).run();   // in-line
				// Now import the diagrams
				for(SerializableDiagram diagram:sf.getDiagrams()) {
					importDiagram(sf.getParentPath(),diagram);
				}
			} 
			catch (Exception ex) {
				ErrorUtil.showError(String.format("ApplicationImportAction: Unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
			}
		}
	}
	// Called during paste
	protected boolean copyChildren(AbstractResourceNavTreeNode fromNode) throws Exception {
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);

		if (fromNode == null) {
			ErrorUtil.showWarning("copy children with NULL node!");
			return false;
		}
		// Iterate over the children of the original node
		@SuppressWarnings("rawtypes")
		Enumeration walker = fromNode.children();
		while(walker.hasMoreElements()) {
			AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
			Optional<ProjectResource> optional = child.getProjectResource();
			ProjectResource res = optional.get();
			byte[] bytes = res.getData();
			String json = "";
			ProjectResourceId resid = null;

			if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
				SerializableApplication sa = mapper.readValue(new String(bytes), SerializableApplication.class);
				if( sa!=null ) {
					renameHandler.convertPaths(sa,res.getResourcePath().getPath());
					json = mapper.writeValueAsString(sa);
					resid = requestHandler.createResourceId(getResourceId().getProjectName(), sa.getPath().toString(), BLTProperties.APPLICATION_RESOURCE_TYPE);
				}
				else {
					ErrorUtil.showWarning(String.format("Failed to deserialize child application (%s)",res.getResourceName()),"Copy Application");
					return false;
				}
			}
			else if( res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {

				SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
				if( sd!=null ) {
					renameHandler.convertPaths(sd,res.getResourcePath().getPath());
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
					json = mapper.writeValueAsString(sd);
					resid = requestHandler.createResourceId(getResourceId().getProjectName(), sd.getPath().toString(), BLTProperties.DIAGRAM_RESOURCE_TYPE);
				}
				else {
					ErrorUtil.showWarning(String.format("Failed to deserialize child diagram (%s)",res.getResourceName()),"Copy Diagram");
					return false;
				}
			}
			else if( res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
				SerializableFamily sf = mapper.readValue(new String(bytes), SerializableFamily.class);
				if( sf!=null ) {
					renameHandler.convertPaths(sf,res.getResourcePath().getPath());
					json = mapper.writeValueAsString(sf);
					resid = requestHandler.createResourceId(getResourceId().getProjectName(), sf.getPath().toString(), BLTProperties.FAMILY_RESOURCE_TYPE);
				}
				else {
					ErrorUtil.showWarning(String.format("Failed to deserialize child family (%s)",res.getResourceName()),"Copy Family");
					return false;
				}
			}
			else if( res.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
				SerializableFolder sf = mapper.readValue(new String(bytes), SerializableFolder.class);
				if( sf!=null ) {
					renameHandler.convertPaths(sf,res.getResourcePath().getPath());;
					json = mapper.writeValueAsString(sf);
					resid = requestHandler.createResourceId(getResourceId().getProjectName(), sf.getPath().toString(), BLTProperties.FOLDER_RESOURCE_TYPE);
				}
				else {
					ErrorUtil.showWarning(String.format("Failed to deserialize child folder (%s)",res.getResourceName()),"Copy Folder");
					return false;
				}
			}
			else {
				ErrorUtil.showWarning(String.format("Unexpected child resource type(%s)",res.getResourceType()),"Copy Node");
				return false;
			}
			ProjectResourceBuilder builder = ProjectResource.newBuilder();
			builder.putData(json.getBytes());
			builder.setResourceId(resid);
			builder.setApplicationScope(ApplicationScope.GATEWAY);
			builder.setVersion(0);
			ProjectResource resource = builder.build();

			new ResourceCreateManager(resource,resource.getResourceName()).run();
			if (!copyChildren(child)) {
				return false;
			}
		}
		return true;
	} 

		
	// From the root node, tell the gateway controller to clear all resources
	private class ClearAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public ClearAction()  {
			super(PREFIX+".Clear",IconUtil.getIcon("delete_all"));
		}

		public void actionPerformed(ActionEvent e) {
			requestHandler.clearController();
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

           Optional<ProjectResource> optional = parentNode.getProjectResource();
           ProjectResource res = optional.get();
           String data = ""+res.getResourceId();
           Transferable t =  new StringSelection(BLT_COPY_OPERATION + data);
				   
		   if (t != null) {
			   try { 
				   clipboard.setContents(t, null); 
			   } catch (Exception ex) {
					log.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
			   }
		   }
				   
		}
	}

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
			try {
				context.getProject().deleteResource(pasted.getResourceId());
			}
			catch(ResourceNotFoundException rnfe) {
				log.warnf("%s: undo: Resource not found (%s)",CLSS,rnfe.getMessage());
			}
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
					log.errorf("%s: actionPerformed: Unhandled Exception in PASTE (%s)",CLSS,ex.getMessage());
					ex.printStackTrace();
     			}
     		}
     		return result;
		}



		public void actionPerformed(ActionEvent e) {
			execute();
		}	
		
		// The paste string represents a resourceID of a new resource/nav tree node
		public void paste(String data, boolean deleteOriginal) {
			ObjectMapper mapper = new ObjectMapper();
			ProjectResourceId clipId = null;
			try {
				clipId = (ProjectResourceId)mapper.readValue(data, ProjectResourceId.class);
			
			}
			catch(JsonParseException jpe) {
				log.warnf("%s: paste: Parse exception of paste object (%s)",CLSS,jpe.getMessage());
				return;
			}
			catch(JsonMappingException jme) {
				log.warnf("%s: paste: Json mapping exception of paste object (%s)",CLSS,jme.getMessage());
				return;
			}
			catch(IOException ioe) {
				log.warnf("%s: paste: IO exception of paste object (%s)",CLSS,ioe.getMessage());
				return;
			}
//			*Note cut disabled for now.  Just copy & delete	
			Optional<ProjectResource> optional = context.getProject().getResource(clipId);
			ProjectResource res = optional.get();
	        optional = parentNode.getProjectResource();
	        ProjectResource dst = optional.get();
	        String newName = nextFreeName((GeneralPurposeTreeNode)parentNode,res.getResourceName());
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						try {	
							ObjectMapper mapper = new ObjectMapper();
							mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
							SerializableDiagram sd = null;

							String json = "";
							SerializableApplication sa = null; 
							SerializableFamily sf = null;
		
							if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
								 AbstractResourceNavTreeNode closest = nearestNonFolderNode(parentNode);
								if (closest instanceof GeneralPurposeTreeNode && ((GeneralPurposeTreeNode)closest).isRootFolder()) {
									sa = mapper.readValue(new String(res.getData()), SerializableApplication.class);
									sa.setName(newName);
									if( sa!=null ) {
										renameHandler.convertPaths(sa,res.getResourcePath().getPath());
										json = mapper.writeValueAsString(sa);
									} 
									else {
										ErrorUtil.showWarning(String.format("Failed to deserialize application (%s)",res.getResourceName()),"Paste Application");
										return;
									}
								} 
								else {
									ErrorUtil.showWarning("Tried to paste an application into an invalid location","Paste Application");
									return;
								}
							}
							else if(res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
								AbstractResourceNavTreeNode node = nearestNonFolderNode(parentNode);
								if (node.getProjectResource() != null && node.getResourcePath().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
									sf = mapper.readValue(new String(res.getData()), SerializableFamily.class);
									sf.setName(newName);
									if( sf!=null ) {
										renameHandler.convertPaths(sf,res.getResourcePath().getPath());  // this goes deep but doesn't matter.  just getting a new UUID
										json = mapper.writeValueAsString(sf);
									}
									else {
										ErrorUtil.showWarning(String.format("Failed to deserialize family (%s)",res.getResourceName()),"Paste Family");
										return;
									}
								} 
								else {
									ErrorUtil.showWarning("Tried to paste a family into an invalid location","Paste Family");
									return;
								}
							} 
							else if( res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
								AbstractResourceNavTreeNode node = nearestNonFolderNode(parentNode);
								if( (node.getProjectResource() != null && 
									 node.getResourcePath().getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) ||
									parentNode.getResourcePath().getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)   ) {
									sd = mapper.readValue(new String(res.getData()), SerializableDiagram.class);
									if( sd!=null ) {
										ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
										renameHandler.convertPaths(sd,res.getResourcePath().getPath());  // converted UUIDs are thrown away because later CopyChildren also does it
										sd.setName(newName);
										for( Block blk:diagram.getBlocks()) {
											ProcessBlockView pbv = (ProcessBlockView)blk;
											if (pbv.isDiagnosis()) {
												renameDiagnosis(sd, pbv);
												continue;
											}
										}										
										sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
										sd.setState(DiagramState.DISABLED);
										json = mapper.writeValueAsString(sd);
										statusManager.setResourceState(node.getResourceId(), sd.getState());

									}
									else {
										ErrorUtil.showWarning(String.format("Failed to deserialize diagram (%s)",res.getResourceName()),"Paste Diagram");
										return;
									}
								} 
								else {
									ErrorUtil.showWarning("Tried to paste a diagram into an invalid location","Paste Diagram");
									return;
								}

							} 
							else {
								ErrorUtil.showWarning(String.format("Unexpected resource type(%s)",res.getResourceType()),"Paste Node");
								return;
							}
							// res is the project resource to copy - we just have to change the name
							ProjectResourceId oldId = res.getResourceId();
							ProjectResourceId newId = requestHandler.createResourceId(oldId.getProjectName(),
									oldId.getFolderPath()+"/"+name, oldId.getResourceType());
							
							ProjectResourceBuilder builder = ProjectResource.newBuilder();
							builder.setResourceId(newId);;
							builder.putData(res.getData());
							builder.setVersion(res.getVersion());
							builder.setApplicationScope(res.getApplicationScope());

							
							AbstractResourceNavTreeNode node = statusManager.findNode(res.getResourceId());  // so basically starting over here
							ProjectResource resource = builder.build();
//							Copies children and assigns new ResourcePaths
							if (copyChildren(node) && deleteOriginal) { // copy children will rename diagnosis blocks
		    					ResourceDeleteManager deleter;
		    					deleter = new ResourceDeleteManager(node);
		    					deleter.acquireResourcesToDelete();
		    					deleter.run();
							}
							
							// Finally display the parent node
							new ResourceCreateManager(resource,resource.getResourceName()).run();
							AbstractResourceNavTreeNode newNode = statusManager.findNode(resource.getResourceId());  // so basically starting over here

							UndoManager.getInstance().add(PasteAction.this,AbstractNavTreeNode.class);
							pasted = resource;
							((GeneralPurposeTreeNode)parentNode).selectChild(new ResourcePath[] {resource.getResourcePath().getParent()} );
							
						}
						catch( IOException ioe) {
							// Should never happen, we just picked this off a chooser
							log.warnf("%s: actionPerformed, IOException(%s)",CLSS,ioe.getLocalizedMessage()); 
							ioe.printStackTrace();
						}
						catch (Exception ex) {
							log.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
							ex.printStackTrace();
						}
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception cloning diagram",err);
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
			log.info("============================ BLT Resources (Designer) =========================");
			listProjectBLTResources();
			log.info("============================ BLT Resources (Gateway)  =========================");
			listControllerResources();
			log.infof("================================ (proj = %s )==============================",context.getProject().getName());
		}
	}

	// Delete the this node and all its descendants. 
	// Note: On a "save" action, the descendants are removed also.
	private class DeleteNodeAction extends BaseAction implements UndoManager.UndoAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode node; 
		ResourceDeleteManager deleter;
		private String bundleString;
		private ProjectResourceId resid = null;    // for the root

		public DeleteNodeAction(AbstractResourceNavTreeNode resourceNode)  {
			super(PREFIX+".DeleteNode",IconUtil.getIcon("delete"));
			this.node = resourceNode;
			this.bundleString = PREFIX+".NodeNoun";
			this.deleter = new ResourceDeleteManager(node);
		}

		public void actionPerformed(ActionEvent e) {
			AbstractNavTreeNode p = node.getParent();
			Optional<ProjectResource> optional = node.getProjectResource();
			ProjectResource res = optional.get();
			resid = res.getResourceId();
			log.infof("%s.DeleteNodeAction: %s, resource %d.",CLSS,node.getName(),resid);
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
					deleter.run();
				}
				else {
					ErrorUtil.showError("Node locked, delete failed");
				}
			}
		}

		// Marks the project resources for deletion.
		@Override
		public boolean execute() {
			deleter.run();
			return true;
		}

		@Override
		public boolean isGroupSequenceIndependent() {return false;}

		@Override
		public boolean undo() {
			deleter.run();
			return true;
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
				String newName = BundleUtil.get().getString(PREFIX+".NewDiagram.Default.Name");
				if( newName==null) newName = "New Diagram";  // Missing string resource
				SerializableDiagram diagram = new SerializableDiagram();
				diagram.setName(newName);
				diagram.setParentPath(StringPath.parse(currentNode.getResourcePath().getParentPath()));
				diagram.setDirty(false);    // Will become dirty as soon as we add a block
				diagram.setState(DiagramState.DISABLED);

				String json = serializeDiagram(diagram);	
				log.debugf("%s.DiagramCreateAction. json=%s",CLSS,json);
				ProjectResourceId resid = requestHandler.createResourceId(currentNode.getResourceId().getProjectName(), currentNode.getResourcePath().getParentPath()+"/"+newName, BLTProperties.DIAGRAM_RESOURCE_TYPE);
				byte[] bytes = json.getBytes();
				log.infof("%s.DiagramCreateAction. create new %s(%s), %s (%d bytes)",CLSS,BLTProperties.DIAGRAM_RESOURCE_TYPE.getTypeId(),
						newName,currentNode.getResourcePath().getParentPath()+"/"+newName,bytes.length);
				ProjectResourceBuilder builder = ProjectResource.newBuilder();
				builder.setApplicationScope(ApplicationScope.GATEWAY);
				builder.setResourceId(resid);
				builder.putData(json.getBytes());
				builder.setVersion(0);
				ProjectResource resource = builder.build();
				new ResourceCreateManager(resource,newName).run();	
				currentNode.selectChild(new ResourcePath[] {resource.getResourcePath()} );
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						workspace.open(resid);
					}
				});

			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception creating diagram",err);
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
				String newName = BundleUtil.get().getString(PREFIX+".NewFamily.Default.Name");
				if( newName==null) newName = "New Folks";  // Missing Resource
				SerializableFamily fam = new SerializableFamily();
				fam.setName(newName);
				ProjectResourceId resid = requestHandler.createResourceId(getResourceId().getProjectName(), currentNode.getResourcePath().getFolderPath(), BLTProperties.FAMILY_RESOURCE_TYPE);
				String json = serializeFamily(fam);
				log.debugf("%s.FamilyCreateAction. json=%s",CLSS,json);
				byte[] bytes = json.getBytes();
				log.infof("%FamilyCreateAction. create %s(%s),(%d bytes)",CLSS,fam.getName(),BLTProperties.FAMILY_RESOURCE_TYPE.getTypeId(),bytes.length);
				ProjectResourceBuilder builder = ProjectResource.newBuilder();
				builder.setApplicationScope(ApplicationScope.GATEWAY);
				builder.setResourceId(resid);
				builder.putData(bytes);
				builder.setVersion(0);
				ProjectResource famResource = builder.build();
				new ResourceCreateManager(builder.build(),newName).run();	
				//recreate();
				currentNode.selectChild(new ResourcePath[] {famResource.getResourcePath()} );
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
				String newName = BundleUtil.get().getString(PREFIX+".NewFolder.Default.Name");
				if( newName==null) newName = "New Folder";  // Missing Resource
				ProjectResourceId resid = requestHandler.createResourceId(getResourceId().getProjectName(), getResourceId().getFolderPath(), BLTProperties.FOLDER_RESOURCE_TYPE);
				ProjectResourceBuilder builder = ProjectResource.newBuilder();
				builder.putAttribute("name",newName);
				builder.setApplicationScope(ApplicationScope.GATEWAY);
				builder.setFolder(true);
				builder.setResourceId(resid);
				builder.setVersion(0);
				ProjectResource resource = builder.build();
				new ResourceCreateManager(resource,newName).run();	
				log.infof("%s.FolderCreateAction. create %s(%s,%s) at %s",CLSS,newName,BLTProperties.FOLDER_RESOURCE_TYPE.getModuleId(),
						BLTProperties.FOLDER_RESOURCE_TYPE.getTypeId(),resid.getResourcePath().getPath().toString());
				//recreate();
				currentNode.selectChild(new ResourcePath[] {resid.getResourcePath()} );
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception creating folder",err);
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
						try {
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
											log.infof("%s:ImportDiagramAction imported diagram:\n%s", CLSS,sd.getName());
											renameHandler.convertPaths(sd,getResourcePath().getPath());
											sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
											sd.setState(DiagramState.DISABLED);
											String json = mapper.writeValueAsString(sd);

											log.debugf("%s:ImportDiagramAction saved resource as:\n%s", CLSS,json);
											ProjectResourceId resid = requestHandler.createResourceId(getResourceId().getProjectName(), getResourceId().getFolderPath(), BLTProperties.DIAGRAM_RESOURCE_TYPE);
											ProjectResourceBuilder builder = ProjectResource.newBuilder();
											builder.putData(json.getBytes());
											builder.setApplicationScope(ApplicationScope.GATEWAY);
											builder.setResourceId(resid);
											builder.setVersion(0);;
											ProjectResource resource = builder.build();
											new ResourceCreateManager(resource,resource.getResourceName()).run();	
											parentNode.selectChild(new ResourcePath[] {resource.getResourcePath()} );
											statusManager.setResourceState(resid, sd.getState());
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
				requestHandler.startController();
				this.setEnabled(false);
				stopAction.setEnabled(true);
			} 
			catch (Exception ex) {
				log.warnf("%s: startAction: ERROR: %s",CLSS,ex.getMessage(),ex);
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
				requestHandler.stopController();
				this.setEnabled(false);
				startAction.setEnabled(true);
			}
			catch(Exception ex) {
				log.warnf("%s: stopAction: ERROR: %s",CLSS,ex.getMessage(),ex);
				ErrorUtil.showError(CLSS+" Exception stopping the controller",ex);
			}
		}
	}
	// Navigate the NavTree from the top. Call GetAux on each application, family or block
	// serializing the resource then save. Always execute for production database
	// and tag provider. A similar exercise is performed by the Gateway hook on startup.
	private class SynchronizeAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode root;
		private String db;
		private String provider;
		private DesignableProject proj;
		

		public SynchronizeAction(AbstractResourceNavTreeNode tnode)  {
			super(PREFIX+".Synchronize",IconUtil.getIcon("refresh")); 
			this.root = tnode;
			this.proj = (DesignableProject)context.getProject();
		}

		/**
		 * Update resources within the project in memory. Do not do a final project save.
		 */
		public void actionPerformed(ActionEvent e) {
			db       = requestHandler.getProjectProductionDatabase(context.getProjectName());
			provider = requestHandler.getProjectProductionTagProvider(context.getProjectName());
			synchronizeNode(root,provider,db);
		}	
		
		// This function is called recursively to update the project resource with the nav tree, especially auxiliary data.
		@SuppressWarnings("unchecked")
		private void synchronizeNode(AbstractResourceNavTreeNode node,String tagp,String dsource) {
			//log.infof("%s.refreshNode: %s, (%s,%s)",CLSS,node.getName(),provider,dsource);
			Optional<ProjectResource> optional = node.getProjectResource();
			ProjectResource pr = optional.get();
			if(pr==null || pr.getResourceType()==null) return;
			GeneralPurposeDataContainer container = null;
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			byte[] bytes = null; 
			try {
				if( pr.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
					SerializableApplication sa = deserializeApplication(pr);
					container = ApplicationScriptFunctions.readAuxData(pr.getResourceId(),sa.getResourcePath().toString(),tagp, dsource);
					sa.setAuxiliaryData(container);
					bytes = mapper.writeValueAsBytes(sa);
				}
				else if( pr.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
					SerializableFamily sf = deserializeFamily(pr);
					container = ApplicationScriptFunctions.readAuxData(pr.getResourceId(),sf.getResourcePath().toString(),tagp, dsource);
					sf.setAuxiliaryData(container);
					bytes = mapper.writeValueAsBytes(sf);
				}
				else if( pr.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
					SerializableDiagram dia = deserializeDiagram(pr);
					dia.setDirty(true);    // Dirty because gateway doesn't know about it yet
					for(SerializableBlock blk:dia.getBlocks()) {
						container = ApplicationScriptFunctions.readAuxData(pr.getResourceId(),blk.getId().toString(),tagp, dsource);
						blk.setAuxiliaryData(container);
					}
					bytes = mapper.writeValueAsBytes(dia);
				}
				else if( pr.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE) ) {
					;
				}
				ResourceUpdateManager rum = new ResourceUpdateManager(pr,bytes);
				rum.run();
				return;  // Non BLT type, not interested
			}
			catch(JsonProcessingException jpe) {
				log.warnf("%s.synchronizeNode: Exception parsing JSON for resource %s (%s)",CLSS,pr.getResourceName(),jpe.getMessage());
			}
			
			Enumeration<AbstractResourceNavTreeNode> childWalker = node.children();
			while(childWalker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = childWalker.nextElement();
				synchronizeNode(child,tagp,dsource);
			}
		}
	}
	
	/**
	 * FinalDiagnoses must have unique names within an application. If the current node is an application
	 * scan it. Otherwise check its children.
	 * @param node
	 * @return error message. Empty string implies a successful scan.
	 */
	@SuppressWarnings("unchecked")
	public void scanForNameConflicts(AbstractResourceNavTreeNode node, StringBuffer buf) {
		// Check if subject node is an Application. If so, scan its tree for duplicate names of FinalDiagnoses
		if (node.getProjectResource() != null && node.getResourceId().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			StringBuffer msg = ((GeneralPurposeTreeNode)node).scanApplicationForDuplicateDiagnosisNames(node);
			if( msg.length()>1 ) {
				buf.append(node.getName());
				buf.append(":\n");
				buf.append(msg);
			}
		} 
		// Look in recursively in tree for applications
		else {	
			Enumeration<AbstractResourceNavTreeNode> nodeWalker = node.children();
			while(nodeWalker.hasMoreElements()) {
				AbstractResourceNavTreeNode theNode = nodeWalker.nextElement();
				scanForNameConflicts(theNode,buf);
			}
		}
	}

	private StringBuffer scanApplicationForDuplicateDiagnosisNames(AbstractResourceNavTreeNode appNode) {
		StringBuffer buf = new StringBuffer();
		List<String> nameList = new ArrayList<>();  // Names in this application
		
		Enumeration<AbstractResourceNavTreeNode> nodeWalker = appNode.children();
		while(nodeWalker.hasMoreElements()) {
			AbstractResourceNavTreeNode theNode = nodeWalker.nextElement();
			parseNodeForConflicts(nameList, theNode, buf); 
		}
		return buf;
	}

	@SuppressWarnings("unchecked")
	private void parseNodeForConflicts(List<String> nameList, AbstractResourceNavTreeNode theNode, StringBuffer buf) {
		if (theNode.getProjectResource().get().getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
			Optional<ProjectResource> optional = theNode.getProjectResource();
			ProjectResource res = optional.get();
			String json = new String(res.getData());
			SerializableDiagram sd = null;
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			try {
				sd = mapper.readValue(json,SerializableDiagram.class);
				// Synchronize names as the resource may have been re-named and/or
				// state changed since it was serialized
				sd.setName(res.getResourceName());
				sd.setState(statusManager.getResourceState(resourceId));
				
				// For a small number of blocks on Pete's system, we've removed the name attribute and re-instated.
				// The correct name may be in the property
				ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
				for( Block blk:diagram.getBlocks()) {
					ProcessBlockView pbv = (ProcessBlockView)blk;
					// Re-instate name from property, if necessary
					if( BlockConstants.DEFAULT_BLOCK_NAME.equals(pbv.getName())&& pbv.getProperty(BlockConstants.BLOCK_PROPERTY_NAME)!=null) {
						pbv.setName(pbv.getProperty(BlockConstants.BLOCK_PROPERTY_NAME).getValue().toString());
					}
					// For now only check final diagnosis blocks
					if (pbv.isDiagnosis()) {
						String key = (pbv.getClassName() + pbv.getName()).toLowerCase();
						if (nameList.contains(key)) {
							buf.append( "Duplicate " + pbv.getClassName() + " block named " + pbv.getName() + " found in diagram " + diagram.getName() + "\n");
						} 
						else {
							nameList.add(key);
						}
					}
				}
			} 
			catch (JsonParseException jpe) {
				buf.append(jpe.getLocalizedMessage());
				buf.append("\n");
				log.warnf("%s: open parse exception (%s)",CLSS,jpe.getLocalizedMessage());
			} 
			catch (JsonMappingException jme) {
				buf.append(jme.getLocalizedMessage());
				buf.append("\n");
				log.warnf("%s: open mapping exception (%s)",CLSS,jme.getLocalizedMessage());
			} 
			catch (IOException ioe) {
				buf.append(ioe.getLocalizedMessage());
				buf.append("\n");
				log.warnf("%s: open io exception (%s)",CLSS,ioe.getLocalizedMessage());
			}
			
		}
		// Not-a-diagram
		else {
			Enumeration<AbstractResourceNavTreeNode> nodeWalker = theNode.children();
			while(nodeWalker.hasMoreElements()) {
				AbstractResourceNavTreeNode aNode = nodeWalker.nextElement();
				parseNodeForConflicts(nameList, aNode, buf);
			}
		}
	}
	
	private boolean isRootChild(ProjectResource res) {
		boolean result = false;
		ResourcePath rp = res.getResourcePath();
		if( rp!=null && rp.getPath()!=null ) {
			if(rp.getParentPath()!=null && rp.getParentPath().isEmpty() ) {
				ResourceType type = res.getResourceId().getResourceType();
				if( type!=null && 
					   (type.equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ||
						type.equals(BLTProperties.APPLICATION_RESOURCE_TYPE) ||
						type.equals(BLTProperties.FAMILY_RESOURCE_TYPE) ||
						type.equals(BLTProperties.FOLDER_RESOURCE_TYPE) )  ) {
					result = true;
				}
			}
		}
		return result;
	}
	/**
	 * @param res
	 * @return true if this node is a direct child of the current node
	 */
	private boolean isChildNode(ProjectResource res) {
		boolean result = false;
		if( this.resourceId.getResourcePath().getParentPath()!=null && 
			this.resourceId.getResourcePath().getPath().isParentOf(res.getResourcePath().getPath()) ) {
			result = true;
		}
		return result;
	}
    // ************************ ProjectResourceListener *****************************
	/**
	 * The updates that we are interested in are:
	 *    1) Name changes to this resource
	 * We can ignore deletions because we delete the model resource
	 * by deleting the panel resource.
	 */
	@Override
	public void resourcesCreated(String projectName,List<ChangeOperation.CreateResourceOperation> ops) {
		for(ChangeOperation.CreateResourceOperation op:ops ) {
			ProjectResourceId id = op.getResourceId();
			log.infof("%s.resourcesCreated.%s: %s(%s)",CLSS,op,getName(),id.getProjectName(),id.getResourcePath().getPath().toString());
			ProjectResource res = op.getResource();
			createChildNode(res);
		}
	}
	/**
	 * The updates that we are interested in are:
	 *    1) Name changes to this resource
	 * We can ignore deletions because we delete the model resource
	 * by deleting the panel resource.
	 */
	@Override
	public void resourcesDeleted(String projectName,List<ChangeOperation.DeleteResourceOperation> ops) {
		log.debug(CLSS+".resourcesDeleted (ignore)");
	}
	@Override
	public void resourcesModified(String projectName, List<ChangeOperation.ModifyResourceOperation> changes) {
		// Take care of any special status before invoking the super-class method.
		for(ChangeOperation.ModifyResourceOperation op:changes ) {
			if( op.getResourceId().equals(resourceId) ) {
				log.infof("%s.resourcesModified.%s: %s(%s)",CLSS,op,getName(),resourceId.getProjectName(),resourceId.getResourcePath().getPath().toString());
				ProjectResource res = op.getResource();
				super.onResourceModified(res);
			}
		}
	}
	
	// ************************ NavTreeNodeInterface **************************
	// These methods are used by the NodeStatusManager
	@Override
	public ProjectResourceId getResourceId() { return this.resourceId; }
	// Dirtiness refers to internal state, independent of children.
	@Override
	public boolean isDirty() { return this.dirty; }
	public void setDirty(boolean flag) { this.dirty = flag; }
	// Unsubscribe as a project change listener
	@Override
	public void prepareForDeletion() { uninstall(); }
	/**
	 * Either our state or the state of another node changed, annotate our state. 
	 * Dirtiness for a nav tree node means that one or more of its descendent diagrams
	 * is dirty.
	 * Note: This method should ONLY be called from the node status manager.
	 */
	@Override
	public void updateUI(boolean dty) {
		log.infof("%s.updateUI: %s dirty = %s",CLSS,resourceId.getResourcePath().getPath().toString(),(dty?"true":"false"));
		setItalic(dty);   // NOTE: italic system may be broken ?
		refresh();  // Update the UI
	}
}
