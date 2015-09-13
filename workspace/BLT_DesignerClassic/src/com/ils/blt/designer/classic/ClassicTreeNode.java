/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *  Based on sample code provided by Inductive Automation.
 */
package com.ils.blt.designer.classic;

import java.awt.Component;
import java.awt.EventQueue;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Enumeration;
import java.util.List;
import java.util.UUID;

import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.ApplicationUUIDResetHandler;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableFolder;
import com.ils.blt.common.serializable.UUIDResetHandler;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.ResourceCreateManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.classic.config.ApplicationConfigurationDialog;
import com.ils.blt.designer.config.FamilyConfigurationDialog;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.navtree.ExportDialog;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.navtree.ImportDialog;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ProjectChangeListener;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
/**
 * A folder in the designer scope to support the diagnostics toolkit diagram
 * layout. In addition to standard folders, folders can be of type "Application" or
 * "Family". These hold properties special to the Diagnostics Toolkit.  Menu options 
 * vary depending on folder type. Labels are likewise dependent.
 * 
 * Use of Applications and Families is optional 
 * 
 * Leaf nodes are of type DiagramTreeNode.
 */
public class ClassicTreeNode extends GeneralPurposeTreeNode  {

	
	/** 
	 * Create a new folder node representing the root folder. The root folder does
	 * not worry about cleanliness.
	 * @param ctx the designer context
	 */
	public ClassicTreeNode(DesignerContext ctx,DiagramWorkspace wksp,ToolkitRequestHandler handler,NodeStatusManager sm) {
		super(ctx,BLTProperties.CLASSIC_ROOT_FOLDER_UUID,wksp,handler,sm);
		String rootName = requestHandler.getToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_CLASSIC_ROOT);
		if( rootName == null ) rootName = BLTProperties.DEFAULT_CLASSIC_ROOT_FOLDER_NAME;
		this.setName(rootName);
		this.resourceId = BLTProperties.ROOT_RESOURCE_ID;
		this.setText(rootName);
		closedIcon = IconUtil.getIcon("folder_closed");
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
	public ClassicTreeNode(DesignerContext context,ProjectResource resource,UUID self,DiagramWorkspace wksp,
								  ToolkitRequestHandler handler, NodeStatusManager sm) {
		super(context,resource,self,wksp,handler,sm);
		this.resourceId = resource.getResourceId();
		setName(resource.getName());      // Also sets text for tree

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


	// Note: Override the default to take care of database interaction
	@Override
	public void projectResourceModified(ProjectResource res, ProjectChangeListener.ResourceModification changeType) {
		// Take care of our special status before invoking the super-class method.
		//logger.infof("%s.projectResourceModified.%s: %s(%d), res %s(%d)",TAG,changeType.name(),getName(),this.resourceId,res.getName(),res.getResourceId());
		if (res.getResourceId() == this.resourceId) {
			if( res.getName()==null || !res.getName().equals(getName()) ) {
				logger.infof("%s.projectResourceModified(%d), setting name %s to %s",TAG,this.resourceId,getName(),res.getName());
				ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
				// For application or family name changes, we need to synchronize the database
				if( res.getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
					try {
						SerializableApplication sa = deserializeApplication(res);
						extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_RENAME_SCRIPT, 
								sa.getId().toString(),getName(),res.getName());
					}
					catch( Exception ex ) {
						log.errorf("ApplicationConfigurationController.save: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
					}
				}
				else if( res.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)) {
					try {
						SerializableFamily sf = deserializeFamily(res);
						extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_RENAME_SCRIPT, 
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
				node = new ClassicTreeNode(context, res, res.getDataAsUUID(),workspace,requestHandler,statusManager);
				logger.tracef("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
			}
			else if ( BLTProperties.APPLICATION_RESOURCE_TYPE.equals(res.getResourceType()) )       {
				SerializableApplication sa = deserializeApplication(res);
				node = new ClassicTreeNode(context, res, sa.getId(),workspace,requestHandler,statusManager);
				logger.tracef("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
			}
			else if ( BLTProperties.FAMILY_RESOURCE_TYPE.equals(res.getResourceType()) )       {
				SerializableFamily fa = deserializeFamily(res); 
				node = new ClassicTreeNode(context, res, fa.getId(),workspace,requestHandler,statusManager);
				logger.tracef("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
			}
			else if (BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE.equals(res.getResourceType())) {
				node = new DiagramTreeNode(context,res,workspace,requestHandler,statusManager);
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
			ApplicationCreateAction applicationCreateAction = new ApplicationCreateAction(this);
			ApplicationImportAction applicationImportAction = new ApplicationImportAction(menu.getRootPane(),this);
			ToolkitConfigureAction configureAction = new ToolkitConfigureAction(menu.getRootPane());
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
			menu.add(applicationCreateAction);
			menu.add(applicationImportAction);
			menu.add(configureAction);
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

	// Return true if there is a "family" in the ancestral hierarchy of this folder node
	private boolean hasFamily() {
		boolean answer = false;

		AbstractNavTreeNode parentNode = getParent();
		while( parentNode!=null ) {
			if( parentNode instanceof ClassicTreeNode ) {
				ClassicTreeNode node = (ClassicTreeNode)parentNode;
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

	protected boolean isRootFolder() {
		return getFolderId().equals(BLTProperties.CLASSIC_ROOT_FOLDER_UUID);
	}
	
	// Recursively descend the node tree, gathering up associated resources.
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to an application.
	private SerializableApplication recursivelyDeserializeApplication(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		SerializableApplication sa = null;
		if( res!=null ) {
			logger.infof("%s.recursivelyDeserializeApplication: %s (%d)",TAG,res.getName(),res.getResourceId());
			sa = deserializeApplication(res);

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

	// Recursively descend the node tree, gathering up associated resources.
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to an application.
	private SerializableFamily recursivelyDeserializeFamily(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		SerializableFamily sfam = null;
		if( res!=null ) {
			logger.infof("%s.recursivelyDeserializeFamily: %s (%d)",TAG,res.getName(),res.getResourceId());
			sfam = deserializeFamily(res);

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				ProjectResource cres = child.getProjectResource();
				if(cres==null) continue;
				if( cres.getResourceType().equals(BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE)) {
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

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				ProjectResource cres = child.getProjectResource();
				if(cres==null) continue;
				if( cres.getResourceType().equals(BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE)) {
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
							ApplicationConfigurationDialog dialog = new ApplicationConfigurationDialog(context.getFrame(),context,sa);
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
				ProjectResource resource = new ProjectResource(newResId,BLTProperties.CLASSIC_MODULE_ID, BLTProperties.APPLICATION_RESOURCE_TYPE,
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
		private final ClassicTreeNode node;
		private final Component anchor;
		public ApplicationExportAction(Component c,ClassicTreeNode gptn)  {
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
										// Convert the view into a serializable object
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
													BLTProperties.CLASSIC_MODULE_ID, BLTProperties.APPLICATION_RESOURCE_TYPE,
													sa.getName(), ApplicationScope.GATEWAY, json.getBytes());
											resource.setParentUuid(getFolderId());
											new ResourceCreateManager(resource).run();
											root.selectChild(new long[] {newId} );
											// Now import families
											for(SerializableFamily fam:sa.getFamilies()) {
												importFamily(sa.getId(),fam);
											}
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
							}  // Cancel
						} 
						catch (Exception ex) {
							ErrorUtil.showError(String.format("ApplicationImportAction: Unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
						}
						// No need to inform of success, we'll see the new diagram
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception importing application",err);
			}
		}

		private void importFamily(UUID parentId,SerializableFamily sf) {
			ObjectMapper mapper = new ObjectMapper();
			try{
				long newId = context.newResourceId();
				String json = mapper.writeValueAsString(sf);
				if(logger.isTraceEnabled() ) logger.trace(json);
				ProjectResource resource = new ProjectResource(newId,
						BLTProperties.CLASSIC_MODULE_ID, BLTProperties.FAMILY_RESOURCE_TYPE,
						sf.getName(), ApplicationScope.GATEWAY, json.getBytes());
				resource.setParentUuid(parentId);
				logger.infof("%s:ApplicationImportAction importing family %s(%s) (%s/%s)", TAG,sf.getName(),newId,parentId.toString(),sf.getId().toString());
				executionEngine.executeOnce(new ResourceCreateManager(resource));	
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
				logger.infof("%s.DiagramCreateAction. create new %s(%d), %s (%d bytes)",TAG,BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE,newId,
						newName,bytes.length);
				ProjectResource resource = new ProjectResource(newId,BLTProperties.CLASSIC_MODULE_ID, BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE,
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
							FamilyConfigurationDialog dialog = new FamilyConfigurationDialog(context.getFrame(),context,sf);
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
				ProjectResource resource = new ProjectResource(newId,BLTProperties.CLASSIC_MODULE_ID, BLTProperties.FAMILY_RESOURCE_TYPE,
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
										mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL, true);
										SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
										if( sd!=null ) {
											logger.infof("%s:ImportDiagramAction imported diagram:\n%s", TAG,sd.getName());
											UUIDResetHandler uuidHandler = new UUIDResetHandler(sd);
											uuidHandler.convertUUIDs();
											sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
											String json = mapper.writeValueAsString(sd);
											logger.debugf("%s:ImportDiagramAction saved resource as:\n%s", TAG,json);
											ProjectResource resource = new ProjectResource(newId,
													BLTProperties.CLASSIC_MODULE_ID, BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE,
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
					BLTProperties.CLASSIC_MODULE_ID, BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE,
					sd.getName(), ApplicationScope.GATEWAY, json.getBytes());
			resource.setParentUuid(parentId);
			executionEngine.executeOnce(new ResourceCreateManager(resource));	
		} 
		catch (Exception ex) {
			ErrorUtil.showError(String.format("importDiagram: importing diagrm, unhandled Exception (%s)",ex.getMessage()),
					"Import Diagram",ex,true);
		}
	}
	
	// Launch a dialog that recursively saves auxiliary data from the application
	// into the current database.
	private class RestoreAuxiliaryDataAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode node;

		public RestoreAuxiliaryDataAction(AbstractResourceNavTreeNode treeNode)  {
			super(PREFIX+".RestoreAuxiliaryData",IconUtil.getIcon("data_replace"));  // preferences
			node = treeNode;
		}

		public void actionPerformed(ActionEvent e) {
			// Traverse the hierarchy of the application, saving auxiliary data at each step
			executionEngine.executeOnce(new AuxiliaryDataRestoreManager(workspace,node));
		}
	}
	
	// Recursively save auxiliary data from the application and its descendants
	// into the current database.
	private class SaveAuxiliaryDataAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode node;

		public SaveAuxiliaryDataAction(AbstractResourceNavTreeNode treeNode)  {
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
	 * the project resources.
	 */
	private class SetApplicationStateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode app;
		private final DiagramState state;
		public SetApplicationStateAction(AbstractResourceNavTreeNode applicationNode,DiagramState s)  {
			super(PREFIX+".SetApplicationState."+s.name());
			this.app = applicationNode;
			this.state = s;
		}
		
		// We need to set the state both locally and in the gateway
		public void actionPerformed(ActionEvent e) {
			requestHandler.setApplicationState(app.getName(), state.name());
			if( state.name().equals(DiagramState.ISOLATED)) {
				executionEngine.executeOnce(new AuxiliaryDataSaveManager(app));
			}
			// Set the nodes in the navtree
			recursivelyUpdateNodeState(app,state);
		}
		
		public void recursivelyUpdateNodeState(AbstractNavTreeNode node,DiagramState diagramStatetate) {
			if( node==null) return;
			if( node instanceof DiagramTreeNode ) {
				statusManager.setResourceState(((DiagramTreeNode) node).getResourceId(),diagramStatetate);
				DiagramTreeNode dtn = (DiagramTreeNode)node;
				dtn.setIcon(dtn.getIcon());
				dtn.refresh();
			}
			@SuppressWarnings("unchecked")
			Enumeration<AbstractNavTreeNode>  childWalker = node.children();
			while(childWalker.hasMoreElements()) {
				recursivelyUpdateNodeState(childWalker.nextElement(),diagramStatetate);
			}
		}
	}
}
