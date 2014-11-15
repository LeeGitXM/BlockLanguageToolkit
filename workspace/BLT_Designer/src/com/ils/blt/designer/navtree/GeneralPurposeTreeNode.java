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
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Enumeration;
import java.util.List;
import java.util.UUID;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JPopupMenu;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.ApplicationUUIDResetHandler;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.common.serializable.SerializableFolder;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.common.serializable.UUIDResetHandler;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayException;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectChangeListener;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.IgnitionDesigner;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.gateway.DTGatewayInterface;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.FolderNode;
import com.inductiveautomation.ignition.designer.navtree.model.ResourceDeleteAction;
/**
 * A folder in the designer scope to support the diagnostics toolkit diagram
 * layout. In addition to standard folders, folders can be of type "Application" or
 * "Family". These hold properties special to the Diagnostics Toolkit.  Menu options 
 * vary depending on folder type. Labels are likewise dependent.
 * 
 * Leaf nodes are of type DiagramNode.
 */
public class GeneralPurposeTreeNode extends FolderNode implements ChangeListener,ProjectChangeListener {
	private static final String TAG = "GeneralPurposeTreeNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final LoggerEx logger = LogUtil.getLogger(getClass().getPackage().getName());
	private StartAction startAction = new StartAction();
	private StopAction stopAction = new StopAction();
	private final DiagramWorkspace workspace; 
	private final NodeStatusManager statusManager;
	private final ApplicationCreateAction applicationCreateAction = new ApplicationCreateAction();
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
		workspace = ((BLTDesignerHook)ctx.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		setText(BundleUtil.get().getString(PREFIX+".RootFolderName"));
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
	public GeneralPurposeTreeNode(DesignerContext context,ProjectResource resource,UUID self) {
		super(context,resource.getModuleId(),resource.getApplicationScope(),self);
		this.resourceId = resource.getResourceId();
		setName(resource.getName());      // Also sets text for tree
		
		workspace = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		statusManager.addChangeListener(this);
		
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

	private boolean isRootFolder() {
		return getFolderId().equals(BLTProperties.ROOT_FOLDER_UUID);
	}
	
	@Override
	public Icon getIcon() {
		return closedIcon;
	}
	@Override
	public Icon getExpandedIcon() { 
		return openIcon;
	}

	/**
	 * Create a child node because we've discovered a resource that matches this instance as a parent
	 * based on its content matching the our UUID. With each node creation we create a StatusManager
	 * entry to keep track of "dirtiness" throughout the tree.
	 */
	@Override
	protected AbstractNavTreeNode createChildNode(ProjectResource res) {
		logger.infof("%s.createChildNode type:%s, level=%d", TAG,res.getResourceType(),getDepth());
		AbstractNavTreeNode node = null;
		if (    ProjectResource.FOLDER_RESOURCE_TYPE.equals(res.getResourceType()))       {
			node = new GeneralPurposeTreeNode(context, res, res.getDataAsUUID());
			logger.infof("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
		}
		else if ( BLTProperties.APPLICATION_RESOURCE_TYPE.equals(res.getResourceType()) )       {
			SerializableApplication sa = deserializeApplication(res);
			node = new GeneralPurposeTreeNode(context, res, sa.getId());
			logger.infof("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
		}
		else if ( BLTProperties.FAMILY_RESOURCE_TYPE.equals(res.getResourceType()) )       {
			SerializableFamily fa = deserializeFamily(res); 
			node = new GeneralPurposeTreeNode(context, res, fa.getId());
			logger.infof("%s.createChildNode: (%s) %s->%s",TAG,res.getResourceType(),this.getName(),node.getName());
		}
		else if (BLTProperties.DIAGRAM_RESOURCE_TYPE.equals(res.getResourceType())) {
			node = new DiagramNode(context,res,workspace);
			logger.infof("%s.createChildDiagram: %s->%s",TAG,this.getName(),node.getName());
		} 
		else {
			logger.warnf("%s: Attempted to create a child of type %s (ignored)",TAG,res.getResourceType());
			throw new IllegalArgumentException();
		}
		// Set proper dirtiness for re-constituted nodes
		statusManager.defineResource(resourceId, res.getResourceId());
		node.setItalic(statusManager.isResourceDirty(res.getResourceId()));
		return node;
	}
	
	@Override
	public String getWorkspaceName() {
		return DiagramWorkspace.key;
	}
	
	@Override
	public boolean isEditActionHandler() {
		return isRootFolder();
	}
	/**
	 * Define the menu used for popups. This appears to be called only once for each node.
	 */
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		
		if (isRootFolder()) { 
			ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();

			ApplicationImportAction applicationImportAction = new ApplicationImportAction();
			ClearAction clearAction = new ClearAction();
			DebugAction debugAction = new DebugAction();
			SaveAllAction saveAllAction = new SaveAllAction();
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
			menu.add(saveAllAction);
			menu.add(startAction);
			menu.add(stopAction);
			menu.addSeparator();
			menu.add(clearAction);
			menu.add(debugAction);
		}
		else if(getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
			ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
			ApplicationExportAction applicationExportAction = new ApplicationExportAction(menu.getRootPane(),this);
			FamilyAction familyAction = new FamilyAction();
			NewFolderAction newFolderAction = new NewFolderAction(context,BLTProperties.MODULE_ID,ApplicationScope.DESIGNER,getFolderId(),this);
			ApplicationConfigureAction applicationConfigureAction = new ApplicationConfigureAction(getProjectResource());
			ApplicationSaveAction applicationSaveAction = new ApplicationSaveAction(this);
			applicationSaveAction.setEnabled(!handler.resourceExists(context.getProject().getId(),resourceId) || 
					                          statusManager.isResourceDirty(resourceId));
			menu.add(familyAction);
			menu.add(newFolderAction);
			menu.addSeparator();
			menu.add(applicationConfigureAction);
			menu.add(applicationExportAction);
			menu.add(applicationSaveAction);
			addEditActions(menu);
		}
		else if(getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE)) {
			DiagramAction diagramAction = new DiagramAction();
			NewFolderAction newFolderAction = new NewFolderAction(context,BLTProperties.MODULE_ID,ApplicationScope.DESIGNER,getFolderId(),this);
			FamilyConfigureAction familyConfigureAction = new FamilyConfigureAction(getProjectResource());
			ImportDiagramAction importAction = new ImportDiagramAction();
			menu.add(diagramAction);
			menu.add(importAction);
			menu.add(newFolderAction);
			menu.addSeparator();
			menu.add(familyConfigureAction);
			addEditActions(menu);
			
		}
		else if(getProjectResource().getResourceType().equalsIgnoreCase(BLTProperties.FOLDER_RESOURCE_TYPE)) {
			
			if( hasFamily() ) {
				DiagramAction diagramAction = new DiagramAction();
				menu.add(diagramAction);
				ImportDiagramAction importAction = new ImportDiagramAction();
				CloneDiagramAction cloneAction = new CloneDiagramAction();
				menu.add(importAction);
				menu.add(cloneAction);
			}
			else {
				FamilyAction familyAction = new FamilyAction();
				menu.add(familyAction);
			}
			NewFolderAction newFolderAction = new NewFolderAction(context,BLTProperties.MODULE_ID,ApplicationScope.DESIGNER,getFolderId(),this);
			menu.add(newFolderAction);
			menu.addSeparator();
			addEditActions(menu);	
		}
		else {   
			FamilyAction familyAction = new FamilyAction();
			menu.add(familyAction);
			menu.addSeparator();
			addEditActions(menu);
		}
	}
	
	// Return true if there is a "family" in the ancestral hierarchy of this folder node
	private boolean hasFamily() {
		boolean answer = false;
		AbstractNavTreeNode parent = getParent();
		while( parent!=null ) {
			if( parent instanceof GeneralPurposeTreeNode ) {
				GeneralPurposeTreeNode node = (GeneralPurposeTreeNode)parent;
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
			parent = parent.getParent();
		}
		return answer;
	}
	/**
	 * Exclude cut and paste which are currently not supported.
	 */
	@Override
	protected void addEditActions(JPopupMenu menu)
    {
        menu.add(renameAction);
        menu.add(deleteAction);
    }
	
	private boolean siblings(List<AbstractNavTreeNode> nodes) {
		if (nodes == null || nodes.size() < 1) {
			return false;
		}
		int depth = nodes.get(0).getDepth();
		for (AbstractNavTreeNode node : nodes) {
			if (node.getDepth() != depth) {
				return false;
			}
		}
		return true;
	}

	@Override
	public boolean canDelete(List<AbstractNavTreeNode> selectedChildren) {
		return isEditActionHandler() && siblings(selectedChildren);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void doDelete(List<? extends AbstractNavTreeNode> children,
			DeleteReason reason) {
		logger.infof("%s.doDelete: children of resource %d..",TAG,resourceId);
		for (AbstractNavTreeNode node : children) {
			if (node instanceof DiagramNode) {
				((DiagramNode) node).closeAndCommit();
			}
		}

		ResourceDeleteAction delete = new ResourceDeleteAction(context,
				(List<AbstractResourceNavTreeNode>) children,
				reason.getActionWordKey(), (getDepth()==1? (PREFIX+".ApplicationNoun"):(PREFIX+".FamilyNoun")));
		if (delete.execute()) {
			UndoManager.getInstance().add(delete, GeneralPurposeTreeNode.class);
		}
	}

	@Override
	public void onSelected() {
		UndoManager.getInstance()
				.setSelectedContext(GeneralPurposeTreeNode.class);
	}
	
	/**
	 * Save all applications. This is only valid on the root node. During
	 * the accumulation, we set the resources to "clean".
	 */
	public void saveAll() {
		if( !isRootFolder() ) return;
		Project diff = context.getProject().getEmptyCopy();
		accumulateNodeResources(this,diff);
		try {
			// false=> do not publish
			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ..."); 
		}
		catch(GatewayException ge) {
			logger.warnf("%s.SaveAll: Exception saving project resource %d (%s)",TAG,resourceId,ge.getMessage());
		}
	}
	/**
	 * Traverse the entire node hierarchy looking for diagrams that need saving.
	 * When found, serialize into the project resource. This is in anticipation
	 * of a top-level save.
	 */
	public void serializeResourcesInNeedOfSave() {
		saveThoseInNeed(this);
	}
	/**
	 *  Serialize an Application into JSON.
	 * @param application to be serialized
	 */ 
	private String serializeApplication(SerializableApplication application) {
		String json = "";
		ObjectMapper mapper = new ObjectMapper();
		logger.infof("%s: serializeApplication creating json ... %s",TAG,(mapper.canSerialize(SerializableApplication.class)?"true":"false"));
		try{ 
		    json = mapper.writeValueAsString(application);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize application (%s)",TAG,jpe.getMessage());
		}
		logger.infof("%s: serializeApplication created json ... %s",TAG,json);
		return json;
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
			logger.warnf("%s.deserializeApplication: Deserialization exception (%s)",ex.getMessage());
		}
		return sa;
	}
	/**
	 * Convert the resource data into a SerializableApplication
	 * @param res
	 * @return
	 */
	private SerializableDiagram deserializeDiagram(ProjectResource res) {
		SerializableDiagram sd = null;
		try{
			byte[] bytes = res.getData();
			ObjectMapper mapper = new ObjectMapper();
			sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
			sd.setName(res.getName());   // Sync the SerializableApplication name w/ res
		}
		catch(Exception ex) {
			logger.warnf("%s.SerializableDiagram: Deserialization exception (%s)",ex.getMessage());
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
	/**
	 *  Serialize a diagram into JSON. 
	 * @param diagram to be serialized
	 */ 
	private String serializeDiagram(SerializableDiagram diagram) {
		String json = "";
		ObjectMapper mapper = new ObjectMapper();
		logger.infof("%s: serializeDiagram creating json ... %s",TAG,(mapper.canSerialize(SerializableDiagram.class)?"true":"false"));
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
		logger.infof("%s: serializeFamily creating json ... %s",TAG,(mapper.canSerialize(SerializableFamily.class)?"true":"false"));
		try{ 
		    json = mapper.writeValueAsString(family);
		}
		catch(JsonProcessingException jpe) {
			logger.warnf("%s: Unable to serialize family (%s)",TAG,jpe.getMessage());
		}
		logger.infof("%s: serializeFamily created json ... %s",TAG,json);
		return json;
	}
	// From the root node, tell the gateway controller to clear all resources
	private class ClearAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public ClearAction()  {
			super(PREFIX+".Clear",IconUtil.getIcon("delete_all"));
		}

		public void actionPerformed(ActionEvent e) {
			ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
			handler.clearController();
		}
	}
	// From the root node, recursively log the contents of the tree
	private class DebugAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public DebugAction()  {
			super(PREFIX+".Debug",IconUtil.getIcon("bug_yellow"));
		}

		public void actionPerformed(ActionEvent e) {
			logger.info("============================ Resources (Designer) =========================");
			listProjectResources();
			logger.info("============================ Resources (Gateway) ==========================");
			listControllerResources();
			logger.infof("================================ (proj = %d )==============================",context.getProject().getId());
		}
	}
	// From the root node, create a folder for diagrams belonging to a family
	private class ApplicationCreateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
	    public ApplicationCreateAction()  {
	    	super(PREFIX+".NewApplication",IconUtil.getIcon("folder_new"));
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".NewApplication.Default.Name");
				if( newName==null) newName = "New App";  // Missing Resource
				SerializableApplication app = new SerializableApplication();
				app.setName(newName);
	
				logger.infof("%s: new application action ...",TAG);

				String json = serializeApplication(app);
			
				logger.debugf("%s: ApplicationAction. json=%s",TAG,json);
				byte[] bytes = json.getBytes();
				logger.debugf("%s: ApplicationAction. create new %s resource %d (%d bytes)",TAG,BLTProperties.APPLICATION_RESOURCE_TYPE,
						newId,bytes.length);
				ProjectResource resource = new ProjectResource(newId,
						BLTProperties.MODULE_ID, BLTProperties.APPLICATION_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				context.updateResource(resource);
				selectChild(newId);
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
	// Launch a dialog that configures application attributes.
    private class ApplicationConfigureAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Configure Application";
    	private final ProjectResource res;
	    public ApplicationConfigureAction(ProjectResource resource)  {
	    	super(PREFIX+".ConfigureApplication",IconUtil.getIcon("gear"));  // preferences
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
    						ApplicationConfigurationDialog dialog = new ApplicationConfigurationDialog(sa);
    						dialog.pack();
    						dialog.setVisible(true);   // Returns when dialog is closed
    						if( !dialog.isCancelled() ) {
    							sa = dialog.getApplication();
    							String json = serializeApplication(sa);
    							res.setName(sa.getName());
    							res.setData(json.getBytes());
    							context.updateResource(res);
    							statusManager.setResourceDirty(resourceId, true);		
    						}
    					}
    					else {
    						ErrorUtil.showWarning(String.format("ApplicationConfigurationAction: Failed to deserialize resource"),POPUP_TITLE);
    					}


    				}
    			});
    		} 
    		catch (Exception err) {
    			ErrorUtil.showError(err);
    		}
    	}
	}
	private class ApplicationImportAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Import Application";
	    public ApplicationImportAction()  {
	    	super(PREFIX+".ImportApplication",IconUtil.getIcon("import1"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {

						try {
							long newId = context.newResourceId();
							String title = BundleUtil.get().getString(PREFIX+".Import.Application.DialogTitle");
							String label = BundleUtil.get().getString(PREFIX+".Import.Application.NameLabel");
							ImportDialog dialog = new ImportDialog(label,title);
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
											logger.infof("%s:ApplicationImportAction imported application %s", TAG,sa.getName());
											ApplicationUUIDResetHandler handler = new ApplicationUUIDResetHandler(sa);
											handler.convertUUIDs();
											String json = mapper.writeValueAsString(sa);
											if(logger.isTraceEnabled() ) logger.trace(json);
											ProjectResource resource = new ProjectResource(newId,
													BLTProperties.MODULE_ID, BLTProperties.APPLICATION_RESOURCE_TYPE,
													sa.getName(), ApplicationScope.GATEWAY, json.getBytes());
											resource.setParentUuid(getFolderId());
											context.updateResource(resource);
											selectChild(newId);
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
				ErrorUtil.showError(err);
			}
		}
		private void importFamily(UUID parent,SerializableFamily sf) {
			ObjectMapper mapper = new ObjectMapper();
			try{
				long newId = context.newResourceId();
				String json = mapper.writeValueAsString(sf);
				if(logger.isTraceEnabled() ) logger.trace(json);
				ProjectResource resource = new ProjectResource(newId,
						BLTProperties.MODULE_ID, BLTProperties.FAMILY_RESOURCE_TYPE,
						sf.getName(), ApplicationScope.GATEWAY, json.getBytes());
				resource.setParentUuid(parent);
				context.updateResource(resource);
				selectChild(newId);
				// Now import the diagrams
				for(SerializableDiagram diagram:sf.getDiagrams()) {
					importDiagram(sf.getId(),diagram);
				}
			} 
			catch (Exception ex) {
				ErrorUtil.showError(String.format("ApplicationImportAction: Unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
			}
		}
		private void importDiagram(UUID parent,SerializableDiagram sd) {
			ObjectMapper mapper = new ObjectMapper();
			try{
				long newId = context.newResourceId();
				String json = mapper.writeValueAsString(sd);
				if(logger.isTraceEnabled() ) logger.trace(json);
				ProjectResource resource = new ProjectResource(newId,
						BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
						sd.getName(), ApplicationScope.GATEWAY, json.getBytes());
				resource.setParentUuid(parent);
				context.updateResource(resource);
				selectChild(newId);
			} 
			catch (Exception ex) {
				ErrorUtil.showError(String.format("ApplicationImportAction: Unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
			}
		}
	}
	// Save the entire Application hierarchy.
	private class ApplicationSaveAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final AbstractResourceNavTreeNode root;

	    public ApplicationSaveAction(AbstractResourceNavTreeNode node)  {
	    	super(PREFIX+".SaveApplication",IconUtil.getIcon("add2")); 
	    	root = node;
	    }
	    
		public void actionPerformed(ActionEvent e) {
			// Traverse the entire hierarchy, saving each step
    		Project diff = context.getProject().getEmptyCopy();
    		accumulateNodeResources(root,diff);
    		try {
    			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ..."); // Do not publish
    		}
    		catch(GatewayException ge) {
    			logger.warnf("%s.ApplicationSaveAction: Exception saving project resource %d (%s)",TAG,resourceId,ge.getMessage());
    		}
    		setItalic(false);
		}
	}

	// Create a new diagram
    private class DiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public DiagramAction()  {
	    	super(PREFIX+".NewDiagram",IconUtil.getIcon("folder_new"));  // preferences
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
				logger.infof("%s: new diagram action ...",TAG);

				String json = serializeDiagram(diagram);
			
				logger.debugf("%s: DiagramAction. json=%s",TAG,json);
				byte[] bytes = json.getBytes();
				logger.debugf("%s: DiagramAction. create new %s resource %d (%d bytes)",TAG,BLTProperties.DIAGRAM_RESOURCE_TYPE,
						newId,bytes.length);
				ProjectResource resource = new ProjectResource(newId,
						BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				context.updateResource(resource);
				selectChild(newId);
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						workspace.open(newId);
					}
				});
				
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
    
    
    private class CloneDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Clone Diagram";
	    public CloneDiagramAction()  {
	    	super(PREFIX+".CloneDiagram",IconUtil.getIcon("copy"));  // preferences
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
							ImportDialog dialog = new ImportDialog(label,title);
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
										context.updateResource(resource);
										selectChild(newId);

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
				ErrorUtil.showError(err);
			}
		}
	}
	// From the root node, create a folder for diagrams belonging to a family
	private class FamilyAction extends BaseAction {
		private static final long serialVersionUID = 1L;
	    public FamilyAction()  {
	    	super(PREFIX+".NewFamily",IconUtil.getIcon("folder_new"));
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".NewFamily.Default.Name");
				if( newName==null) newName = "New Folks";  // Missing Resource
				SerializableFamily fam = new SerializableFamily();
				fam.setName(newName);
	
				logger.debugf("%s: new family action ... (%s)",TAG);

				String json = serializeFamily(fam);
			
				logger.debugf("%s: FamilyAction. json=%s",TAG,json);
				byte[] bytes = json.getBytes();
				logger.infof("%s: FamilyAction. create new %s, %s, resource %d (%d bytes)",TAG,BLTProperties.FAMILY_RESOURCE_TYPE,
						newName,newId,bytes.length);
				ProjectResource resource = new ProjectResource(newId,
						BLTProperties.MODULE_ID, BLTProperties.FAMILY_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				context.updateResource(resource);;
				selectChild(newId);
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
    // This really ought to launch a dialog that configures family attributes.
    private class FamilyConfigureAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Configure Family";
    	private ProjectResource res;
	    public FamilyConfigureAction(ProjectResource resource)  {
	    	super(PREFIX+".ConfigureFamily",IconUtil.getIcon("gear"));  // preferences
	    	res = resource;
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
    						FamilyConfigurationDialog dialog = new FamilyConfigurationDialog(sf);
    						dialog.pack();
    						dialog.setVisible(true);   // Returns when dialog is closed
    						if( !dialog.isCancelled()) {
    							sf = dialog.getFamily();
    							String json = serializeFamily(sf);
    							res.setName(sf.getName());
    							res.setData(json.getBytes());
    							context.updateResource(res);
    							statusManager.setResourceDirty(resourceId, true);
    						}
    					}
    					else {
    						ErrorUtil.showWarning(String.format("FamilyConfigurationAction: Failed to deserialize resource"),POPUP_TITLE);
    					}
    				}
    			});
    		} 
    		catch (Exception err) {
    			ErrorUtil.showError(err);
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
						ExportDialog dialog = new ExportDialog();
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
					    			if(logger.isDebugEnabled()) logger.debugf("%s.actionPerformed: creating json ... %s",TAG,(mapper.canSerialize(SerializableDiagram.class)?"true":"false"));
					    			try{ 
					    				// Convert the view into a serializable object
					    				SerializableApplication sap = node.recursivelyDeserializeApplication(node);
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
				ErrorUtil.showError(err);
			}
		}
	}
    private class ImportDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Import Diagram";
	    public ImportDiagramAction()  {
	    	super(PREFIX+".ImportDiagram",IconUtil.getIcon("import1"));  // preferences
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
							ImportDialog dialog = new ImportDialog(label,title);
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
											UUIDResetHandler handler = new UUIDResetHandler(sd);
											handler.convertUUIDs();
											sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
											String json = mapper.writeValueAsString(sd);
											logger.infof("%s:ImportDiagramAction saved resource as:\n%s", TAG,json);
											ProjectResource resource = new ProjectResource(newId,
													BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
													sd.getName(), ApplicationScope.GATEWAY, json.getBytes());
											resource.setParentUuid(getFolderId());
											context.updateResource(resource);
											selectChild(newId);
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
				ErrorUtil.showError(err);
			}
		}
	}
    // Save the entire Application hierarchy.
    private class SaveAllAction extends BaseAction {
    	private static final long serialVersionUID = 1L;

    	public SaveAllAction()  {
    		super(PREFIX+".SaveAll",IconUtil.getIcon("add2")); 
    	}

    	public void actionPerformed(ActionEvent e) {
    		// Traverse the entire hierarchy, saving each step
    		saveAll();
    	}
    	
    }
    // Start refers to a global startup of the Execution controller in the Gateway
    private class StartAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public StartAction()  {
	    	super(PREFIX+".StartExecution",IconUtil.getIcon("disk_play"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
				handler.startController();
				this.setEnabled(false);
				stopAction.setEnabled(true);
			} 
			catch (Exception ex) {
				logger.warnf("%s: startAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(ex);
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
				ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
				handler.stopController();
				this.setEnabled(false);
				startAction.setEnabled(true);
			}
			catch(Exception ex) {
				logger.warnf("%s: stopAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(ex);
			}
		}
	}
    
    // Recursively descend the node tree, gathering up associated resources.
    // Since this is used during a save, set the resources clean.
    public void accumulateNodeResources(AbstractResourceNavTreeNode node,Project diff) {
    	ProjectResource res = node.getProjectResource();
    	if( res!=null ) {
    		logger.infof("%s.accumulateNodeResources: %s (%d)",TAG,res.getName(),res.getResourceId());
    		diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
    		if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
    			// If the resource is open, we need to save it ..
    			DiagramNode dnode = (DiagramNode)node;
    			dnode.saveDiagram();  // Whether it's open or closed.
    		}
    		statusManager.clearDirtyChildCount(resourceId);
    		statusManager.setResourceDirty(res.getResourceId(), false);
    	}
    	else {
    		logger.warnf("%s.accumulateNodeResources: %s has no project resource",TAG,node.getName());
    	}
		@SuppressWarnings("rawtypes")
		Enumeration walker = node.children();
		while(walker.hasMoreElements()) {
			Object child = walker.nextElement();
			accumulateNodeResources((AbstractResourceNavTreeNode)child,diff);
		}
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
    			Object child = walker.nextElement();
    			ProjectResource cres = node.getProjectResource();
    			if(cres==null) continue;
    			if(cres.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)){
    				SerializableFamily sfam = recursivelyDeserializeFamily((AbstractResourceNavTreeNode) child);
    				if( sfam!=null ) sa.addFamily(sfam);
    			}
    			else if(cres.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
    				SerializableFolder sf = recursivelyDeserializeFolder((AbstractResourceNavTreeNode) child);
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

    		@SuppressWarnings("rawtypes")
    		Enumeration walker = node.children();
    		while(walker.hasMoreElements()) {
    			Object child = walker.nextElement();
    			ProjectResource cres = node.getProjectResource();
    			if(cres==null) continue;
    			if( cres.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
    				SerializableDiagram sd = recursivelyDeserializeDiagram((AbstractResourceNavTreeNode) child);
    				if( sd!=null ) sfam.addDiagram(sd);
    			}
    			else if(cres.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
    				SerializableFolder sf = recursivelyDeserializeFolder((AbstractResourceNavTreeNode) child);
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
    			Object child = walker.nextElement();
    			ProjectResource cres = node.getProjectResource();
    			if(cres==null) continue;
    			if( cres.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
    				SerializableDiagram sd = recursivelyDeserializeDiagram((AbstractResourceNavTreeNode) child);
    				if( sd!=null ) sfold.addDiagram(sd);
    			}
    			else if(cres.getResourceType().equals(BLTProperties.FAMILY_RESOURCE_TYPE)){
    				SerializableFamily sfam = recursivelyDeserializeFamily((AbstractResourceNavTreeNode) child);
    				if( sfam!=null ) sfold.addFamily(sfam);
    			}
    			else if(cres.getResourceType().equals(BLTProperties.FOLDER_RESOURCE_TYPE)) {
    				SerializableFolder sf = recursivelyDeserializeFolder((AbstractResourceNavTreeNode) child);
    				if( sf!=null ) sfold.addFolder(sf);
    			}
    			else {
    				logger.infof("%s.recursivelyDeserializeFolder: %s unexpected child resource type (%s)",TAG,res.getName(),cres.getName(),cres.getResourceType());
    			}
    		}
    	}
    	return sfold;
    }
    // Recursively descend the node tree, looking for diagram resources where
    // the associated DiagramView is out-of-synch with the project resource.
    // When found update the project resource.
    private void saveThoseInNeed(AbstractResourceNavTreeNode node) {
    	ProjectResource res = node.getProjectResource();
    	if( res!=null ) {
    		logger.infof("%s.saveThoseInNeed: %s (%d)",TAG,res.getName(),res.getResourceId());
    		if(res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
    			// If the resource is open, we need to save it ..
    			DiagramNode dnode = (DiagramNode)node;
    			dnode.updateResource();  // Only if necessary
    		}
    	}
    	else {
    		logger.warnf("%s.saveThoseInNeed: %s has no project resource",TAG,node.getName());
    	}
    	@SuppressWarnings("rawtypes")
    	Enumeration walker = node.children();
    	while(walker.hasMoreElements()) {
    		Object child = walker.nextElement();
    		saveThoseInNeed((AbstractResourceNavTreeNode)child);
    	}
    }
    /**
	 * Search the project for all resources. This is for debugging.
	 * We filter out those that are global (have no module) as these
	 * are system things that we don't care about for the moment.
	 */
	public void listProjectResources() {
		List <ProjectResource> resources = context.getProject().getResources();
		for( ProjectResource res : resources ) {
			if( res.getModuleId()==null || res.getModuleId().length()==0) continue;
			logger.info("Res: "+res.getResourceId()+" "+res.getResourceType()+" "+res.getModuleId()+" ("+res.getName()+
					":"+res.getParentUuid()+")");
		}
	}
	
	/**
	 * Query the block controller in the Gateway. The resources that it knows
	 * about may, or may not, coincide with those in the Designer. 
	 */
	public void listControllerResources() {
		try {
			ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
			List <SerializableResourceDescriptor> descriptors = handler.queryControllerResources();
			for( SerializableResourceDescriptor descriptor : descriptors ) {
				logger.info("Res: "+descriptor.getProjectId()+":"+descriptor.getResourceId()+" "+
						         descriptor.getType()+" ("+descriptor.getName()+")");
			}
		} 
		catch (Exception ex) {
			logger.warnf("%s. startAction: ERROR: %s",TAG,ex.getMessage(),ex);
			ErrorUtil.showError(ex);
		}
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
	
	
	// ============================================== Project Change Listener =======================================
	
	@Override
	public void projectUpdated(Project diff) {
		logger.infof("%s.projectUpdated ...",TAG);
		super.projectUpdated(diff);
	}

	@Override
	public void projectResourceModified(ProjectResource res, ProjectChangeListener.ResourceModification changeType) {
		if (res.getResourceId() == this.resourceId) {
			logger.infof("%s.projectResourceModified, setting name to: %s",TAG,res.getName());
			boolean changed = !res.getName().equals(getName());
			setName(res.getName());
			statusManager.setResourceDirty(resourceId, changed);
		} 
		else if ((changeType == ProjectChangeListener.ResourceModification.Deleted) ) {
			statusManager.setResourceDirty(resourceId, true);  // A child has been deleted
			recreate();  // Reconstruct the tree
		} 
		else {
			super.projectResourceModified(res, changeType);
		}
	}
	// ================================ ChangeListener ======================================
	// Either our state or the state of another node changed.
	// No matter what we re-compute our state.
	public void stateChanged(ChangeEvent event) {
		// Set italics, enable Save
		boolean dirty = statusManager.isResourceDirty(resourceId);
		logger.infof("%s.stateChanged: %d dirty = %s",TAG,resourceId,(dirty?"true":"false"));
		setItalic(dirty);
		applicationCreateAction.setEnabled(dirty);    // Only applies to an application node
		refresh();  // Update the UI
	}
}
