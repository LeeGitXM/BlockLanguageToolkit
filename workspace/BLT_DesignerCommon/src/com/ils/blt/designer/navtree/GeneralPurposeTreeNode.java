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
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JPopupMenu;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.ResourceCreateManager;
import com.ils.blt.designer.ResourceDeleteManager;
import com.ils.blt.designer.ResourceSaveManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.config.ScriptExtensionConfigurationDialog;
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
 * Support a general tree of resources consisting of folders
 * and diagrams.
 */
public abstract class GeneralPurposeTreeNode extends FolderNode implements NavTreeNodeInterface, ProjectChangeListener {
	protected static final String TAG = "GeneralPurposeTreeNode";
	protected static final int OFFSET = 100;
	protected static final String PREFIX = BLTProperties.CUSTOM_PREFIX;  // Required for some defaults
	protected final LoggerEx logger = LogUtil.getLogger(getClass().getPackage().getName());
	protected boolean dirty = false;
	protected final DiagramWorkspace workspace;
	protected final NodeStatusManager statusManager;
	protected final ToolkitRequestHandler requestHandler;
	protected final ExecutionManager executionEngine;
	
	protected final DeleteNodeAction deleteNodeAction;
	protected final FolderCreateAction folderCreateAction;
	protected final StartAction startAction = new StartAction();
	protected final StopAction stopAction = new StopAction();
	protected TreeSaveAction treeSaveAction = null;


	protected final ImageIcon defaultIcon = IconUtil.getIcon("folder_closed");
	protected ImageIcon openIcon = null;
	protected ImageIcon closedIcon = null;

	/** 
	 * Create a new folder node representing the root folder. The root folder does
	 * not worry about cleanliness.
	 * @param ctx the designer context
	 */
	public GeneralPurposeTreeNode(DesignerContext ctx,UUID nodeId,DiagramWorkspace wksp,ToolkitRequestHandler handler,NodeStatusManager sm) {
		super(ctx, handler.getModuleId(), ApplicationScope.GATEWAY,nodeId);
		this.resourceId = BLTProperties.ROOT_RESOURCE_ID;
		this.executionEngine = new BasicExecutionEngine(1,TAG);
		this.requestHandler  = handler;
		deleteNodeAction = null;
		folderCreateAction = new FolderCreateAction(this);
		this.workspace =wksp;
		this.statusManager = sm;
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
	public GeneralPurposeTreeNode(DesignerContext context,ProjectResource resource,UUID self,DiagramWorkspace wksp,
								  ToolkitRequestHandler handler, NodeStatusManager sm) {
		super(context,resource.getModuleId(),resource.getApplicationScope(),self);
		this.resourceId = resource.getResourceId();
		this.executionEngine = new BasicExecutionEngine(1,TAG);
		setName(resource.getName());      // Also sets text for tree
		
		deleteNodeAction = new DeleteNodeAction(this);
		folderCreateAction = new FolderCreateAction(this);
		this.workspace =wksp;
		this.statusManager = sm;
		this.requestHandler = handler;
		closedIcon = IconUtil.getIcon("folder_closed");
		setIcon(closedIcon);
		openIcon = IconUtil.getIcon("folder");
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
		return openIcon;
	}
	@Override
	public Icon getIcon() {
		return closedIcon;
	}
	@Override
	public long getResourceId() { return this.resourceId; }
	
	@Override
	public String getWorkspaceName() {
		return workspace.getKey();
	}
	// Dirtiness refers to internal state, independent of children.
	public boolean isDirty() { return this.dirty; }
	protected boolean isRootFolder() {
		return (getFolderId().equals(BLTProperties.CLASSIC_ROOT_FOLDER_UUID) ||
				getFolderId().equals(BLTProperties.SCHEMATIC_ROOT_FOLDER_UUID)  );
	}
	public void setDirty(boolean flag) { this.dirty = flag; }
	/**
	 * Query the block controller in the Gateway. The resources that it knows
	 * about may, or may not, coincide with those in the Designer. 
	 */
	public void listControllerResources() {
		try {
			List <SerializableResourceDescriptor> descriptors = requestHandler.listResourceNodes();
			for( SerializableResourceDescriptor descriptor : descriptors ) {
				logger.info("Res: "+descriptor.getProjectId()+":"+descriptor.getResourceId()+" "+
						descriptor.getType()+" ("+descriptor.getName()+")");
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
	public void listProjectResources() {
		List <ProjectResource> resources = context.getProject().getResources();
		for( ProjectResource res : resources ) {
			if( res.getModuleId()==null || res.getModuleId().length()==0) continue;
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
		UndoManager.getInstance().setSelectedContext(getClass());
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
	
	// For DiagramNode.delete
	public void recreate() { super.recreate(); }

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
			logger.warnf("%s.SerializableDiagram: Deserialization exception (%s)",TAG,ex.getMessage());
		}
		return sd;
	}

	
	/**
	 * Create an ImageIcon from the resource path. If it doesn't exist, return the default.
	 * @param path
	 * @return
	 */
	protected ImageIcon iconFromPath(String path) {
		Dimension iconSize = new Dimension(20,20);
		ImageIcon result = defaultIcon;
		Image img = ImageLoader.getInstance().loadImage(path,iconSize);
		if( img!=null ) result = new ImageIcon(img);
		return result;
	}


	// This nodes of the tree is associated with a diagram. It's only other possible children
	// are other diagrams which are children of encapsulation blocks. At present these are not handled
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to a diagram.
	protected SerializableDiagram recursivelyDeserializeDiagram(AbstractResourceNavTreeNode node) {
		ProjectResource res = node.getProjectResource();
		SerializableDiagram sdiag = null;
		if( res!=null ) {
			logger.infof("%s.recursivelyDeserializeDiagram: %s (%d)",TAG,res.getName(),res.getResourceId());
			sdiag = deserializeDiagram(res);
		}
		return sdiag;
	}
	

	/**
	 *  Serialize a diagram into JSON. 
	 * @param diagram to be serialized
	 */ 
	protected String serializeDiagram(SerializableDiagram diagram) {
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


	// ============================================= private action classes ===========================================================
		
	// From the root node, tell the gateway controller to clear all resources
	protected class ClearAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public ClearAction()  {
			super(PREFIX+".Clear",IconUtil.getIcon("delete_all"));
		}

		public void actionPerformed(ActionEvent e) {
			requestHandler.clearController();
		}
	}
	protected class CloneDiagramAction extends BaseAction {
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
												requestHandler.getModuleId(), BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE,
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
	protected class DebugAction extends BaseAction {
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
	// Delete the this node and all its descendants. 
	// Note: On a "save" action, the descendants are removed also.
	protected class DeleteNodeAction extends BaseAction implements UndoManager.UndoAction {
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
			boolean wasDirty = statusManager.isResourceDirty(resid);
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
						if( wasDirty ) statusManager.decrementDirtyNodeCount(parentNode.resourceId);
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

	
	// From the root node, create a folder for diagrams belonging to a family
	protected class FolderCreateAction extends BaseAction {
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
				UUID newId = context.addFolder(newResId, requestHandler.getModuleId(), ApplicationScope.GATEWAY, newName, getFolderId());
				logger.infof("%s.FolderCreateAction. create new %s(%d), %s (%s, parent %s)",TAG,BLTProperties.FOLDER_RESOURCE_TYPE,newResId,newName,
						newId.toString(),getFolderId().toString());
				//recreate();
				currentNode.selectChild(new long[] {newResId} );
				statusManager.incrementDirtyNodeCount(resourceId);
			} 
			catch (Exception err) {
				ErrorUtil.showError(TAG+" Exception creating folder",err);
			}

		}
	}

	// Save the entire Application hierarchy.
	protected class SaveAllAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode node;

		public SaveAllAction(AbstractResourceNavTreeNode treeNode)  {
			super(PREFIX+".SaveAll",IconUtil.getIcon("add2")); 
			this.node = treeNode;
		}

		public void actionPerformed(ActionEvent e) {
			// Traverse the entire hierarchy, saving each step
			if( !isRootFolder() ) return;
			executionEngine.executeOnce(new ResourceSaveManager(workspace,node));
			statusManager.cleanAll();
		}
	}

	// Start refers to a global startup of the Execution controller in the Gateway
	protected class StartAction extends BaseAction {
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
				logger.warnf("%s: startAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(TAG+" Exception starting the controller",ex);
			}
		}
	}

	protected class StopAction extends BaseAction {
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
				logger.warnf("%s: stopAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(TAG+" Exception stopping the controller",ex);
			}
		}
	}
	// Launch a dialog to configure toolkit-wide attributes.
	public class ToolkitConfigureAction extends BaseAction {
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
						ScriptExtensionConfigurationDialog dialog = new ScriptExtensionConfigurationDialog(context.getFrame(),context,requestHandler);
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
	// Save this node and all its descendants.
	public class TreeSaveAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final GeneralPurposeTreeNode node;

		public TreeSaveAction(GeneralPurposeTreeNode treeNode, String bundleString)  {
			super(bundleString,IconUtil.getIcon("add2")); 
			node = treeNode;
		}

		public void actionPerformed(ActionEvent e) {
			executionEngine.executeOnce(new ResourceSaveManager(workspace,node));
			ProjectResource pr = node.getProjectResource();
			if( pr!=null )
				statusManager.clearDirtyChildCount(pr.getResourceId());
			else
				statusManager.clearDirtyChildCount(BLTProperties.ROOT_RESOURCE_ID);
		}
	}
}
