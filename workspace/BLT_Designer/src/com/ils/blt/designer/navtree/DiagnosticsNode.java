/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.navtree;

import java.awt.event.ActionEvent;
import java.util.List;
import java.util.UUID;

import javax.swing.JPopupMenu;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.tree.TreePath;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.DiagnosticsFrame;
import com.ils.blt.designer.workspace.DiagnosticsWorkspace;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectChangeListener;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.common.xmlserialization.SerializationException;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.ResourceDeleteAction;
/**
 * A DiagnosticsNode appears as leaf node in the Diagnostics NavTree hierarchy.
 * It doesn't have any NavTree-type children, but it does have two nested objects, 
 * a DiagnosticsFrame and a diag-model resource. 
 * 
 * The frame is responsible for rendering the diagram based on the model resource.
 * The model can exist without the frame, but not vice-versa.
 */
public class DiagnosticsNode extends AbstractResourceNavTreeNode 
							implements ProjectChangeListener, InternalFrameListener  {
	private static final String TAG = "DiagnosticsNode:";
	private static final String NAV_PREFIX = "NavTree";       // Required for some defaults
	private static final String BUNDLE_NAME = "navtree";      // Name of properties file
	private static final String MODEL_SUFFIX = "-model";      // Suffix for model child resource
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final CloseAction closeAction;
	private final OpenAction openAction;
	private final SaveAction saveAction;
	private DesignerContext context;
	private DiagnosticsFrame frame = null;
	private ProjectResource modelResource = null;
	
	private String name;
	private final UUID parentUUID;   // Parent resource, a navtree folder
	private long resourceId;
	private boolean isFrameOpen = false;

	/**
	 * Constructor. A DiagnosticsNode is created initially without child resources.
	 *      The model resource either pre-exists or is created when a new frame is
	 *      instantiated.
	 * @param context designer context
	 * @param resource panel resource 
	 * @param nodeName node name
	 */
	public DiagnosticsNode(DesignerContext context,ProjectResource resource) {
		BundleUtil.get().addBundle(NAV_PREFIX,getClass(),BUNDLE_NAME);
		this.context = context;
		this.resourceId = resource.getResourceId();
		this.parentUUID = resource.getParentUuid();
		this.name = resource.getName();	

		setName(name);
		setText(name);
		setIcon(IconUtil.getIcon("folder_document"));

		setItalic(context.getProject().isResourceDirty(resourceId));
		context.addProjectChangeListener(this);
		
		closeAction = new CloseAction();
		openAction  = new OpenAction();
		saveAction  = new SaveAction();
	}
	
	/**
	 * We have been asked to open a visible frame. If our frame exists,
	 * then open it, otherwise create one. Once open, search model
	 * resources and define the model. If the model resource does not
	 * exist, then create a new one.
	 */
	public void openFrame() {
		DiagnosticsWorkspace workspace = DiagnosticsWorkspace.getInstance();
		// First the frame ... if we don't know about it, then it shouldn't exist.
		String frameName = name+MODEL_SUFFIX;  // Make frame name different
		if( frame==null ) {
			// Create it
			TreePath treePath = pathToRoot();
			String tpath = treePathToString(treePath);
			log.debugf("%s: openFrame from: %s",TAG,tpath);
			frame = new DiagnosticsFrame(context,frameName,tpath);
			frame.addInternalFrameListener(this);
		}			
		workspace.open(frame);
		
		// Next the model resource. If we don't have one, then search.
		// (In case we were notified before we existed).
		// Once we have one, then set the model in the frame
		if( modelResource ==null ) {
			List <ProjectResource> resources = context.getProject().getResources();
			long resid = -1;
			for( ProjectResource res : resources ) {
				if( res.getResourceType().equalsIgnoreCase(BLTProperties.MODEL_RESOURCE_TYPE ) &&
						res.getParentUuid().equals(parentUUID)   )       {   
					modelResource = res;
					break;
				}
			}
			// If there is no existing resource, create one with the supplied name
			if( modelResource == null ) {
				try {
					resid = context.newResourceId();
					String model = frame.getModel();
					log.infof("%s: openFrame - new empty model = %s",TAG,model);
					modelResource = new ProjectResource(resid,
							BLTProperties.MODULE_ID, BLTProperties.MODEL_RESOURCE_TYPE,
							frameName, ApplicationScope.GATEWAY, model.getBytes());
					modelResource.setParentUuid(parentUUID);
					context.updateResource(modelResource);  // This will cause an update.
					log.infof("%s: openFrame: created model resource %s (%d)",TAG,resid);
				}
				catch(SerializationException se) {
					log.warnf("%s: openFrame Unable to serialize model (%s)",TAG,se.getMessage());
				}
				catch(Exception ex) {
					log.warnf("%s: openFrame:  Exception (%s)",TAG,ex.getMessage());
				}
			}
		}
		else {
			log.info(String.format("%s: openFrame: existing model resource %d",TAG,modelResource.getResourceId()));
		}
		frame.setResourceId(modelResource.getResourceId());
	}
	
	
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		closeAction.setEnabled(isFrameOpen);
		openAction.setEnabled(!isFrameOpen);
		menu.add(openAction);
		menu.add(closeAction);
		menu.add(saveAction);
		menu.addSeparator();
		menu.add(renameAction);
        menu.add(deleteAction);
	}


	// Called when the parent folder is deleted
	public void closeAndCommit() {
		if( frame!=null ) {
			DiagnosticsWorkspace workspace = DiagnosticsWorkspace.getInstance();
			frame.saveResource();
			workspace.remove(frame);
		}
	}
	
	/**
	 * Before deleting ourself, delete the frame and model, if they exist.
	 * The children aren't AbstractNavTreeNodes ... (??)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void doDelete(List<? extends AbstractNavTreeNode> children,DeleteReason reason) {
		ResourceDeleteAction delete = new ResourceDeleteAction(context,
				(List<AbstractResourceNavTreeNode>) children,
				reason.getActionWordKey(), NAV_PREFIX+".DiagramNoun");
		if (delete.execute()) {
			UndoManager.getInstance().add(delete, DiagnosticsNode.class); 
		}
	}
	
	@Override
	public ProjectResource getProjectResource() {
		return context.getProject().getResource(resourceId);
	}

	@Override
	public String getWorkspaceName() {
		return DiagnosticsWorkspace.getInstance().getKey();
	}
	@Override
	public boolean isEditActionHandler() {return true;}
	@Override
	public boolean isEditable() {return true;}
	
	/**
	 * As far as the tree knows, we're a leaf.
	 */
	@Override
	public boolean isLeaf() { return true; }
	
	@Override
	public void onDoubleClick() {
		openFrame();
	}
	@Override
	public void onEdit(String newTextValue) {
		// Sanitize name
		if (!NAME_PATTERN.matcher(newTextValue).matches()) {
			ErrorUtil.showError(BundleUtil.get().getString(
					NAV_PREFIX+".InvalidName", newTextValue));
		}

		boolean hadLock = context.isLockOpen(resourceId);
		if (context.requestLock(resourceId)) {
			try {
				log.info(TAG+"onEdit: alterName "+newTextValue);
				context.structuredRename(resourceId, newTextValue);
				context.updateLock(resourceId);
			} catch (IllegalArgumentException ex) {
				ErrorUtil.showError(ex.getMessage());
			}
			if (!hadLock) {
				context.releaseLock(resourceId);
			}
		}
		// Rename the child resources as well ...
		// TODO:
	}

	@Override
	protected void uninstall() {
		super.uninstall();
		context.removeProjectChangeListener(this);
	}
	
	/**
	 * We have received notice of an updated or new model resource.
	 * If the frame is open, then we need to update the visible model.
	 * @param res
	 */
	private void updateModel(ProjectResource res) {
		this.modelResource = res;
		if( frame!=null ) {

			log.debugf("%s: updateModel: Deserializing ..",TAG);
			String xml = new String(res.getData());
			log.debugf("%s: updateModel: res = %s",TAG,xml);
			frame.setModel(xml);
			frame.setResourceId(res.getResourceId());
		}
	}
	
	private void closeFrame() {
		DiagnosticsWorkspace workspace = DiagnosticsWorkspace.getInstance();
		workspace.close(frame);
	}
	

	
	// Set the internal flag that marks whether the frame is open or not.
	// Set the icon accordingly.
	private void setFrameOpen(boolean isOpen) {
		isFrameOpen = isOpen;
		if( isOpen )  setIcon(IconUtil.getIcon("table_sql"));
		else          setIcon(IconUtil.getIcon("folder_document"));
	}

	// ----------------------- Project Change Listener -------------------------------
	/**
	 * The updates that we are interested in are:
	 *    1) Name changes to this resource
	 * We can ignore deletions because we delete the model resource
	 * by deleting the panel resource.
	 */
	@Override
	public void projectUpdated(Project diff) {
		log.debug(TAG+"projectUpdated "+diff.getDescription());
		if (diff.isResourceDirty(resourceId) && !diff.isResourceDeleted(resourceId)) {
			log.info(TAG+"projectUpdated, setting name/italic + refreshing");
			setName(diff.getResource(resourceId).getName());
			refresh();
		}
		setItalic(context.getProject().isResourceDirty(resourceId));
	}
	/**
	 * The updates that we are interested in are:
	 *    1) Addition of a DTProperties.MODEL_RESOURCE_TYPE with same parent as this.
	 *    2) Resource name change, we change ours to keep in sync.
	 */
	@Override
	public void projectResourceModified(ProjectResource res,ResourceModification changeType) {
		log.debug(TAG+"projectModified: "+res.getResourceId()+" "+res.getResourceType()+" "+res.getModuleId()+" ("+res.getName()+
				":"+res.getParentUuid()+")");
		if (res.getResourceId() == resourceId
				&& changeType != ResourceModification.Deleted) {
			log.info(TAG+"projectResourceModified, setting name/italic + refreshing");
			setName(res.getName());
			setItalic(true);
			refresh();
		}
		// Watch for a model resource that is our "child"
		else if( res.getResourceType().equalsIgnoreCase(BLTProperties.MODEL_RESOURCE_TYPE) && res.getParentUuid().equals(parentUUID) ) {
			updateModel(res);
		}
	}
	/**
	 * Convert a tree path to a colon-delimited string.
	 * @param tpath
	 * @return the path as a string
	 */
	private String treePathToString(TreePath tpath) {
		String result = "";
		if( tpath!=null && tpath.getPathCount()>2) {
			// 0 = the project name
			// 1 = null for the root node.
			int index=2;
			while(index<tpath.getPathCount()) {
				result = String.format("%s:%s", result,tpath.getPathComponent(index));
				index++;
			}
		}
		return result;
	}
	// ==================================== Internal Frame Listener ==================================
	// A DiagnosticsNode is an internal frame listener on the desktop manager.
	// Filter events so that we only react to the "interesting" changes.
	@Override
	public void internalFrameOpened(InternalFrameEvent e) {
		log.debug(TAG+"internalFrameOpened: "+e.getInternalFrame().getName());
		setFrameOpen(true);
	}

	@Override
	public void internalFrameClosing(InternalFrameEvent e) {
	}

	@Override
	public void internalFrameClosed(InternalFrameEvent e) {
		log.debug(TAG+"internalFrameClosed: "+e.getInternalFrame().getName());
		setFrameOpen(false);
	}

	@Override
	public void internalFrameIconified(InternalFrameEvent e) {	
	}

	@Override
	public void internalFrameDeiconified(InternalFrameEvent e) {	
	}

	@Override
	public void internalFrameActivated(InternalFrameEvent e) {	
		log.debug(TAG+"internalFrameActivated: "+e.getInternalFrame().getName());
		setFrameOpen(true);
	}

	@Override
	public void internalFrameDeactivated(InternalFrameEvent e) {
		log.debug(TAG+"internalFrameDeactivated: "+e.getInternalFrame().getName());
		setFrameOpen(false);
	}
	
	// ==================================== Action Classes ==================================
	// Close the workspace
	private class CloseAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public CloseAction()  {
			super(NAV_PREFIX+".Close",IconUtil.getIcon("window_reset"));
		}

		public void actionPerformed(ActionEvent e) {
			log.infof("%s: Close.actionPerformed",TAG);
			closeFrame();
		}
	}
	private class OpenAction extends BaseAction {
		private static final long serialVersionUID = 1L;

		public OpenAction()  {
			super(NAV_PREFIX+".Open",IconUtil.getIcon("window_new"));	
		}

		public void actionPerformed(ActionEvent e) {
			log.infof("%s: Open.actionPerformed",TAG);
			openFrame();
		}
	}
	private class SaveAction extends BaseAction {
		private static final long serialVersionUID = 1L;

		public SaveAction()  {
			super(NAV_PREFIX+".Save",IconUtil.getIcon("add2"));
		}

		public void actionPerformed(ActionEvent e) {
			log.infof("%s: Save.actionPerformed",TAG);
			if( frame!=null ) frame.saveResource();
			else log.warnf("%s: Save.actionPerformed - null frame",TAG);
		}
	}
}
