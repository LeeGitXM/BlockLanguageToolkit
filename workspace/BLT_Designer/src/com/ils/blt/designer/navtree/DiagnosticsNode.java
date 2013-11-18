/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.navtree;

import java.awt.event.ActionEvent;
import java.util.List;
import java.util.UUID;

import javax.swing.JInternalFrame;
import javax.swing.JPopupMenu;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.tree.TreePath;

import com.ils.blt.designer.workspace.DiagnosticsFrame;
import com.ils.blt.designer.workspace.DiagnosticsWorkspace;
import com.ils.diagnostics.common.DTProperties;
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
import com.inductiveautomation.ignition.common.xmlserialization.serialization.XMLSerializer;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.ResourceDeleteAction;
/**
 * A DiagnosticsNode appears as leaf node in the Diagnostics NavTree hierarchy.
 * It doesn't have any NavTree-type children, but it does have a nested object, 
 * a DiagnosticsFrame. The frame is responsible for rendering the diagram
 * based on the ModelResource. This class is responsible for maintaining the
 * corresponding project resource.
 */
public class DiagnosticsNode extends AbstractResourceNavTreeNode 
							implements ProjectChangeListener, InternalFrameListener  {
	private static final String TAG = "DiagnosticsNode:";
	private static final String NAV_PREFIX = "NavTree";       // Required for some defaults
	private static final String BUNDLE_NAME = "navtree";      // Name of properties file
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final CloseAction closeAction;
	private final OpenAction openAction;
	private final SaveAction saveAction;
	private DesignerContext context;
	private DiagnosticsFrame frame = null;
	private final String name;
	private final UUID uuid;
	private long resourceId;
	private boolean isFrameOpen = false;

	/**
	 * Constructor. A DiagnosticsNode always has a child DiagnosticsWorkspace.
	 *      This is either re-constituted at this time based on resource Id, 
	 *      or is created.
	 * @param context designer context
	 * @param resource project resource that 
	 * @param nodeName node name
	 */
	public DiagnosticsNode(DesignerContext context,ProjectResource resource,String nodeName) {
		BundleUtil.get().addBundle(NAV_PREFIX,getClass(),BUNDLE_NAME);
		this.context = context;
		this.resourceId = resource.getResourceId();
		this.uuid = UUID.randomUUID();
		this.name = nodeName;	

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
	 * Search existing frames for the workspace, else create a new one.
	 */
	public void findOrCreateFrame() {
		DiagnosticsWorkspace workspace = DiagnosticsWorkspace.getInstance();
		JInternalFrame[] frames = workspace.getAllFrames();
		frame = null;
		for(JInternalFrame frm:frames) {
			if( name.equals(frm.getName()) ) {
				if( frm instanceof DiagnosticsFrame ) frame = (DiagnosticsFrame)frm;
				break;
			}
		}
		// If we haven't found one, create one
		if( frame==null ) {
			TreePath treePath = pathToRoot();
			String tpath = treePathToString(treePath);
			log.debug(TAG+"findOrCreateFrame from:"+tpath);
			frame = new DiagnosticsFrame(context,name,tpath);
		}
		frame.addInternalFrameListener(this);
		
		// Next search existing resources to see if we have a match.
		// If we find one, then set the model in the frame
		List <ProjectResource> resources = context.getProject().getResources();
		long resid = -1;
		boolean resourceExists = false;
		for( ProjectResource res : resources ) {
			if( uuid.equals(res.getParentUuid()) ) {    // parentUUID can be null
				// Deserialize the model
				// TODO
				resid = res.getResourceId();
				resourceExists = true;
				break;
			}
		}
		// If there is no existing resource, create one with the supplied name
		if( !resourceExists ) {
			try {
				resid = context.newResourceId();
				String model = frame.getModel();
				log.debug(TAG+"NEW MODEL = \n"+model);
				XMLSerializer s = context.createSerializer();
				byte[] bytes = s.serializeBinary(model, true);
				ProjectResource resource = new ProjectResource(resid,
						DTProperties.MODULE_ID, DTProperties.MODEL_RESOURCE_TYPE,
						name, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(uuid);
				context.updateResource(resource);
				log.info(String.format("%s: create frame %d",TAG,resid));
			}
			catch(SerializationException se) {
				log.warn(String.format("%s.findOrCreateFrame: Unable to serialize model (%s)",TAG,se.getMessage()));
			}
			catch(Exception ex) {
				log.warn(String.format("%s.findOrCreateFrame: Exception (%s)",TAG,ex.getMessage()));
			}
		}
		else {
			log.info(String.format("%s: find frame %d",TAG,resid));
		}
		frame.setResourceId(resid);
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
	
	@SuppressWarnings("unchecked")
	@Override
	public void doDelete(List<? extends AbstractNavTreeNode> children,DeleteReason reason) {
		// We have no children -- just place on the undo queue.
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
	public UUID getUUID() { return this.uuid; }

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
		DiagnosticsWorkspace workspace = DiagnosticsWorkspace.getInstance();
		workspace.open(frame);
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
	}

	@Override
	protected void uninstall() {
		super.uninstall();
		context.removeProjectChangeListener(this);
	}
	
	private void closeFrame() {
		DiagnosticsWorkspace workspace = DiagnosticsWorkspace.getInstance();
		workspace.close(frame);
	}
	
	private void openFrame() {
		DiagnosticsWorkspace workspace = DiagnosticsWorkspace.getInstance();
		workspace.open(frame);
	}
	
	// Set the internal flag that markw wheter the frame is open or not.
	// Set the icon acccordingly.
	private void setFrameOpen(boolean isOpen) {
		isFrameOpen = isOpen;
		if( isOpen )  setIcon(IconUtil.getIcon("table_sql"));
		else          setIcon(IconUtil.getIcon("folder_document"));
	}

	// ----------------------- Project Change Listener -------------------------------
	/**
	 * The updates that we are interested in are:
	 *    1) Name changes to this resource
	 *    2) Addition of a DTProperties.MODEL_RESOURCE_TYPE with this as a parent.
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

	@Override
	public void projectResourceModified(ProjectResource res,
			ResourceModification changeType) {
		log.debug(TAG+"projectModified: "+res.getResourceId()+" "+res.getResourceType()+" "+res.getModuleId()+" ("+res.getName()+
				":"+res.getParentUuid()+")");
		if (res.getResourceId() == resourceId
				&& changeType != ResourceModification.Deleted) {
			log.info(TAG+"projectResourceModified, setting name/italic + refreshing");
			setName(res.getName());
			setItalic(true);
			refresh();
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
			closeFrame();
		}
	}
	private class OpenAction extends BaseAction {
		private static final long serialVersionUID = 1L;

		public OpenAction()  {
			super(NAV_PREFIX+".Open",IconUtil.getIcon("window_new"));
		}

		public void actionPerformed(ActionEvent e) {
			openFrame();
		}
	}
	private class SaveAction extends BaseAction {
		private static final long serialVersionUID = 1L;

		public SaveAction()  {
			super(NAV_PREFIX+".Save",IconUtil.getIcon("add2"));
		}

		public void actionPerformed(ActionEvent e) {
			frame.saveResource();
		}
	}
}
