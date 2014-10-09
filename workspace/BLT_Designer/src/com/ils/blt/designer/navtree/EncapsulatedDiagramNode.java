/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.navtree;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.DiagramState;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.designable.DesignableContainer;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayException;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectChangeListener;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.IgnitionDesigner;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.gateway.DTGatewayInterface;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.DesignerProjectContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode.DeleteReason;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.FolderNode;
import com.inductiveautomation.ignition.designer.navtree.model.ResourceDeleteAction;

/**
 * A DiagnosticsNode appears as leaf node in the Diagnostics NavTree hierarchy.
 * It doesn't have any NavTree-type children, but it does have two nested objects, 
 * a DiagnosticsFrame and a diag-model resource. 
 * 
 * The frame is responsible for rendering the diagram based on the model resource.
 * The model can exist without the frame, but not vice-versa.
 */
public class EncapsulatedDiagramNode extends DiagramNode  {
	private static final String TAG = "EncapsulatedDiagramNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final LoggerEx logger = LogUtil.getLogger(getClass().getPackage().getName());



	/**
	 * Constructor. A DiagramNode is created initially without child resources.
	 *      The model resource either pre-exists or is created when a new frame is
	 *      instantiated.
	 * @param context designer context
	 * @param resource panel resource 
	 * @param ws the tabbed workspace holding the diagrams
	 */
	public EncapsulatedDiagramNode(DesignerContext context,ProjectResource resource,DiagramWorkspace ws) {
		super(context,resource,ws);
	}
	


	/**
	 *  Called when the parent folder is deleted.
	 *  If we're closing and committing, then it's fair to
	 *  conclude that the workspace is not dirty.
	 */
	public void closeAndCommit() {
		logger.infof("%s.closeAndCommit: res %d",TAG,resourceId);
		if( workspace.isOpen(resourceId) ) {
			DesignableContainer c = workspace.findDesignableContainer(resourceId);
			BlockDesignableContainer container = (BlockDesignableContainer)c;
			ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
			diagram.setDirty(false);
			workspace.close(resourceId);
		}
	}
	
	
	/**
	 * Before deleting ourself, delete the frame and model, if they exist.
	 * The children aren't AbstractNavTreeNodes ... (??)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void doDelete(List<? extends AbstractNavTreeNode> children,DeleteReason reason) {
		logger.infof("%s.doDelete: res %d",TAG,resourceId);
		ResourceDeleteAction delete = new ResourceDeleteAction(context,
				(List<AbstractResourceNavTreeNode>) children,
				reason.getActionWordKey(), PREFIX+".DiagramNoun");
		if (delete.execute()) {
			UndoManager.getInstance().add(delete, EncapsulatedDiagramNode.class); 
		}
	}
	
	@Override
	public ProjectResource getProjectResource() {
		return context.getProject().getResource(resourceId);
	}

	/**
	 * Return an icon appropriate to the diagram state and whether or not it is displayed.
	 * As far as we can tell getExpandedIcon is never called.
	 */
	@Override
	public Icon getIcon() {	
		icon = closedIcon;
		if( workspace.isOpen(resourceId) ) {
			icon = openIcon;
			DiagramState ds = statusManager.getResourceState(resourceId);
			if( ds.equals(DiagramState.DISABLED))        icon = openDisabledIcon;
			else if( ds.equals(DiagramState.RESTRICTED)) icon = openRestrictedIcon;
		}
		else {
			DiagramState ds = statusManager.getResourceState(resourceId);
			if( ds.equals(DiagramState.DISABLED))        icon = closedDisabledIcon;
			else if( ds.equals(DiagramState.RESTRICTED)) icon = closedRestrictedIcon;
		}
		return icon;
	}
	
	@Override
	public String getWorkspaceName() {
		return DiagramWorkspace.key;
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
		workspace.open(resourceId);
	}
	
	@Override
	public void onEdit(String newTextValue) {
		// Sanitize name
		if (!NAME_PATTERN.matcher(newTextValue).matches()) {
			ErrorUtil.showError(BundleUtil.get().getString(PREFIX+".InvalidName", newTextValue));
		}

		boolean hadLock = context.isLockOpen(resourceId);
		if (context.requestLock(resourceId)) {
			try {
				String oldName = getProjectResource().getName();
				logger.infof("%s: onEdit: alterName from %s to %s",TAG,oldName,newTextValue);
				context.structuredRename(resourceId, newTextValue);
				// If it's open, change its name. Otherwise we sync on opening.
				if(workspace.isOpen(resourceId) ) {
					BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
					if(tab!=null) tab.setName(newTextValue);
				}
				context.updateLock(resourceId);
			} catch (IllegalArgumentException ex) {
				ErrorUtil.showError(ex.getMessage());
			}
			if (!hadLock) {
				context.releaseLock(resourceId);
			}
		}

	}

	/**
	 * Save the current diagram resource, whether or not it is displayed.
	 */
	public void saveDiagram() {
		logger.infof("%s.saveDiagram: %d...",TAG,resourceId);
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
		if( tab!=null ) {
			// If the diagram is open on a tab, call the workspace method to update the project resource
			// from the diagram view. This method handles re-paint of the background.
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			for( Block blk:view.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				pbv.setDirty(false);
			}
			workspace.saveDiagram(tab);
			view.registerChangeListeners();
		}
		// Now save the resource, as it is.
		Project diff = context.getProject().getEmptyCopy();
		ProjectResource res = getProjectResource();
		diff.putResource(res, true);    // Mark as dirty for our controller as resource listener
		try {
			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ..."); // Do not publish
		}
		catch(GatewayException ge) {
			logger.warnf("%s.saveDiagram: Exception saving project resource %d (%s)",TAG,resourceId,ge.getMessage());
		}
		statusManager.clearDirtyBlockCount(resourceId);
		statusManager.setResourceDirty(resourceId,false);
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
		logger.debug(TAG+"projectUpdated "+diff.getDescription());
		if (diff.isResourceDirty(resourceId) && !diff.isResourceDeleted(resourceId)) {
			logger.infof("%s: projectUpdated, setting name ...",TAG);
			setName(diff.getResource(resourceId).getName());
			refresh();
		}
		statusManager.setResourceDirty(resourceId,context.getProject().isResourceDirty(resourceId));
	}
	/**
	 * We got here from either a Save() action or a name change
	 */
	@Override
	public void projectResourceModified(ProjectResource res,ResourceModification changeType) {
		if (res.getResourceId() == resourceId
				&& changeType != ResourceModification.Deleted) {
			logger.infof("%s.projectResourceModified, setting name to: %s",TAG,res.getName());
			boolean changed = !res.getName().equals(getName());
			setName(res.getName());
			statusManager.setResourceDirty(resourceId, changed);
		}
	}
	

	
	// ================================ ChangeListener ======================================

	/**
	 * This method allows us to have children. Children are always EncapsulatedDiagramNodes.
	 * @param arg0
	 * @return
	 */
	protected AbstractNavTreeNode createChildNode(ProjectResource arg0) {
		// TODO Auto-generated method stub
		return null;
	}
}
