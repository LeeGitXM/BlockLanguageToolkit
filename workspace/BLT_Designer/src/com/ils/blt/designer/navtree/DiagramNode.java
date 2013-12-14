/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.navtree;

import java.util.List;

import javax.swing.JPopupMenu;
import javax.swing.tree.TreePath;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
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
import com.inductiveautomation.ignition.designer.navtree.model.ResourceDeleteAction;
/**
 * A DiagnosticsNode appears as leaf node in the Diagnostics NavTree hierarchy.
 * It doesn't have any NavTree-type children, but it does have two nested objects, 
 * a DiagnosticsFrame and a diag-model resource. 
 * 
 * The frame is responsible for rendering the diagram based on the model resource.
 * The model can exist without the frame, but not vice-versa.
 */
public class DiagramNode extends AbstractResourceNavTreeNode implements ProjectChangeListener  {
	private static final String TAG = "DiagnosticsNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults

	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private DesignerContext context;
	private long resourceId;
	private final DiagramWorkspace workspace;


	/**
	 * Constructor. A DiagnosticsNode is created initially without child resources.
	 *      The model resource either pre-exists or is created when a new frame is
	 *      instantiated.
	 * @param context designer context
	 * @param resource panel resource 
	 * @param nodeName node name
	 */
	public DiagramNode(DesignerContext context,ProjectResource resource,DiagramWorkspace ws) {
		this.context = context;
		this.resourceId = resource.getResourceId();
		this.workspace = ws;

		setName(resource.getName());
		setText(resource.getName());
		setIcon(IconUtil.getIcon("folder_document"));

		setItalic(context.getProject().isResourceDirty(resourceId));
		context.addProjectChangeListener(this);
	}
	
	
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);

		menu.add(renameAction);
        menu.add(deleteAction);
	}


	// Called when the parent folder is deleted
	public void closeAndCommit() {
		if( workspace.isOpen(resourceId) ) workspace.close(resourceId);
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
				reason.getActionWordKey(), PREFIX+".DiagramNoun");
		if (delete.execute()) {
			UndoManager.getInstance().add(delete, DiagramNode.class); 
		}
	}
	
	@Override
	public ProjectResource getProjectResource() {
		return context.getProject().getResource(resourceId);
	}

	@Override
	public String getWorkspaceName() {
		return DiagramWorkspace.key;
	}
	@Override
	public boolean isEditActionHandler() {return false;}
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
			log.infof("%s: projectUpdated, setting name/italic + refreshing",TAG);
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
	}
}
