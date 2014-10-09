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
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.ResourceDeleteAction;

/**
 * A DiagramNode appears as leaf node in the Diagnostics NavTree hierarchy.
 * It serves as a Nav-tree standin for a DiagramWorkspace. A DiagramNode
 * may have children - EncapsulatedDiagramNodes - which are standins for
 * sub-workspaces of EncapsulationBlocks. 
 * 
 * The frame is responsible for rendering the diagram based on the model resource.
 * The model can exist without the frame, but not vice-versa.
 */
public class DiagramNode extends AbstractResourceNavTreeNode implements ChangeListener, ProjectChangeListener  {
	private static final String TAG = "DiagramNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults

	protected final LoggerEx logger = LogUtil.getLogger(getClass().getPackage().getName());
	protected DesignerContext context;
	protected long resourceId;
	protected final DiagramWorkspace workspace;
	protected final NodeStatusManager statusManager;
	protected final SaveDiagramAction saveAction = new SaveDiagramAction();
	protected final ImageIcon defaultIcon;
	protected final ImageIcon openIcon;
	protected final ImageIcon closedIcon;
	protected final ImageIcon openDisabledIcon;
	protected final ImageIcon closedDisabledIcon;
	protected final ImageIcon openRestrictedIcon;
	protected final ImageIcon closedRestrictedIcon;

	/**
	 * Constructor. A DiagramNode is created initially without child resources.
	 *      The model resource either pre-exists or is created when a new frame is
	 *      instantiated.
	 * @param context designer context
	 * @param resource panel resource 
	 * @param ws the tabbed workspace holding the diagrams
	 */
	public DiagramNode(DesignerContext context,ProjectResource resource,DiagramWorkspace ws) {
		this.context = context;
		this.resourceId = resource.getResourceId();
		this.workspace = ws;
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		statusManager.addChangeListener(this);
		setName(resource.getName());
		setText(resource.getName());
		
		defaultIcon = IconUtil.getIcon("unknown");
		openIcon = iconFromPath("Block/icons/navtree/diagram.png");
		// We have just defined the default (expanded) variant. Here are some more.
		closedIcon = iconFromPath("Block/icons/navtree/diagram_closed.png");
		openDisabledIcon = iconFromPath("Block/icons/navtree/diagram_disabled.png");
		closedDisabledIcon = iconFromPath("Block/icons/navtree/diagram_closed_disabled.png");
		openRestrictedIcon = iconFromPath("Block/icons/navtree/diagram_restricted.png");
		closedRestrictedIcon = iconFromPath("Block/icons/navtree/diagram_closed_restricted.png");
		setIcon( closedIcon);
		setItalic(context.getProject().isResourceDirty(resourceId));
		context.addProjectChangeListener(this);
	}
	
	
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		ExportDiagramAction exportAction = new ExportDiagramAction(menu.getRootPane(),resourceId);
		
		menu.add(exportAction);
		
		// States are: ACTIVE, DISABLED, RESTRICTED
		ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
		DiagramState state = handler.getDiagramState(context.getProject().getId(), resourceId);
		SetStateAction ssaActive = new SetStateAction(DiagramState.ACTIVE);
		ssaActive.setEnabled(!state.equals(DiagramState.ACTIVE));
		SetStateAction ssaDisable = new SetStateAction(DiagramState.DISABLED);
		ssaDisable.setEnabled(!state.equals(DiagramState.DISABLED));
		SetStateAction ssaRestricted = new SetStateAction(DiagramState.RESTRICTED);
		ssaRestricted.setEnabled(!state.equals(DiagramState.RESTRICTED));
		JMenu setStateMenu = new JMenu(BundleUtil.get().getString(PREFIX+".SetState"));
		setStateMenu.add(ssaActive);
		setStateMenu.add(ssaDisable);
		setStateMenu.add(ssaRestricted);
		menu.add(setStateMenu);
	
		// Only allow a Save when the diagram is dirty, exists in the controller
		saveAction.setEnabled(diagramIsSavable(handler,resourceId));
		menu.add(saveAction);
		menu.addSeparator();
		menu.add(renameAction);
        menu.add(deleteAction);
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
	 * @return true if the diagram is in a state to be saved. 
	 */
	private boolean diagramIsSavable(ApplicationRequestHandler handler,long resId) {
		boolean existsInController = handler.resourceExists(context.getProject().getId(),resourceId);
		if( !existsInController )  return false;    // Controller has no knowledge of this diagram
		                                            // Must save application.
		
		return statusManager.isResourceDirty(resourceId);
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
			UndoManager.getInstance().add(delete, DiagramNode.class); 
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
    
    private class ExportDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Export Diagram";
    	private final Component anchor;
    	public ExportDiagramAction(Component c,long resid)  {
    		super(PREFIX+".ExportDiagram",IconUtil.getIcon("export1")); 
    		anchor = c;
    	}

    	public void actionPerformed(ActionEvent e) {
    		if( resourceId<0 ) return;   // Do nothing
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
    								output.setWritable(true); 
    							}
    							else {
    								output.createNewFile();
    							}

    							if( output.canWrite() ) {
    								ProjectResource res = context.getProject().getResource(resourceId);
    								if( res!=null ) {

    									byte[] bytes = res.getData();
    									FileWriter fw = new FileWriter(output,false);  // Do not append
    									try {
    										fw.write(new String(bytes));
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
    								else {
    									ErrorUtil.showWarning(String.format("Resource %d does not exist",resourceId),POPUP_TITLE,false);
    								}
    							}
    							else {
    								ErrorUtil.showWarning(String.format("Cannot write to file (%s)",output.getAbsolutePath()),POPUP_TITLE,false);
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
	
	
	private class SaveDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public SaveDiagramAction()  {
	    	super(PREFIX+".SaveDiagram",IconUtil.getIcon("add2")); 
	    }
	    
		public void actionPerformed(ActionEvent e) {
			saveDiagram();
		}
	}
	private class SetStateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final DiagramState state;
		public SetStateAction(DiagramState s)  {
			super(PREFIX+".SetStateAction."+s.name());
			state = s;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				// If the diagram is showing, then all we do is set the view
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
				if( tab!=null ) {
					ProcessDiagramView view = (ProcessDiagramView)(tab.getModel());
					view.setState(state);
					tab.setBackground(view.getBackgroundColorForState());
				}
				// Otherwise we need to de-serialize and re-serialize
				else {
					ProjectResource res = context.getProject().getResource(resourceId);
					byte[]bytes = res.getData();
					SerializableDiagram sd = null;
					ObjectMapper mapper = new ObjectMapper();
					sd = mapper.readValue(bytes,SerializableDiagram.class);
					// Synchronize names as the resource may have been re-named since it was serialized
					sd.setName(res.getName());
					sd.setState(state);
					bytes = mapper.writeValueAsBytes(sd);
					res.setData(bytes); // We don't alert the gateway at this point. (We may be dirty)
				}

				// Inform the gateway of the state change
				ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
				handler.setDiagramState(context.getProject().getId(), resourceId,state.name());
				statusManager.setResourceState(resourceId,state);
				setIcon(getIcon());
				refresh();
			} 
			catch (Exception ex) {
				logger.warn(String.format("%s.setStateAction: ERROR: %s",TAG,ex.getMessage()),ex);
				ErrorUtil.showError(ex);
			}
		}
	}
	@Override
	protected DesignerProjectContext projectCtx() {
		return context;
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
	
	// ================================ ChangeListener ======================================
	// Either our state or the state of another node changed.
	// No matter what we re-compute our state.
	public void stateChanged(ChangeEvent event) {
		// Set italics, enable Save
		boolean dirty = statusManager.isResourceDirty(resourceId);
		logger.infof("%s.stateChanged: dirty = %s",TAG,(dirty?"true":"false"));
		setItalic(dirty);
		saveAction.setEnabled(dirty);
		refresh();
	}

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
