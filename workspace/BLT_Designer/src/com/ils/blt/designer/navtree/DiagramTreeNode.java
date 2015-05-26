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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.ResourceDeleteManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.designable.DesignableContainer;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.execution.ExecutionManager;
import com.inductiveautomation.ignition.common.execution.impl.BasicExecutionEngine;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectChangeListener;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.DesignerProjectContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;

/**
 * A DiagramNode appears as leaf node in the Diagnostics NavTree hierarchy.
 * It serves as a Nav-tree standin for a DiagramWorkspace. A DiagramNode
 * may have children - EncapsulatedDiagramNodes - which are standins for
 * sub-workspaces of EncapsulationBlocks. 
 * 
 * The frame is responsible for rendering the diagram based on the model resource.
 * The model can exist without the frame, but not vice-versa.
 */
public class DiagramTreeNode extends AbstractResourceNavTreeNode implements NavTreeNodeInterface, ProjectChangeListener  {
	private static final String TAG = "DiagramTreeNode";
	private static final int OFFSET = 100;
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	protected final LoggerEx logger = LogUtil.getLogger(getClass().getPackage().getName());
	protected DesignerContext context;
	private boolean dirty = false;     
	protected long resourceId;
	private final ExecutionManager executionEngine;
	protected final DiagramWorkspace workspace;
	private SaveDiagramAction saveAction = null;
	protected final NodeStatusManager statusManager;
	protected final ImageIcon defaultIcon;
	protected final ImageIcon openIcon;
	protected final ImageIcon closedIcon;
	protected final ImageIcon openDisabledIcon;
	protected final ImageIcon closedDisabledIcon;
	protected final ImageIcon openRestrictedIcon;
	protected final ImageIcon closedRestrictedIcon;

	/**
	 * Constructor. A DiagramTreeNode is created initially without child resources.
	 *      The model resource either pre-exists or is created when a new frame is
	 *      instantiated.
	 * @param context designer context
	 * @param resource panel resource 
	 * @param ws the tabbed workspace holding the diagrams
	 */
	public DiagramTreeNode(DesignerContext context,ProjectResource resource,DiagramWorkspace ws) {
		this.context = context;
		this.executionEngine = new BasicExecutionEngine(1,TAG);
		this.resourceId = resource.getResourceId();
		this.workspace = ws;
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		setName(resource.getName());
		setText(resource.getName());
		
		defaultIcon = IconUtil.getIcon("unknown");
		openIcon = iconFromPath("Block/icons/navtree/diagram.png");
		// We have just defined the default (expanded) variant. Here are some more.
		closedIcon = iconFromPath("Block/icons/navtree/diagram_closed.png");
		openDisabledIcon = iconFromPath("Block/icons/navtree/diagram_disabled.png");
		closedDisabledIcon = iconFromPath("Block/icons/navtree/diagram_closed_disabled.png");
		openRestrictedIcon = iconFromPath("Block/icons/navtree/diagram_isolated.png");
		closedRestrictedIcon = iconFromPath("Block/icons/navtree/diagram_closed_isolated.png");
		setIcon( closedIcon);
		setItalic(statusManager.isResourceDirtyOrHasDirtyChidren(resourceId));
		context.addProjectChangeListener(this);
	}
	@Override
	public void uninstall() {
		//context.removeProjectChangeListener(this);     // (This is what FolderNode does)
	}
	
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		if( this.getParent()==null ) {
			logger.errorf("%s.initPopupMenu: ERROR: Diagram (%d) has no parent",TAG,hashCode());
		}
		ExportDiagramAction exportAction = new ExportDiagramAction(menu.getRootPane(),resourceId);
		menu.add(exportAction);
		DeleteDiagramAction diagramDeleteAction = new DeleteDiagramAction(this);
		DebugDiagramAction debugAction = new DebugDiagramAction();
		ResetDiagramAction resetAction = new ResetDiagramAction();
		
		// States are: ACTIVE, DISABLED, ISOLATED
		ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
		DiagramState state = handler.getDiagramState(context.getProject().getId(), resourceId);
		saveAction = new SaveDiagramAction(this);
		SetStateAction ssaActive = new SetStateAction(DiagramState.ACTIVE);
		ssaActive.setEnabled(!state.equals(DiagramState.ACTIVE));
		SetStateAction ssaDisable = new SetStateAction(DiagramState.DISABLED);
		ssaDisable.setEnabled(!state.equals(DiagramState.DISABLED));
		SetStateAction ssaIsolated = new SetStateAction(DiagramState.ISOLATED);
		ssaIsolated.setEnabled(!state.equals(DiagramState.ISOLATED));
		JMenu setStateMenu = new JMenu(BundleUtil.get().getString(PREFIX+".SetState"));
		setStateMenu.add(ssaActive);
		setStateMenu.add(ssaDisable);
		setStateMenu.add(ssaIsolated);
		menu.add(setStateMenu);
		menu.add(saveAction);
		menu.addSeparator();
		menu.add(renameAction);
        menu.add(diagramDeleteAction);
        menu.addSeparator();
        menu.add(debugAction);
        menu.add(resetAction);
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
			diagram.unregisterChangeListeners();
			workspace.close(resourceId);
		}
	}
	
	/**
	 *  If the diagram associated with this node is open, save its state.
	 */
	public void saveOpenDiagram() {
		logger.infof("%s.closeAndSave: res %d",TAG,resourceId);
		// If the diagram is open on a tab, call the workspace method to update the project resource
		// from the diagram view. This method handles re-paint of the background.

		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
		if( tab!=null ) {

			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			for( Block blk:view.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				pbv.setDirty(false);  // Suppresses the popup?
			}
			workspace.saveDiagramResource(tab);
			view.registerChangeListeners();
		}
	}
	@Override
	public boolean confirmDelete(List<? extends AbstractNavTreeNode> selections) {
		// We only care about the first
		boolean result = false;
		if( selections.size()>0 ) {
			AbstractNavTreeNode selected = selections.get(0);
			result = ErrorUtil.showConfirm(String.format(BundleUtil.get().getString(PREFIX+".Delete.Confirmation.Question.Diagram"), selected.getName()), BundleUtil.get().getString(PREFIX+".Delete.Confirmation.Title.Diagram"));
		}
		return result;
	}

	public boolean isDirty() { return dirty; }
	public void setDirty(boolean flag) { this.dirty = flag; }
	
	@Override
	public long getResourceId() { return this.resourceId; }
	
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
			else if( ds.equals(DiagramState.ISOLATED)) icon = openRestrictedIcon;
		}
		else {
			DiagramState ds = statusManager.getResourceState(resourceId);
			if( ds.equals(DiagramState.DISABLED))        icon = closedDisabledIcon;
			else if( ds.equals(DiagramState.ISOLATED)) icon = closedRestrictedIcon;
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
	
	
	@Override
	public void onDoubleClick() {
		workspace.open(resourceId);
	}
	
	/**
	 *  Note: We ignore locking, as it basically implies that the diagram is showing
	 *        on an open tab. We take care of re-naming the tab.
	 *        Attempt to keep the collapsed state as it was
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
			// If it's open, change its name. Otherwise we sync on opening.
			if(workspace.isOpen(resourceId) ) {
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
				if(tab!=null) {
					tab.setName(newTextValue);
				}
			}
		}
		catch (IllegalArgumentException ex) {
			ErrorUtil.showError(TAG+".onEdit: "+ex.getMessage());
		}
	}
	
	// We're not a listener on anything. But we do need to
	// close the tab if we're open.
	public void prepareForDeletion() {
		closeAndCommit();
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
		logger.debug(TAG+".projectUpdated "+diff.getDescription());
		if (diff.isResourceDirty(resourceId) && !diff.isResourceDeleted(resourceId)) {
			logger.infof("%s.projectUpdated, setting name ...",TAG);
			setName(diff.getResource(resourceId).getName());
			refresh();
		}
		if( context.getProject().isResourceDirty(resourceId)) {
			statusManager.incrementDirtyNodeCount(resourceId);
		}
		else {
			statusManager.clearDirtyChildCount(resourceId);
		}
	}
	
	/**
	 * We got here from either a Save() action or a name change. We don't have children, so no worry about
	 * recreate() after delete. Be careful not to update a project resource here, else we get a hard loop.
	 */
	@Override
	public void projectResourceModified(ProjectResource res,ResourceModification changeType) {
		if (res.getResourceId() == resourceId) {
			logger.debugf("%s.projectResourceModified.%s: %s(%d), res %s(%d)",TAG,changeType.name(),getName(),this.resourceId,res.getName(),res.getResourceId());
			if( res.getName()==null || !res.getName().equals(getName()) ) {
				setName(res.getName());
				setText(res.getName());
			}
		}
	}
	// From the root node, recursively log the contents of the tree
	private class DebugDiagramAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public DebugDiagramAction()  {
			super(PREFIX+".DebugDiagram",IconUtil.getIcon("bug_yellow"));
		}

		public void actionPerformed(ActionEvent e) {
			logger.info("============================ Diagram (Designer) ========================");
			listDiagramComponents();
			logger.info("============================ Diagram(Gateway) ==========================");
			listDiagramGatewayComponents();
			logger.info("========================================================================");
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

    	public void actionPerformed(final ActionEvent e) {
    		if( resourceId<0 ) return;   // Do nothing
    		try {
    			EventQueue.invokeLater(new Runnable() {
    				public void run() {
    					ExportDialog dialog = new ExportDialog(context.getFrame());
    					Object source = e.getSource();
    					if( source instanceof Component) {
    						dialog.setLocationRelativeTo((Component)source);
    					}
    					//dialog.setLocationRelativeTo(anchor);
    					//Point p = dialog.getLocation();
    					//dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
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
    			ErrorUtil.showError(TAG+": Exception writing diagram.",err);
    		}
    	}
    }
    private class DeleteDiagramAction extends BaseAction implements UndoManager.UndoAction {
    	private static final long serialVersionUID = 1L;
    	private final ResourceDeleteManager deleter;
		private String bundleString;
    	private final DiagramTreeNode node;
    	
	    public DeleteDiagramAction(DiagramTreeNode treeNode)  {
	    	super(PREFIX+".DeleteDiagram",IconUtil.getIcon("delete")); 
	    	node = treeNode;
	    	this.bundleString = PREFIX+".DiagramNoun";
	    	this.deleter = new ResourceDeleteManager(node);
	    }

	    public void actionPerformed(ActionEvent e) {
	    	closeAndCommit();

	    	List<AbstractResourceNavTreeNode>selected = new ArrayList<>();
	    	selected.add(node);
	    	boolean wasDirty = node.isDirty();
	    	if(confirmDelete(selected)) {
	    		deleter.acquireResourcesToDelete();
	    		if( execute() ) {
	    			UndoManager.getInstance().add(this,GeneralPurposeTreeNode.class);

	    			AbstractNavTreeNode p = node.getParent();
	    			if( p instanceof GeneralPurposeTreeNode )  {
	    				GeneralPurposeTreeNode parentNode = (GeneralPurposeTreeNode)p;
	    				parentNode.recreate();
	    				parentNode.expand();
	    				if( wasDirty ) statusManager.decrementDirtyNodeCount(parentNode.getResourceId());
	    			}
	    			deleter.deleteInProject();
	    		}
	    		else {
	    			ErrorUtil.showInfo(workspace, TAG+"Delete failed", "Delete Action");
	    		}

	    	}
	    }


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
	
	private class ResetDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public ResetDiagramAction()  {
	    	super(PREFIX+".ResetDiagram",IconUtil.getIcon("check2")); 
	    }
	    
		public void actionPerformed(ActionEvent e) {
			ApplicationRequestHandler handler = new ApplicationRequestHandler();
			handler.resetDiagram(workspace.getActiveDiagram().getId().toString());
		}
	}
	private class SaveDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final DiagramTreeNode node;
	    public SaveDiagramAction(DiagramTreeNode treeNode)  {
	    	super(PREFIX+".SaveDiagram",IconUtil.getIcon("add2"));
	    	this.node = treeNode;
	    }
	    
		public void actionPerformed(ActionEvent e) {
			ProjectResource pr = node.getProjectResource();
			if( pr!=null ) {
				new ResourceUpdateManager(workspace,pr).run();
				statusManager.clearDirtyChildCount(pr.getResourceId());
			}
			else {
				statusManager.clearDirtyChildCount(BLTProperties.ROOT_RESOURCE_ID);
			}
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
					res.setData(bytes); 
					setDirty(true);
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
				ErrorUtil.showError(TAG+" Exception setting state",ex);
				
			}
		}
	}
	@Override
	protected DesignerProjectContext projectCtx() {
		return context;
	}
	
	/**
	 * Find the current process diagram and list its blocks.
	 */
	public void listDiagramComponents() {
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
		if( tab!=null ) {
			// If the diagram is open on a tab, call the workspace method to update the project resource
			// from the diagram view. This method handles re-paint of the background.
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			for( Block blk:view.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				logger.info("Block: "+pbv.getName()+"\t"+pbv.getClassName()+"\t("+pbv.getId().toString()+")");
			}
		}
		else {
			logger.info("     Diagram must be open in tab ...");
		}
	}

	/**
	 * Query the referenced diagram in the Gateway. The blocks that it knows
	 * about may, or may not, coincide with those in the Designer. 
	 */
	public void listDiagramGatewayComponents() {
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
		if( tab!=null ) {
			// If the diagram is open on a tab, call the workspace method to update the project resource
			// from the diagram view. This method handles re-paint of the background.
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
			try {
				List <SerializableBlockStateDescriptor> descriptors = handler.listBlocksInDiagram(view.getId().toString());
				for( SerializableBlockStateDescriptor descriptor : descriptors ) {
					Map<String,String> attributes = descriptor.getAttributes();
					String clss = attributes.get(BLTProperties.BLOCK_ATTRIBUTE_CLASS);
					String uid = attributes.get(BLTProperties.BLOCK_ATTRIBUTE_ID);
					logger.info("Block: "+descriptor.getName()+"\t"+clss+"\t("+uid+")");
				}
			} 
			catch (Exception ex) {
				logger.warnf("%s. startAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(TAG+" Exception listing diagram components",ex);
			}
		}
		else {
			logger.info("     Diagram must be open in tab ...");
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
	
	/**
	 * Update our appearance depending on whether the underlying diagram is dirty,
	 * that is structurally different than what is being shown in the designer UI.
	 */
	public void updateUI(boolean drty) {
		logger.debugf("%s.setDirty: dirty = %s",TAG,(drty?"true":"false"));
		setItalic(drty);
		if( saveAction!=null ) saveAction.setEnabled(drty);
		refresh();
	}

	/**
	 * This method allows us to have children. Children are always EncapsulatedDiagramNodes.
	 * TODO: 
	 * @param arg0
	 * @return
	 */
	protected AbstractNavTreeNode createChildNode(ProjectResource arg0) {
		// TODO Auto-generated method stub
		return null;
	}
}
