/**
 *   (c) 2013-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.navtree;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.client.designable.DesignableContainer;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.execution.ExecutionManager;
import com.inductiveautomation.ignition.common.execution.impl.BasicExecutionEngine;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.ProjectResourceListener;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.ResourceDeleteAction;

/**
 * A DiagramNode appears as leaf node in the BLT NavTree hierarchy.
 * It serves as a Nav-tree standin for a DiagramWorkspace. A DiagramNode
 * has no children.
 * 
 * The frame is responsible for rendering the diagram based on the model resource.
 * The model can exist without the frame, but not vice-versa.
 */
public class DiagramTreeNode extends AbstractResourceNavTreeNode implements NavTreeNodeInterface,NotificationChangeListener,ProjectResourceListener  {
	private static final String CLSS = "DiagramTreeNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults     
	private final ApplicationRequestHandler requestHandler;
	protected final DiagramWorkspace workspace;
	protected final ExecutionManager executor;
	protected final NodeStatusManager statusManager;
	protected final ImageIcon alertBadge;
	protected final ImageIcon defaultIcon;
	protected final ImageIcon openIcon;
	protected final ImageIcon closedIcon;
	protected final ImageIcon openDisabledIcon;
	protected final ImageIcon closedDisabledIcon;
	protected final ImageIcon openRestrictedIcon;
	protected final ImageIcon closedRestrictedIcon;
	private CopyAction copyDiagramAction = null;

	/**
	 * Constructor. A DiagramTreeNode is created initially without child resources.
	 *      The model resource either pre-exists or is created when a new frame is
	 *      instantiated.
	 * @param context designer context
	 * @param resource panel resource 
	 * @param ws the tabbed workspace holding the diagrams
	 */
	public DiagramTreeNode(DesignerContext context,ProjectResource resource,DiagramWorkspace ws) {
		super(context,resource.getResourcePath());
		this.workspace = ws;
		this.executor = new BasicExecutionEngine();
		this.requestHandler = new ApplicationRequestHandler();
		statusManager = NodeStatusManager.getInstance();
		setName(resource.getResourceName());
		setText(resource.getResourceName());
		
		alertBadge =iconFromPath("Block/icons/badges/bell.png");
		defaultIcon = IconUtil.getIcon("unknown");
		openIcon = iconFromPath("Block/icons/navtree/diagram.png");
		// We have just defined the default (expanded) variant. Here are some more.
		closedIcon = iconFromPath("Block/icons/navtree/diagram_closed.png");
		openDisabledIcon = iconFromPath("Block/icons/navtree/diagram_disabled.png");
		closedDisabledIcon = iconFromPath("Block/icons/navtree/diagram_closed_disabled.png");
		openRestrictedIcon = iconFromPath("Block/icons/navtree/diagram_isolated.png");
		closedRestrictedIcon = iconFromPath("Block/icons/navtree/diagram_closed_isolated.png");
		setIcon( closedIcon);
		context.getProject().addProjectResourceListener(this);
		
		NotificationHandler notificationHandler = NotificationHandler.getInstance();
		notificationHandler.addNotificationChangeListener(NotificationKey.keyForDiagram(resourceId), CLSS, this);
		statusManager.createResourceStatus(this, resourceId);
	}
	
	@Override
	public void setName(String name) {
		super.setName(name);
	}
	@Override
	public void uninstall() {
		context.getProject().removeProjectResourceListener(this);     // (This is what FolderNode does)
	}
	
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		if( this.getParent()==null ) {
			log.errorf("%s.initPopupMenu: ERROR: Diagram (%d) has no parent",CLSS,hashCode());
		}
		// If there is a diagram open that is dirty, turn off some of the options.
		boolean cleanView = true;
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId.getResourcePath());
		if( tab!=null ) {
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			cleanView = !view.isChanged();
		}
		ExportDiagramAction exportAction = new ExportDiagramAction(menu.getRootPane(),resourceId, this);
		exportAction.setEnabled(cleanView);
		menu.add(exportAction);
		DeleteDiagramAction deleteAction = new DeleteDiagramAction(this);
		DebugDiagramAction debugAction = new DebugDiagramAction();
		ResetDiagramAction resetAction = new ResetDiagramAction();
		resetAction.setEnabled(cleanView);
		renameAction.setEnabled(true);
		
		// States are: ACTIVE, DISABLED, ISOLATED
		DiagramState state = statusManager.getPendingState(resourceId);
		if( state==null) state = requestHandler.getDiagramState(resourceId);
		copyDiagramAction = new CopyAction(this);
		SetStateAction ssaActive = new SetStateAction(DiagramState.ACTIVE);
		ssaActive.setEnabled(!state.equals(DiagramState.ACTIVE));
		SetStateAction ssaDisable = new SetStateAction(DiagramState.DISABLED);
		ssaDisable.setEnabled(!state.equals(DiagramState.DISABLED));
		SetStateAction ssaIsolated = new SetStateAction(DiagramState.ISOLATED);
		ssaIsolated.setEnabled(!state.equals(DiagramState.ISOLATED));
		JMenu setStateMenu = new JMenu(BundleUtil.get().getString(PREFIX+".SetState"));
		setStateMenu.setEnabled(cleanView);
		setStateMenu.add(ssaActive);
		setStateMenu.add(ssaDisable);
		setStateMenu.add(ssaIsolated);
		menu.add(setStateMenu);
		menu.addSeparator();
		menu.add(copyDiagramAction);
		menu.add(renameAction);
        menu.add(deleteAction);
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
		log.debugf("%s.closeAndCommit: res %d",CLSS,resourceId);
		if( workspace.isOpen(resourceId.getResourcePath()) ) {
			DesignableContainer c = workspace.findDesignableContainer(resourceId.getResourcePath());
			BlockDesignableContainer container = (BlockDesignableContainer)c;
			ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
			workspace.setDiagramClean(diagram);
			diagram.unregisterChangeListeners();
			workspace.close(resourceId);
		}
		setIcon(getIcon());
		refresh();
	}
	
	/**
	 *  If the diagram associated with this node is open, save its state.
	 */
	public void saveOpenDiagram() {
		log.infof("%s.saveOpenDiagram: res %d",CLSS,resourceId);
		// If the diagram is open on a tab, call the workspace method to update the project resource
		// from the diagram view. This method handles re-paint of the background.

		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId.getResourcePath());
		if( tab!=null ) {
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			for( Block blk:view.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				pbv.setDirty(false);  // Suppresses the popup?
			}
			workspace.saveOpenDiagram(resourceId);
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

	@Override 
	public void setIcon(Icon icon) { super.setIcon(icon); }  // Make public

	/**
	 * Return an icon appropriate to the diagram state and whether or not it is displayed.
	 * If there is a pending state, use it. Otherwise get the state from the Gateway.
	 * As far as we can tell getExpandedIcon is never called.
	 */
	@Override
	public Icon getIcon() {	
		icon = closedIcon;
		DiagramState ds = statusManager.getPendingState(resourceId);
		if( ds==null) ds = requestHandler.getDiagramState(resourceId);
		if( workspace.isOpen(resourceId.getResourcePath()) ) {
			icon = openIcon;
			if( ds.equals(DiagramState.DISABLED))      icon = openDisabledIcon;
			else if( ds.equals(DiagramState.ISOLATED)) icon = openRestrictedIcon;
		}
		else {
			if( ds.equals(DiagramState.DISABLED))      icon = closedDisabledIcon;
			else if( ds.equals(DiagramState.ISOLATED)) icon = closedRestrictedIcon;
		}
		if(statusManager.getAlertState(resourceId)) {
			icon = IconUtil.applyBadge(icon, alertBadge);
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
		Optional<ProjectResource> option = getProjectResource();
		ProjectResource res = option.get();
		workspace.open(res.getResourceId());
		setIcon(getIcon());  // Change icon to show we're now open
		refresh();
	}
	
	/**
	 *  Note: We ignore locking, as it basically implies that the diagram is showing
	 *        on an open tab. We take care of re-naming the tab.
	 *        Attempt to keep the collapsed state as it was
	 */
	@Override
	public void onEdit(String newTextValue) {
		// Sanitize name
		if (!isValid(newTextValue)) {
			ErrorUtil.showError(BundleUtil.get().getString(PREFIX+".InvalidName", newTextValue));
			return;
		}
		Optional<ProjectResource> option = getProjectResource();
		ProjectResource res = option.get();
		String oldName = res.getResourceName();
		try {
			log.infof("%s.onEdit: alterName from %s to %s",CLSS,oldName,newTextValue);
			alterName( newTextValue);
			statusManager.setPendingName(resourceId, name);
			if(workspace.isOpen(resourceId.getResourcePath()) ) {
				BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId.getResourcePath());
				if(tab!=null) {
					tab.setName(newTextValue);
				}
			}
		}
		catch (IllegalArgumentException ex) {
			ErrorUtil.showError(CLSS+".onEdit: "+ex.getMessage());
		}
	}
	
	// We're not a listener on anything. But we do need to
	// close the tab if we're open.
	public void prepareForDeletion() {
		closeAndCommit();
	}

	// ----------------------- Project Resource Listener -------------------------------
	/**
	 * This creates the actual resource when a new diagram node is created
	 */
	@Override
	public void resourcesCreated(String projectName,List<ChangeOperation.CreateResourceOperation> ops) {
		for(ChangeOperation.CreateResourceOperation op:ops ) {
			ProjectResourceId id = op.getResourceId();
			log.infof("%s.resourcesCreated: %s ",CLSS,id.getFolderPath());
		}
	}
	/**
	 * We can ignore deletions because we delete the resource model
	 * by deleting the panel resource.
	 */
	@Override
	public void resourcesDeleted(String projectName,List<ChangeOperation.DeleteResourceOperation> ops) {
		log.info(CLSS+".resourcesDeleted (ignore)");
	}
	
	/**
	 * We got here from either a Save() action or a name change. We don't have children, so no worry about
	 * recreate() after delete. Be careful not to update a project resource here, else we get a hard loop.
	 */
	@Override
	public void resourcesModified(String projectName,List<ChangeOperation.ModifyResourceOperation> ops) {
		for(ChangeOperation.ModifyResourceOperation op:ops ) {
			if( op.getResourceId().equals(resourceId) ) {
				log.infof("%s.resourcesModified: %s",CLSS,resourceId.getFolderPath());
				ProjectResource res = op.getResource();
				if( res.getResourceName()==null || !res.getResourceName().equals(getName()) ) {
					alterName(res.getResourceName());
					setText(res.getResourceName());
				}
			}
		}
	}

	// copy the currently selected node resourceId to the clipboard
	private class CopyAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode parentNode;

		public CopyAction(AbstractResourceNavTreeNode pNode)  {
			super(PREFIX+".CopyNode",IconUtil.getIcon("copy"));
			this.parentNode = pNode;
		}

		public void actionPerformed(ActionEvent e) {
           final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
           Optional<ProjectResource> option = parentNode.getProjectResource();
           ProjectResource res = option.get();
           String data = ""+res.getResourceId();
           Transferable t =  new StringSelection(NavTreeFolder.BLT_COPY_OPERATION + data);
				   
		   if (t != null) {
			   try { 
				   clipboard.setContents(t, null); 
			   } 
			   catch (Exception ex) {
				   ErrorUtil.showError(String.format("actionPerformed: Unhandled Exception (%s)",ex.getMessage()), "Copy Diagram");
			   }
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
			log.info("============================ Diagram (Designer) ========================");
			listDiagramComponents();
			log.info("============================ Diagram(Gateway) ==========================");
			listDiagramGatewayComponents();
			log.info("========================================================================");
		}
	}
    private class ExportDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Export Diagram";
    	private final Component anchor;
    	private DiagramTreeNode node;
    	public ExportDiagramAction(Component c,ProjectResourceId resid, DiagramTreeNode nodeIn)  {
    		super(PREFIX+".ExportDiagram",IconUtil.getIcon("export1")); 
    		anchor = c;
    		node = nodeIn;
    	}

    	public void actionPerformed(final ActionEvent e) {
    		if( resourceId==null ) return;   // Do nothing
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
    						log.debugf("%s.actionPerformed: dialog returned %s",CLSS,output.getAbsolutePath());
    						try {
    							if(output.exists()) {
    								output.setWritable(true); 
    							}
    							else {
    								output.createNewFile();
    							}

    							if( output.canWrite() ) {
    								Optional<ProjectResource> optional = DiagramTreeNode.this.getProjectResource();
    								ProjectResource res = optional.get();
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
    									ErrorUtil.showWarning(String.format("Resource %s does not exist",resourceId.getResourcePath().getPath().toString()),
    											POPUP_TITLE,false);
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
    			ErrorUtil.showError(CLSS+": Exception writing diagram.",err);
    		}
    	}
    }
    private class DeleteDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private String noun;
    	private final AbstractResourceNavTreeNode node;
    	
	    public DeleteDiagramAction(AbstractResourceNavTreeNode tnode)  {
	    	super(PREFIX+".DeleteDiagram",IconUtil.getIcon("delete")); 
	    	this.node = tnode;
	    	this.noun = PREFIX+".DiagramNoun";
	    }
	    public void actionPerformed(ActionEvent e) {
	    	List<AbstractResourceNavTreeNode> nodes = new ArrayList<>();
	    	nodes.add(node);
	    	ResourceDeleteAction deleter = new ResourceDeleteAction(context,nodes,noun);
	    	deleter.execute();
	    }
	}
	
	private class ResetDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public ResetDiagramAction()  {
	    	super(PREFIX+".ResetDiagram",IconUtil.getIcon("check2")); 
	    }
	    
		public void actionPerformed(ActionEvent e) {
			ApplicationRequestHandler handler = new ApplicationRequestHandler();
			// Get the diagramId from the resource. We have to search the list of diagrams for a resource match.
			String projectName = context.getProject().getName();
			List<SerializableResourceDescriptor> diagramDescriptors = handler.listDiagramDescriptors(projectName);
			for(SerializableResourceDescriptor srd:diagramDescriptors ) {
				if( srd.getResourceId()==resourceId ) {
					handler.resetDiagram(resourceId);
					break;
				}
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
			setDiagramState(state);
		}
	}
	/**
	 * Provide public access for the action of setting the state of a diagram.
	 * In particular this is used when recursively setting state from the application
	 * level. This is the canonical way to change diagram state.
	 * 
	 *  
	 * @param state
	 */
	public void setDiagramState(DiagramState state) {
		try {
			// Even if the diagram is showing, we need to do a save to change the state.
			// (That's why this selection is disabled when the view is dirty)
			DiagramState oldState = null;
			Optional<ProjectResource> option = getProjectResource();
			ProjectResource res = option.get();
			BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId.getResourcePath());
			ProjectResourceId viewId = null;
			if( tab!=null ) {
				log.infof("%s.setDiagramState: %s now %s (open)",CLSS, tab.getName(),state.name());
				ProcessDiagramView view = (ProcessDiagramView)(tab.getModel());
				view.setState(state);		// Simply sets the view state
				tab.setBackground(view.getBackgroundColorForState());
				viewId = view.getResourceId();
			}
			// Otherwise we need to de-serialize and get the path
			else {
				byte[]bytes = res.getData();
				SerializableDiagram sd = null;
				ObjectMapper mapper = new ObjectMapper();
				sd = mapper.readValue(bytes,SerializableDiagram.class);
				viewId = requestHandler.createResourceId(res.getProjectName(), sd.getPath().toString(), BLTProperties.DIAGRAM_RESOURCE_TYPE);
			}
			// Inform the gateway of the state and let listeners update the UI
			ApplicationRequestHandler arh = new ApplicationRequestHandler();
			arh.setDiagramState(viewId, state.name());
			statusManager.setPendingState(resourceId,state);
			setIcon(getIcon());
			refresh();
		} 
		catch (Exception ex) {
			log.warn(String.format("%s.setStateAction: ERROR: %s",CLSS,ex.getMessage()),ex);
			ErrorUtil.showError(CLSS+" Exception setting state",ex);
		}
	}
	
	/**
	 * Find the current process diagram and list its blocks.
	 */
	public void listDiagramComponents() {
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId.getResourcePath());
		if( tab!=null ) {
			// If the diagram is open on a tab, call the workspace method to update the project resource
			// from the diagram view. This method handles re-paint of the background.
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			log.info("Diagram: "+view.getDiagramName()+" ("+view.getResourceId().getResourcePath().getPath().toString()+")");
			for( Block blk:view.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				log.info("Block: "+pbv.getName()+"\t"+pbv.getClassName()+"\t("+pbv.getId().toString()+")");
			}
		}
		else {
			log.info("     Diagram must be open in tab ...");
		}
	}

	/**
	 * Query the referenced diagram in the Gateway. The blocks that it knows
	 * about may, or may not, coincide with those in the Designer. 
	 */
	public void listDiagramGatewayComponents() {
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId.getResourcePath());
		if( tab!=null ) {
			// If the diagram is open on a tab, call the workspace method to update the project resource
			// from the diagram view. This method handles re-paint of the background.
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			log.info("Diagram: "+view.getDiagramName()+" ("+view.getResourceId().getResourcePath().getPath().toString()+")");
			ApplicationRequestHandler handler = new ApplicationRequestHandler();
			try {
				List <SerializableBlockStateDescriptor> descriptors = handler.listBlocksInDiagram(view.getResourceId());
				for( SerializableBlockStateDescriptor descriptor : descriptors ) {
					Map<String,String> attributes = descriptor.getAttributes();
					String clss = attributes.get(BLTProperties.BLOCK_ATTRIBUTE_CLASS);
					String uid = attributes.get(BLTProperties.BLOCK_ATTRIBUTE_ID);
					log.info("Block: "+descriptor.getName()+"\t"+clss+"\t("+uid+")");
				}
			} 
			catch (Exception ex) {
				log.warnf("%s. startAction: ERROR: %s",CLSS,ex.getMessage(),ex);
				ErrorUtil.showError(CLSS+" Exception listing diagram components",ex);
			}
		}
		else {
			log.info("     Diagram must be open in tab ...");
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
	 * Update the node name italic/plain in nav tree.
	 * Note: This method should ONLY be called from the node status manager.
	 */
	@Override
	public void updateUI(boolean saved) {
		log.infof("%s.updateUI: %s saved = %s",CLSS,resourceId.getResourcePath().getPath().toString(),(saved?"true":"false"));
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				setItalic(!saved);
				refresh();
			}
		});
	}
	
	/**
	 * This method allows us to have children, but a diagram has no children. 
	 * @param arg0
	 * @return
	 */
	protected AbstractNavTreeNode createChildNode(ProjectResource arg0) {
		return null;
	}
	
	//============================================ Notification Change Listener =====================================
	// The value is in response to a diagram state change.
	// Do not re-inform the Gateway, since that's where this notification originated
	@Override
	public void diagramStateChange(String path, String state) {
		try {
			DiagramState ds = DiagramState.valueOf(state);
			statusManager.setPendingState(resourceId, ds);
			// Force repaints of both NavTree and workspace
			refresh();
			BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId.getResourcePath());
			if( tab!=null ) {
				ProcessDiagramView view = (ProcessDiagramView)(tab.getModel());
				view.setState(ds);  // There are no side effects
				tab.setBackground(view.getBackgroundColorForState());
				SwingUtilities.invokeLater(new WorkspaceRepainter());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.diagramStateChange(%d): Illegal diagram state (%s)", CLSS,resourceId,state);
		}
	}
	@Override
	public void bindingChange(String pname,String binding) {}
	// This is the name of a block, so doesn't appear in the tree
	@Override
	public void nameChange(String nm) {}
	@Override
	public void propertyChange(String pname,Object value) {}
	@Override
	public void valueChange(QualifiedValue value) {}
	@Override
	public void watermarkChange(String newWatermark) {}


	
	/**
	 * Do it.  (Note this will change diagnosis names to avoid collisions).
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
		//  update the name now so it doesn't cause duplicate name problems on save
		return success;
	}
	



}
