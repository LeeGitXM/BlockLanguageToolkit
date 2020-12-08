/**
 *   (c) 2013-2018  ILS Automation. All rights reserved.
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
import com.ils.blt.designer.AuxiliaryDataRestoreManager;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.ResourceDeleteManager;
import com.ils.blt.designer.ResourceUpdateManager;
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
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectChangeListener;
import com.inductiveautomation.ignition.common.project.ProjectResource;
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
public class DiagramTreeNode extends AbstractResourceNavTreeNode implements NavTreeNodeInterface,NotificationChangeListener,ProjectChangeListener  {
	private static final String TAG = "DiagramTreeNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	protected DesignerContext context;
	private boolean dirty = false;     
	protected final long resourceId;
	private final ExecutionManager executionEngine;
	protected final DiagramWorkspace workspace;
	private SaveDiagramAction saveAction = null;
	protected final NodeStatusManager statusManager;
	protected final ImageIcon alertBadge;
	protected final ImageIcon defaultIcon;
	protected final ImageIcon openIcon;
	protected final ImageIcon closedIcon;
	protected final ImageIcon openDisabledIcon;
	protected final ImageIcon closedDisabledIcon;
	protected final ImageIcon openRestrictedIcon;
	protected final ImageIcon closedRestrictedIcon;
//	private CutAction cutDiagramAction = null;
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
		this.context = context;
		this.executionEngine = new BasicExecutionEngine(1,TAG);
		this.resourceId = resource.getResourceId();
		this.workspace = ws;
		statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
		setName(resource.getName());
		setText(resource.getName());
		
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
//		setItalic(context.getProject().isResourceDirty(resource));  // EREIAM JH - Disabled until italic system fixed
		context.addProjectChangeListener(this);
		
		NotificationHandler notificationHandler = NotificationHandler.getInstance();
		notificationHandler.addNotificationChangeListener(NotificationKey.keyForDiagram(resourceId), TAG, this);
	}
	@Override
	public void uninstall() {
		//context.removeProjectChangeListener(this);     // (This is what FolderNode does)
	}
	
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		if( this.getParent()==null ) {
			log.errorf("%s.initPopupMenu: ERROR: Diagram (%d) has no parent",TAG,hashCode());
		}
		// If there is a diagram open that is dirty, turn off some of the options.
		boolean cleanView = true;
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
		if( tab!=null ) {
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			cleanView = !view.isDirty();
		}
		ExportDiagramAction exportAction = new ExportDiagramAction(menu.getRootPane(),resourceId, this);
		exportAction.setEnabled(cleanView);
		menu.add(exportAction);
//		DuplicateDiagramAction duplicateAction = new DuplicateDiagramAction(this);
		DeleteDiagramAction diagramDeleteAction = new DeleteDiagramAction(this);
		DebugDiagramAction debugAction = new DebugDiagramAction();
		ResetDiagramAction resetAction = new ResetDiagramAction();
		resetAction.setEnabled(cleanView);
		renameAction.setEnabled(cleanView);
		
		// States are: ACTIVE, DISABLED, ISOLATED
		DiagramState state = statusManager.getResourceState(resourceId);
		saveAction = new SaveDiagramAction(this);
		copyDiagramAction = new CopyAction(this);
//		cutDiagramAction = new CutAction(this);
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
		menu.add(saveAction);
		menu.addSeparator();
		menu.add(copyDiagramAction);
//		menu.add(cutDiagramAction);
//		menu.add(duplicateAction);
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
		log.debugf("%s.closeAndCommit: res %d",TAG,resourceId);
		if( workspace.isOpen(resourceId) ) {
			DesignableContainer c = workspace.findDesignableContainer(resourceId);
			BlockDesignableContainer container = (BlockDesignableContainer)c;
			ProcessDiagramView diagram = (ProcessDiagramView)container.getModel();
			diagram.setDirty(false);
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
		log.infof("%s.saveOpenDiagram: res %d",TAG,resourceId);
		// If the diagram is open on a tab, call the workspace method to update the project resource
		// from the diagram view. This method handles re-paint of the background.

		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
		if( tab!=null ) {

			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			for( Block blk:view.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				pbv.setDirty(false);  // Suppresses the popup?
			}
			//view.registerChangeListeners();
			workspace.saveDiagramResource(tab);
			
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
	public void setIcon(Icon icon) { super.setIcon(icon); }  // Make public
	
	@Override
	public long getResourceId() { return this.resourceId; }
	
	/**
	 * This is the resource ..
	 */
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
		DiagramState ds = statusManager.getResourceState(resourceId);
		if( workspace.isOpen(resourceId) ) {
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
		workspace.open(resourceId);
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
		if (!NAME_PATTERN.matcher(newTextValue).matches()) {
			ErrorUtil.showError(BundleUtil.get().getString(PREFIX+".InvalidName", newTextValue));
			return;
		}
		String oldName = getProjectResource().getName();
		try {
			log.infof("%s.onEdit: alterName from %s to %s",TAG,oldName,newTextValue);
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
		log.debug(TAG+".projectUpdated "+diff.getDescription());
		if (diff.isResourceDirty(resourceId) && !diff.isResourceDeleted(resourceId)) {
			log.infof("%s.projectUpdated, setting name ...",TAG);
			setName(diff.getResource(resourceId).getName());
			refresh();
		}
	}
	
	/**
	 * We got here from either a Save() action or a name change. We don't have children, so no worry about
	 * recreate() after delete. Be careful not to update a project resource here, else we get a hard loop.
	 */
	@Override
	public void projectResourceModified(ProjectResource res,ResourceModification changeType) {
		if (res.getResourceId() == resourceId) {
			log.debugf("%s.projectResourceModified.%s: %s(%d), res %s(%d)",TAG,changeType.name(),getName(),this.resourceId,res.getName(),res.getResourceId());
			if( res.getName()==null || !res.getName().equals(getName()) ) {
				setName(res.getName());
				setText(res.getName());
			}
		}
	}

	// copy the currently selected node UUID to the clipboard
	private class CopyAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode parentNode;

		public CopyAction(AbstractResourceNavTreeNode pNode)  {
			super(PREFIX+".CopyNode",IconUtil.getIcon("copy"));
			this.parentNode = pNode;
		}

		public void actionPerformed(ActionEvent e) {
           final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

           ProjectResource res = parentNode.getProjectResource();
           if (parentNode instanceof GeneralPurposeTreeNode) {
        	   executionEngine.executeOnce(new AuxiliaryDataRestoreManager(workspace,(GeneralPurposeTreeNode)parentNode));
           }
           
           String data = ""+res.getResourceId();
           Transferable t =  new StringSelection(GeneralPurposeTreeNode.BLT_COPY_OPERATION + data);
				   
		   if (t != null) {
			   try { 
				   clipboard.setContents(t, null); 
			   } catch (Exception ex) {
				   ErrorUtil.showError(String.format("actionPerformed: Unhandled Exception (%s)",ex.getMessage()), "Copy Diagram");
			   }
		   }
				   
		}
	}

//	// copy the currently selected node UUID to the clipboard
//	private class CutAction extends BaseAction {
//		private static final long serialVersionUID = 1L;
//		private final AbstractResourceNavTreeNode parentNode;
//
//		public CutAction(AbstractResourceNavTreeNode pNode)  {
//			super(PREFIX+".CutNode",IconUtil.getIcon("cut"));
//			this.parentNode = pNode;
//		}
//
//		public void actionPerformed(ActionEvent e) {
//	       final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
//
//	       ProjectResource res = parentNode.getProjectResource();
//           String data = ""+res.getResourceId();
//           
//           Transferable t =  new StringSelection(GeneralPurposeTreeNode.BLT_CUT_OPERATION + data);
//				   
//		   if (t != null) {
//			   try { 
//				   clipboard.setContents(t, null); 
//			   } catch (Exception ex) {
//				   ErrorUtil.showError(String.format("actionPerformed: Unhandled Exception (%s)",ex.getMessage()), "Cut Diagram");
//			   }
//		   }
//				   
//		}
//	}

	
	
//	private class DuplicateDiagramAction extends BaseAction {
//		private static final long serialVersionUID = 1L;
//		private final AbstractResourceNavTreeNode parentNode;
//		public DuplicateDiagramAction(AbstractResourceNavTreeNode pNode)  {
//			super(PREFIX+".DuplicateNode",IconUtil.getIcon("copy"));  // preferences
//			this.parentNode = pNode;
//		}
//
//		public void actionPerformed(ActionEvent e) {
//			try {
//				EventQueue.invokeLater(new Runnable() {
//					public void run() {
//						long newId;
//
//						try {	
//							newId = context.newResourceId();
//							ProjectResource res = parentNode.getProjectResource();
//							byte[] bytes = res.getData();
//							// It would be nice to simply use the clone constructor, but
//							// unfortunately, we have to replace all UUIDs with new ones
//							ObjectMapper mapper = new ObjectMapper();
//							mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
//							SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
//							if( sd!=null ) {
//								UUIDResetHandler uuidHandler = new UUIDResetHandler(sd);
//								uuidHandler.convertUUIDs();
//								sd.setDirty(true);    // Dirty because gateway doesn't know about it yet
//								sd.setState(DiagramState.DISABLED);
//								String json = mapper.writeValueAsString(sd);
//								GeneralPurposeTreeNode grandparent = (GeneralPurposeTreeNode)parentNode.getParent();
//								ProjectResource resource = new ProjectResource(newId,
//										BLTProperties.MODULE_ID, BLTProperties.DIAGRAM_RESOURCE_TYPE,
//										grandparent.nextFreeName(grandparent,sd.getName()), ApplicationScope.GATEWAY, json.getBytes());
//								
//								resource.setParentUuid(grandparent.getUUID());
//								new ResourceCreateManager(resource).run();	
//								grandparent.selectChild(new long[] {newId} );
//								statusManager.setResourceState(newId, sd.getState(),false);
//							}
//							else {
//								ErrorUtil.showWarning(String.format("Failed to deserialize diagram (%s)",res.getName()),"Clone Diagram");
//							}
//						}
//						catch( IOException ioe) {
//							// Should never happen, we just picked this off a chooser
//							log.warnf("%s: actionPerformed, IOException(%s)",TAG,ioe.getLocalizedMessage()); 
//						}
//						catch (Exception ex) {
//							log.errorf("%s: actionPerformed: Unhandled Exception (%s)",TAG,ex.getMessage());
//						}
//					}
//				});
//			} 
//			catch (Exception err) {
//				ErrorUtil.showError(TAG+" Exception cloning diagram",err);
//			}
//		}
//	} 
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
    	public ExportDiagramAction(Component c,long resid, DiagramTreeNode nodeIn)  {
    		super(PREFIX+".ExportDiagram",IconUtil.getIcon("export1")); 
    		anchor = c;
    		node = nodeIn;
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
    						log.debugf("%s.actionPerformed: dialog returned %s",TAG,output.getAbsolutePath());
    						try {
    							if(output.exists()) {
    								output.setWritable(true); 
    							}
    							else {
    								output.createNewFile();
    							}

    							if( output.canWrite() ) {
    								// restore auxiliary data so it gets included in the export.  
    								//  The parent of a diagram has to be a GeneralPurposeTreeNode.
    								executionEngine.executeOnce(new AuxiliaryDataRestoreManager(workspace, (GeneralPurposeTreeNode)node.getParent()));
    		    					
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
	    	if(confirmDelete(selected)) {
	    		deleter.acquireResourcesToDelete();
	    		if( execute() ) {
	    			UndoManager.getInstance().add(this,GeneralPurposeTreeNode.class);

	    			AbstractNavTreeNode p = node.getParent();
	    			if( p instanceof GeneralPurposeTreeNode )  {
	    				GeneralPurposeTreeNode parentNode = (GeneralPurposeTreeNode)p;
	    				parentNode.recreate();
	    				parentNode.expand();
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
			// Get the diagramId from the resource. We have to search the list of diagrams for a resource match.
			String projectName = context.getProject().getName();
			List<SerializableResourceDescriptor> diagramDescriptors = handler.listDiagramDescriptors(projectName);
			for(SerializableResourceDescriptor srd:diagramDescriptors ) {
				if( srd.getResourceId()==resourceId ) {
					handler.resetDiagram(srd.getId());
					break;
				}
			}
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
//				pr.setLocked(false);  // doesn't help
				node.setItalic(false);
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
			ProjectResource res = context.getProject().getResource(resourceId);
			BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
			String uuidString = "";
			if( tab!=null ) {
				log.infof("%s.setDiagramState: %s now %s (open)",TAG, tab.getName(),state.name());
				ProcessDiagramView view = (ProcessDiagramView)(tab.getModel());
				view.setState(state);		// Simply sets the view state
				tab.setBackground(view.getBackgroundColorForState());
				uuidString = view.getId().toString();
			}
			// Otherwise we need to de-serialize and get the UUID
			else {
				byte[]bytes = res.getData();
				SerializableDiagram sd = null;
				ObjectMapper mapper = new ObjectMapper();
				sd = mapper.readValue(bytes,SerializableDiagram.class);
				uuidString = sd.getId().toString();
			}
			// Inform the gateway of the state and let listeners update the UI
			ApplicationRequestHandler arh = new ApplicationRequestHandler();
			arh.setDiagramState(uuidString, state.name());
			statusManager.setResourceState(resourceId,state,true);
			setDirty(false);
			setIcon(getIcon());
			refresh();
		} 
		catch (Exception ex) {
			log.warn(String.format("%s.setStateAction: ERROR: %s",TAG,ex.getMessage()),ex);
			ErrorUtil.showError(TAG+" Exception setting state",ex);
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
			log.info("Diagram: "+view.getDiagramName()+" ("+view.getId().toString()+")");
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
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
		if( tab!=null ) {
			// If the diagram is open on a tab, call the workspace method to update the project resource
			// from the diagram view. This method handles re-paint of the background.
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			log.info("Diagram: "+view.getDiagramName()+" ("+view.getId().toString()+")");
			ApplicationRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getApplicationRequestHandler();
			try {
				List <SerializableBlockStateDescriptor> descriptors = handler.listBlocksInDiagram(view.getId().toString());
				for( SerializableBlockStateDescriptor descriptor : descriptors ) {
					Map<String,String> attributes = descriptor.getAttributes();
					String clss = attributes.get(BLTProperties.BLOCK_ATTRIBUTE_CLASS);
					String uid = attributes.get(BLTProperties.BLOCK_ATTRIBUTE_ID);
					log.info("Block: "+descriptor.getName()+"\t"+clss+"\t("+uid+")");
				}
			} 
			catch (Exception ex) {
				log.warnf("%s. startAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(TAG+" Exception listing diagram components",ex);
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
	 * Update our appearance depending on whether the underlying diagram is dirty,
	 * that is structurally different than what is being shown in the designer UI.
	 */
	public void updateUI(boolean drty) {
		log.debugf("%s.setDirty: dirty = %s",TAG,(drty?"true":"false"));
//		setItalic(drty);     // EREIAM JH - Disabled until italic system fixed
		if( saveAction!=null ) saveAction.setEnabled(drty);
		refresh();
	}

	/**
	 * This method allows us to have children. Children are always EncapsulatedDiagramNodes.
	 * As of yet we do not support encapsulated diagrams. 
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
	public void diagramStateChange(long resId, String state) {
		try {
			DiagramState ds = DiagramState.valueOf(state);
			statusManager.setResourceState(resourceId, ds,false);
			// Force repaints of both NavTree and workspace
			refresh();
			BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
			if( tab!=null ) {
				ProcessDiagramView view = (ProcessDiagramView)(tab.getModel());
				view.setState(ds);  // There are no side effects
				tab.setBackground(view.getBackgroundColorForState());
				SwingUtilities.invokeLater(new WorkspaceRepainter());
			}
		}
		catch(IllegalArgumentException iae) {
			log.warnf("%s.diagramStateChange(%d): Illegal diagram state (%s)", TAG,resourceId,state);
		}
	}
	@Override
	public void bindingChange(String binding) {
	}
	// This is the name of a block, so doesn't appear in the tree
	@Override
	public void nameChange(String name) {
	}
	@Override
	public void valueChange(QualifiedValue value) {

	}
	@Override
	public void watermarkChange(String newWatermark) {
	}


	
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
		//  update the name now so it doens't cause duplicate name problems on save
		return success;
	}
	



}
