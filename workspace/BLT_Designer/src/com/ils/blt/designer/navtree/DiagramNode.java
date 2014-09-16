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
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.DiagramState;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.BLTDesignerHook;
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
 * A DiagnosticsNode appears as leaf node in the Diagnostics NavTree hierarchy.
 * It doesn't have any NavTree-type children, but it does have two nested objects, 
 * a DiagnosticsFrame and a diag-model resource. 
 * 
 * The frame is responsible for rendering the diagram based on the model resource.
 * The model can exist without the frame, but not vice-versa.
 */
public class DiagramNode extends AbstractResourceNavTreeNode implements ProjectChangeListener  {
	private static final String TAG = "DiagramNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults

	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private DesignerContext context;
	private long resourceId;
	private final DiagramWorkspace workspace;


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

		setName(resource.getName());
		setText(resource.getName());
		setIcon(IconUtil.getIcon("tag_tree"));
		Dimension iconSize = new Dimension(20,20);
		Image img = ImageLoader.getInstance().loadImage("Block/icons/navtree/diagram.png",iconSize);
		if( img !=null) {
			setIcon( new ImageIcon(img));
		}

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
		SaveDiagramAction saveAction = new SaveDiagramAction();
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
		log.infof("%s.closeAndCommit: res %d",TAG,resourceId);
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
		if( !existsInController )  return false;    // Controller has no knowledge of this resource
		
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
		if( tab!=null ) {
			ProcessDiagramView view = (ProcessDiagramView)(tab.getModel());
			if( view.isDirty() ) return true;
			for(Block blk:view.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				if(pbv.isDirty()) return true;
			}
		}
		// Otherwise we need to de-serialize just to determine dirtiness
		else {
			ProjectResource res = context.getProject().getResource(resourceId);
			byte[]bytes = res.getData();
			SerializableDiagram sd = null;
			ObjectMapper mapper = new ObjectMapper();
			try {
				sd = mapper.readValue(bytes,SerializableDiagram.class);
				if( sd.isDirty() ) return true;
				for(SerializableBlock sb:sd.getBlocks()) {
					if( sb.isDirty() ) return true;
				}
			}
			catch(Exception ex) {
				log.warnf("%s.diagramIsSavable: Exception deserializing res %d (%s)",TAG,resourceId,ex.getMessage());
			}
		}
		return false;
	}
	/**
	 * Before deleting ourself, delete the frame and model, if they exist.
	 * The children aren't AbstractNavTreeNodes ... (??)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void doDelete(List<? extends AbstractNavTreeNode> children,DeleteReason reason) {
		log.infof("%s.doDelete: res %d",TAG,resourceId);
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
	public Icon getIcon() {
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
				log.infof("%s: onEdit: alterName from %s to %s",TAG,oldName,newTextValue);
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
		log.infof("%s.saveDiagram: %d...",TAG,resourceId);
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resourceId);
		if( tab!=null ) {
			// If the diagram is open on a tab, just call the workspace method
			workspace.saveDiagram(tab);
			ProcessDiagramView view = (ProcessDiagramView)tab.getModel();
			tab.setBackground(view.getBackgroundColorForState());
			for( Block blk:view.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				pbv.setDirty(false);
			}
			view.registerChangeListeners();
		}
		else {
			// We simply save the resource, as is.
			Project diff = context.getProject().getEmptyCopy();
			ProjectResource res = getProjectResource();
			diff.putResource(res, true);    // Mark as dirty
			try {
				DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), diff, false, "Committing ..."); // Do not publish
			}
			catch(GatewayException ge) {
				log.warnf("%s.saveDiagram: Exception saving project resource %d (%s)",TAG,resourceId,ge.getMessage());
			}
		}
		setItalic(false);
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
	 *    1) Addition of a BLTProperties.MODEL_RESOURCE_TYPE with same parent as this.
	 *    2) Resource name change, we change ours to keep in sync.
	 */
	@Override
	public void projectResourceModified(ProjectResource res,ResourceModification changeType) {
		log.debug(TAG+": projectModified: "+res.getResourceId()+" "+res.getResourceType()+" "+res.getModuleId()+" ("+res.getName()+
				":"+res.getParentUuid()+")");
		if (res.getResourceId() == resourceId
				&& changeType != ResourceModification.Deleted) {
			log.infof("%s: projectResourceModified, setting name/italic + refreshing",TAG);
			setName(res.getName());
			setItalic(true);
			refresh();    // Updates the tree model
		}
	}
    
    private class ExportDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Export Diagram";
    	private final long resourceId;
    	private final Component anchor;
    	public ExportDiagramAction(Component c,long resid)  {
    		super(PREFIX+".ExportDiagram",IconUtil.getIcon("export1")); 
    		anchor = c;
    		resourceId=resid;
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
    						log.debugf("%s.actionPerformed: dialog returned %s",TAG,output.getAbsolutePath());
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
			} 
			catch (Exception ex) {
				log.warn(String.format("%s.setStateAction: ERROR: %s",TAG,ex.getMessage()),ex);
				ErrorUtil.showError(ex);
			}
		}
	}
	@Override
	protected DesignerProjectContext projectCtx() {
		return context;
	}
	
	
}
