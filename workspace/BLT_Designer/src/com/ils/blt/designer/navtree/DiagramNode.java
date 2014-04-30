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
import javax.swing.JPopupMenu;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ApplicationRequestManager;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectChangeListener;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
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
	private static final String TAG = "DiagramNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults

	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private DesignerContext context;
	private long resourceId;
	private final DiagramWorkspace workspace;
	private ImageIcon enabledIcon = null;
	private ImageIcon disabledIcon= null;
	
	private DisableAction disableAction = new DisableAction();
	private EnableAction enableAction = new EnableAction();


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
		Image img = ImageLoader.getInstance().loadImage("Block/icons/small/diagram.png",iconSize);
		if( img !=null) {
			enabledIcon = new ImageIcon(img);
			setIcon( enabledIcon);
		}
		img = ImageLoader.getInstance().loadImage("Block/icons/small/diagram_disabled.png",iconSize);
		if( img !=null) disabledIcon = new ImageIcon(img);

		setItalic(context.getProject().isResourceDirty(resourceId));
		context.addProjectChangeListener(this);
	}
	
	
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		ExportDiagramAction exportAction = new ExportDiagramAction(menu.getRootPane(),workspace.getActiveDiagram());
		SaveDiagramAction saveAction = new SaveDiagramAction();
		menu.add(exportAction);
		ApplicationRequestManager handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
		if( handler.isDiagramEnabled(new Long(context.getProject().getId()),new Long(resourceId)) ) {
			enableAction.setEnabled(false);
		}
		else {
			disableAction.setEnabled(false);
		}
		menu.addSeparator();
		menu.add(renameAction);
		menu.add(saveAction);
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
	public Icon getIcon() {
		Icon icon = enabledIcon;
		// Problem because diagram may not be active yet ...
		//if(!workspace.getActiveDiagram().isEnabled()) icon = disabledIcon;
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
    private class DisableAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public DisableAction()  {
	    	super(PREFIX+".DisableDiagram",IconUtil.getIcon("disk_play")); 
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				ApplicationRequestManager handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
				handler.enableDiagram(new Long(context.getProject().getId()),new Long(resourceId),Boolean.FALSE);
				this.setEnabled(false);
				enableAction.setEnabled(true);
			} 
			catch (Exception ex) {
				log.warnf("%s: DisableAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(ex);
			}
		}
	}
    
    private class EnableAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public EnableAction()  {
	    	super(PREFIX+".EnableDiagram",IconUtil.getIcon("disk_forbidden")); 
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				ApplicationRequestManager handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
				handler.startController();
				this.setEnabled(false);
				disableAction.setEnabled(true);
			} 
			catch (Exception ex) {
				log.warnf("%s: startAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(ex);
			}
		}
	}
	private class ExportDiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Export Diagram";
    	private final ProcessDiagramView view;
    	private final Component anchor;
	    public ExportDiagramAction(Component c,ProcessDiagramView v)  {
	    	super(PREFIX+".ExportDiagram",IconUtil.getIcon("export1")); 
	    	anchor = c;
	    	view=v;
	    }
	    
		public void actionPerformed(ActionEvent e) {
		
			if( view==null ) return;   // Do nothing
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
					    			ObjectMapper mapper = new ObjectMapper();
					    			if(log.isDebugEnabled()) log.debugf("%s.actionPerformed: creating json ... %s",TAG,(mapper.canSerialize(SerializableDiagram.class)?"true":"false"));
					    			try{ 
					    				// Convert the view into a serializable object
					    				SerializableDiagram sd = view.createSerializableRepresentation();
					    				String json = mapper.writeValueAsString(sd);
					    				FileWriter fw = new FileWriter(output,false);  // Do not append
					    				try {
					    					fw.write(json);
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
					    			catch(JsonProcessingException jpe) {
					    				ErrorUtil.showError("Unable to serialize diagram",POPUP_TITLE,jpe,true);
					    			}
					    		}
					    		else {
					    			ErrorUtil.showWarning(String.format("selected file (%s) is not writable.",output.getAbsolutePath()),POPUP_TITLE,false);
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
			ProjectResource res = getProjectResource();
			try {
				res.setEditCount(res.getEditCount()+1);
				context.updateResource(res);
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
			
		}
	}
}
