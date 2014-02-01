/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.navtree;

import java.awt.Component;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import javax.swing.JPopupMenu;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessDiagramView;
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
	protected ExportAction exportAction = null;


	/**
	 * Constructor. A DiagnosticsNode is created initially without child resources.
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
		setIcon(IconUtil.getIcon("folder_document"));

		setItalic(context.getProject().isResourceDirty(resourceId));
		context.addProjectChangeListener(this);
	}
	
	
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		exportAction = new ExportAction(menu.getRootPane(),workspace.getActiveDiagram());
		menu.add(exportAction);
		menu.addSeparator();
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
	
	private class ExportAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final ProcessDiagramView view;
    	private final Component anchor;
	    public ExportAction(Component c,ProcessDiagramView v)  {
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
					    if( output!=null ) {
					    	log.debugf("%s: actionPerformed, dialog returned %s",TAG,output.getAbsolutePath());
					    	if(!output.exists()) {
					    		try {
					    			output.createNewFile();
					    			if( output.canWrite() ) {
					    				ObjectMapper mapper = new ObjectMapper();
					    				if(log.isDebugEnabled()) log.debugf("%s: serializeDiagram creating json ... %s",TAG,(mapper.canSerialize(SerializableDiagram.class)?"true":"false"));
					    				try{ 
					    					String json = mapper.writeValueAsString(view);
					    					FileWriter fw = new FileWriter(output,false);  // Do not append
					    					try {
					    						fw.write(json);
					    					}
					    					catch(IOException ioe) {
					    						log.warnf("%s: actionPerformed: Error writing file %s (%s)",TAG,output.getAbsolutePath(),ioe.getLocalizedMessage());
					    					}
					    					finally {
					    						fw.close();
					    					}
					    				}
					    				catch(JsonProcessingException jpe) {
					    					log.warnf("%s: Unable to serialize diagram (%s)",TAG,jpe.getMessage());
					    				}
					    			}
								    else {
								    	log.warnf("%s: actionPerformed, selected file is not writable: %s",TAG,output.getAbsolutePath());
								    }
					    		}
					    		catch (IOException ioe) {
					    			log.warnf("%s: actionPerformed: Error creating or closing file %s (%s)",TAG,output.getAbsolutePath(),ioe.getLocalizedMessage());
					    		}
					    	}
					    }
					}

				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
}
