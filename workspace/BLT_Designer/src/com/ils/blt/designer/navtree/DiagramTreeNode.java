/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *  Based on sample code provided by Inductive Automation.
 */
package com.ils.blt.designer.navtree;

import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.UUID;

import javax.swing.JPopupMenu;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.UUIDResetHandler;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.PropertiesRequestHandler;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.FolderNode;
import com.inductiveautomation.ignition.designer.navtree.model.ResourceDeleteAction;
/**
 * A folder in the designer scope to support the diagnostics toolkit diagram
 * layout. The folder depth is two or three. Menu options vary depending on whether
 * this is the root node, or not. Labels depend on the depth.
 */
public class DiagramTreeNode extends FolderNode {
	private static final String TAG = "DiagramTreeNode";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private static final int DIAGRAM_DEPTH = 2;                        // For a two-tier menu
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	// These are the various actions beyond defaults
	private DebugAction debugAction = null;
	private ApplicationAction applicationAction = null;
	private FamilyAction familyAction = null;
	protected CloneAction cloneAction = null;
	protected DiagramAction diagramAction = null;
	protected ImportAction importAction = null;
	protected StartAction startAction = null;
	protected StopAction stopAction = null;
	private final DiagramWorkspace workspace; 
	

	/** 
	 * Create a new folder node representing the root folder
	 * @param ctx the designer context
	 */
	public DiagramTreeNode(DesignerContext ctx) {
		super(ctx, BLTProperties.MODULE_ID, ApplicationScope.GATEWAY,BLTProperties.ROOT_FOLDER_UUID);
		workspace = ((BLTDesignerHook)ctx.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		setText(BundleUtil.get().getString(PREFIX+".RootFolderName"));
		setIcon(IconUtil.getIcon("folder_closed"));
		log.info(TAG+"root:"+this.pathToRoot());	
	}

	/**
	 * This version of the constructor is used for all except the root. Create
	 * either a family container or a diagram holder.
	 * 
	 * NOTE: At this point the depth is unknown. We wait until setting edit actions
	 *       to actually define the depth-based actions.
	 * 
	 * @param context the designer context
	 * @param resource the project resource
	 */
	public DiagramTreeNode(DesignerContext context,ProjectResource resource) {
		super(context, resource);

		workspace = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		setIcon(IconUtil.getIcon("folder_closed"));
	}

	private boolean isRootFolder() {
		return getFolderId().equals(BLTProperties.ROOT_FOLDER_UUID);
	}


	/**
	 * Create a child node because we've discovered a resource that matches this instance as a parent
	 * based on its content matching the our UUID.
	 */
	@Override
	protected AbstractNavTreeNode createChildNode(ProjectResource res) {
		log.debug(String.format("%s.createChildNode type:%s, level=%d", TAG,res.getResourceType(),getDepth()));
		AbstractNavTreeNode node = null;
		if (ProjectResource.FOLDER_RESOURCE_TYPE.equals(res.getResourceType())) {
			node = new DiagramTreeNode(context, res);
			log.debug(TAG+"createChildFolder:"+this.pathToRoot()+"->"+node.pathToRoot());
			return node;
		}
		else if (BLTProperties.MODEL_RESOURCE_TYPE.equals(res.getResourceType())) {
			node = new DiagramNode(context,res,workspace);
			log.debug(TAG+"createChildPanel:"+this.pathToRoot()+"->"+node.pathToRoot());
			return node;
		} 
		else {
			log.warnf("%s: Attempted to create a child of type %s (ignored)",TAG,res.getResourceType());
			throw new IllegalArgumentException();
		}
	}
	
	@Override
	public String getWorkspaceName() {
		return DiagramWorkspace.key;
	}
	
	@Override
	public boolean isEditActionHandler() {
		return isRootFolder();
	}
	/**
	 * Define the menu used for popups. This appears to be called only once for each node.
	 */
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		
		if (isRootFolder()) { 
			PropertiesRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();

			applicationAction = new ApplicationAction(this.folderId);
			startAction = new StartAction();
			stopAction = new StopAction();
			debugAction = new DebugAction();
			if( handler.isControllerRunning() ) {
				startAction.setEnabled(false);
			}
			else {
				stopAction.setEnabled(false);
			}
			menu.add(applicationAction);
			menu.add(startAction);
			menu.add(stopAction);
			menu.addSeparator();
			menu.add(debugAction);
		}
		else if( getDepth()==DIAGRAM_DEPTH) {
			diagramAction = new DiagramAction();
			importAction = new ImportAction();
			cloneAction = new CloneAction();
			menu.add(diagramAction);
			menu.add(importAction);
			menu.add(cloneAction);
			menu.addSeparator();
			addEditActions(menu);
			
		}
		else {   // Depth == 2 and DIAGRAM_DEPTH==3
			familyAction = new FamilyAction(this.folderId);
			menu.add(familyAction);
			menu.addSeparator();
			addEditActions(menu);
		}
	}
	/**
	 * Exclude cut and paste which are currently not supported.
	 */
	@Override
	protected void addEditActions(JPopupMenu menu)
    {
        menu.add(renameAction);
        menu.add(deleteAction);
    }
	
	private boolean siblings(List<AbstractNavTreeNode> nodes) {
		if (nodes == null || nodes.size() < 1) {
			return false;
		}
		int depth = nodes.get(0).getDepth();
		for (AbstractNavTreeNode node : nodes) {
			if (node.getDepth() != depth) {
				return false;
			}
		}
		return true;
	}

	@Override
	public boolean canDelete(List<AbstractNavTreeNode> selectedChildren) {
		return isEditActionHandler() && siblings(selectedChildren);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void doDelete(List<? extends AbstractNavTreeNode> children,
			DeleteReason reason) {
		for (AbstractNavTreeNode node : children) {
			if (node instanceof DiagramNode) {
				((DiagramNode) node).closeAndCommit();
			}
		}

		ResourceDeleteAction delete = new ResourceDeleteAction(context,
				(List<AbstractResourceNavTreeNode>) children,
				reason.getActionWordKey(), (getDepth()==1? (PREFIX+".ApplicationNoun"):(PREFIX+".FamilyNoun")));
		if (delete.execute()) {
			UndoManager.getInstance().add(delete, DiagramTreeNode.class);
		}
	}

	@Override
	public void onSelected() {
		UndoManager.getInstance()
				.setSelectedContext(DiagramTreeNode.class);
	}
	
	/**
	 *  Serialize a diagram into JSON.
	 * @param diagram to be serialized
	 */ 
	private String serializeDiagram(SerializableDiagram diagram) {
		String json = "";
		ObjectMapper mapper = new ObjectMapper();
		log.infof("%s: serializeDiagram creating json ... %s",TAG,(mapper.canSerialize(SerializableDiagram.class)?"true":"false"));
		try{ 
		    json = mapper.writeValueAsString(diagram);
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s: Unable to serialize diagram (%s)",TAG,jpe.getMessage());
		}
		log.infof("%s: serializeDiagram created json ... %s",TAG,json);
		return json;
	}
	
	// From the root node, recursively log the contents of the tree
	private class DebugAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public DebugAction()  {
			super(PREFIX+".Debug",IconUtil.getIcon("bug_yellow"));
		}

		public void actionPerformed(ActionEvent e) {
			log.info("=============================== Project Resources =========================");
			listAllResources();
			log.info("===========================================================================");
		}
	}
	// From the root node, create a folder for diagrams belonging to a family
	private class ApplicationAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private UUID parent;
	    public ApplicationAction(UUID parentUUID)  {
	    	super(PREFIX+".NewApplication",IconUtil.getIcon("folder_new"));
	    	this.parent = parentUUID;
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".DefaultNewApplicationName");
				if( newName==null) newName = "New Apps";  // Missing Resource
				context.addFolder(newId,moduleId,ApplicationScope.GATEWAY,newName,parent);
				selectChild(newId);
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
	// From the root node, create a folder for diagrams belonging to a family
	private class FamilyAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private UUID parent;
	    public FamilyAction(UUID parentUUID)  {
	    	super(PREFIX+".NewFamily",IconUtil.getIcon("folder_new"));
	    	this.parent = parentUUID;
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".DefaultNewFamilyName");
				if( newName==null) newName = "New Folks";  // Missing Resource
				context.addFolder(newId,moduleId,ApplicationScope.GATEWAY,newName,parent);
				selectChild(newId);
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
	// Create a new diagram
    private class DiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public DiagramAction()  {
	    	super(PREFIX+".NewDiagram",IconUtil.getIcon("folder_new"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".DefaultNewDiagramName");
				if( newName==null) newName = "New Diag";  // Missing string resource
				SerializableDiagram diagram = new SerializableDiagram();
				diagram.setName(newName);
				log.infof("%s: new diagram action ...",TAG);

				String json = serializeDiagram(diagram);
			
				
				log.debugf("%s: DiagramAction. json=%s",TAG,json);
				byte[] bytes = json.getBytes();
				log.debugf("%s: DiagramAction. create new %s resource %d (%d bytes)",TAG,BLTProperties.MODEL_RESOURCE_TYPE,
						newId,bytes.length);
				ProjectResource resource = new ProjectResource(newId,
						BLTProperties.MODULE_ID, BLTProperties.MODEL_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				context.updateResource(resource);
				selectChild(newId);
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						workspace.open(newId);
					}
				});
				
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
    
    
    private class CloneAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Clone Diagram";
	    public CloneAction()  {
	    	super(PREFIX+".CloneDiagram",IconUtil.getIcon("copy"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						long newId;

						try {
							
							newId = context.newResourceId();

							workspace.open(newId);
							String newName = BundleUtil.get().getString(PREFIX+".DefaultImportDiagramName");
							if( newName==null) newName = "Imported Diag";  // Missing string resource
							ImportDialog dialog = new ImportDialog(newName);
							dialog.pack();
							dialog.setVisible(true);   // Returns when dialog is closed
							File input = dialog.getFilePath();
							if( input!=null ) {
								if( input.exists() && input.canRead()) {
									try {
										// Note: Requires Java 1.7
										byte[] bytes = Files.readAllBytes(input.toPath());
										ProjectResource resource = new ProjectResource(newId,
												BLTProperties.MODULE_ID, BLTProperties.MODEL_RESOURCE_TYPE,
												newName, ApplicationScope.GATEWAY, bytes);
										resource.setParentUuid(getFolderId());
										context.updateResource(resource);
										selectChild(newId);

									}
									catch( FileNotFoundException fnfe) {
										// Should never happen, we just picked this off a chooser
										log.warnf("%s: actionPerformed, File not found %s (%s)",TAG,input.getAbsolutePath(),fnfe.getLocalizedMessage()); 
									}
									catch( IOException ioe) {
										// Should never happen, we just picked this off a chooser
										log.warnf("%s: actionPerformed, IOException %s (%s)",TAG,input.getAbsolutePath(),ioe.getLocalizedMessage()); 
									}

								}
								else {
									log.warnf("%s: actionPerformed, selected file does not exist of is not readable: %s",TAG,input.getAbsolutePath());
								}
							}  // Cancel
						} 
						catch (Exception ex) {
							log.errorf("%s: actionPerformed: Unhandled Exception (%s)",TAG,ex.getMessage());
						}
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
    private class ImportAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private final static String POPUP_TITLE = "Import Diagram";
	    public ImportAction()  {
	    	super(PREFIX+".ImportDiagram",IconUtil.getIcon("import1"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						boolean success = false;
						long newId;

						try {
							newId = context.newResourceId();
							String newName = BundleUtil.get().getString(PREFIX+".DefaultImportDiagramName");
							if( newName==null) newName = "Imported Diag";  // Missing string resource
							ImportDialog dialog = new ImportDialog(newName);
							dialog.pack();
							dialog.setVisible(true);   // Returns when dialog is closed
							File input = dialog.getFilePath();
							newName = dialog.getDiagramName();
							if( input!=null ) {
								if( input.exists() && input.canRead()) {
									try {
										// Note: Requires Java 1.7
										byte[] bytes = Files.readAllBytes(input.toPath());
										// It would be nice to simply convert to a resource.
										// Unfortunately we have to replace all UUIDs with new ones
										ObjectMapper mapper = new ObjectMapper();
										SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
										if( sd!=null ) {
											UUIDResetHandler handler = new UUIDResetHandler(sd);
											handler.convertUUIDs();
											String json = mapper.writeValueAsString(sd);
											ProjectResource resource = new ProjectResource(newId,
													BLTProperties.MODULE_ID, BLTProperties.MODEL_RESOURCE_TYPE,
													newName, ApplicationScope.GATEWAY, json.getBytes());
											resource.setParentUuid(getFolderId());
											context.updateResource(resource);
											selectChild(newId);
										}
										else {
											ErrorUtil.showWarning(String.format("Failed to deserialize file (%s)",input.getAbsolutePath()),POPUP_TITLE);
										}
									}
									catch( FileNotFoundException fnfe) {
										// Should never happen, we just picked this off a chooser
										ErrorUtil.showWarning(String.format("File %s not found",input.getAbsolutePath()),POPUP_TITLE); 
									}
									catch( IOException ioe) {
										ErrorUtil.showWarning(String.format("IOException (%s)",ioe.getLocalizedMessage()),POPUP_TITLE); 
									}
									catch(Exception ex) {
										ErrorUtil.showError(String.format("Deserialization exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
									}

								}
								else {
									ErrorUtil.showWarning(String.format("Selected file does not exist or is not readable: %s",input.getAbsolutePath()),POPUP_TITLE);
								}
							}  // Cancel
						} 
						catch (Exception ex) {
							ErrorUtil.showError(String.format("Unhandled Exception (%s)",ex.getMessage()),POPUP_TITLE,ex,true);
						}
						// No need to inform of success, we'll see the new diagram
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
    // Start refers to a global startup of the Execution controller in the Gateway
    private class StartAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public StartAction()  {
	    	super(PREFIX+".StartExecution",IconUtil.getIcon("disk_play"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				PropertiesRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
				handler.startController();
				this.setEnabled(false);
				stopAction.setEnabled(true);
			} 
			catch (Exception ex) {
				log.warnf("%s: startAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(ex);
			}
		}
	}
    private class StopAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public StopAction()  {
	    	super(PREFIX+".StopExecution",IconUtil.getIcon("disk_forbidden"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				PropertiesRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
				handler.stopController();
				this.setEnabled(false);
				startAction.setEnabled(true);
			}
			catch(Exception ex) {
				log.warnf("%s: stopAction: ERROR: %s",TAG,ex.getMessage(),ex);
				ErrorUtil.showError(ex);
			}
		}
	}
    /**
	 * Search the project for all resources. This is for debugging.
	 */
	public void listAllResources() {
		List <ProjectResource> resources = context.getProject().getResources();
		for( ProjectResource res : resources ) {
			log.info("Res: "+res.getResourceId()+" "+res.getResourceType()+" "+res.getModuleId()+" ("+res.getName()+
					":"+res.getParentUuid()+")");
		}
	}
	
	private byte[] longsToByteArray(long most,long least) {
		byte[] byteArray = new byte[16];
	    int i = 0;
	    while (i < 16)
	    {
	      int j;
	      if (i == 0)
	        j = (int)most >>> 32;
	      else if (i == 4)
	        j = (int)most;
	      else if (i == 8)
	        j = (int)least >>> 32;
	      else
	        j = (int)least;
	      byteArray[(i++)] = ((byte)(j >>> 24));
	      byteArray[(i++)] = ((byte)(j >>> 16));
	      byteArray[(i++)] = ((byte)(j >>> 8));
	      byteArray[(i++)] = ((byte)j);
	    }
	    return byteArray;
	}
}
