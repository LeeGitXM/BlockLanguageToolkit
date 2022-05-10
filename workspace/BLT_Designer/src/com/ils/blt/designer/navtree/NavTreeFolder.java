/**
 *   (c) 2013-2022  ILS Automation. All rights reserved.
 *  
 *  Based on sample code provided by Inductive Automation.
 */
package com.ils.blt.designer.navtree;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Image;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.common.serializable.SerializableFolder;
import com.ils.blt.common.serializable.SerializableNodeRenameHandler;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.ResourceCreateManager;
import com.ils.blt.designer.ResourceDeleteManager;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.ProjectResourceListener;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.UndoManager;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.FolderNode;
import com.inductiveautomation.ignition.designer.project.CreateResourceDialog;
import com.inductiveautomation.ignition.designer.project.CreateResourceDialog.CreateResourceDialogBuilder;
import com.inductiveautomation.ignition.designer.project.ResourceNotFoundException;
/**
 * Edit a folder in the designer scope to support the diagnostics toolkit diagram
 * layout. In addition to standard folders, folders can be of type "Application" or
 * "Family". These hold properties special to the Diagnostics Toolkit.  Menu options 
 * vary depending on folder type. Labels are likewise dependent.
 * 
 * An OK action calls extension functions.
 * 
 * Leaf nodes are of type DiagramNode. 
 * Note that a FolderNode is an AbstractResourceNavTreeNode
 */
public class NavTreeFolder extends FolderNode implements NavTreeNodeInterface, ProjectResourceListener {
	private static final String CLSS = "NavTreeFolder";
	public static final String BLT_CUT_OPERATION = "BLTCUT";
	public static final String BLT_COPY_OPERATION = "BLTCOPY";
	private static final int OFFSET = 100;
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final static LoggerEx log = LogUtil.getLogger(NavTreeFolder.class.getPackageName());
	private final DesignerContext context;
	private DiagramState state = DiagramState.ACTIVE;  // Used for Applications and Families
	private final CopyAction copyBranchAction;
	private final PasteAction pasteBranchAction;
	private final StartAction startAction = new StartAction();
	private final StopAction stopAction = new StopAction();
	private final DiagramWorkspace workspace;
	private final SerializableNodeRenameHandler renameHandler;
	private final NodeStatusManager statusManager;
	private final FolderCreateAction folderCreateAction;
	private final ApplicationRequestHandler requestHandler;
	protected final ImageIcon alertBadge;
	private final ImageIcon defaultIcon = IconUtil.getIcon("folder_closed");
	private final ImageIcon openIcon;
	private final ImageIcon closedIcon;
	private final ImageIcon diagramIcon;

	/**
	 * A NavTreeFolder is a NavTreeNode that contains either diagrams or other folders.
	 * The root node is indicated by a resource that has no path and is named with the
	 * module name.
	 * 
	 * @param context the designer context
	 * @param resource the project resource
	 * @param self UUID of the node itself
	 */
	public NavTreeFolder(DesignerContext ctx,ProjectResource resource) {
		super(ctx,resource,ApplicationScope.DESIGNER);
		this.context = ctx;
		this.requestHandler = new ApplicationRequestHandler();
		this.renameHandler = new SerializableNodeRenameHandler();
		this.children = null;
		this.childrenLoaded = false;
		this.parent = null;
		if( isRoot() ) {
			setName(BLTProperties.ROOT_FOLDER_NAME);
		}
		else {
			setName(resource.getResourceName());      // Also sets text for tree
		}
		copyBranchAction = new CopyAction(this);
		pasteBranchAction = new PasteAction(this);
		folderCreateAction = new FolderCreateAction(this);
		
		workspace = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		statusManager = NodeStatusManager.getInstance();
		alertBadge =iconFromPath("Block/icons/badges/bell.png");

		// Simple folder
		closedIcon = IconUtil.getIcon("folder_closed");
		openIcon = IconUtil.getIcon("folder"); 
		diagramIcon = iconFromPath("Block/icons/navtree/diagram.png");
		setIcon(closedIcon);
		statusManager.createResourceStatus(this, resourceId);
	}

	// For debugging
	@Override 
	public void checkChildren() {
		//log.infof("%s.checkChildren: %s, count = %d",CLSS,getName(),(children==null?0:this.children.size()));
		super.checkChildren();
	}
	@Override
	public boolean confirmDelete(List<? extends AbstractNavTreeNode> selections) {
		// We only care about the first
		boolean result = false;
		if( selections.size()>0 ) {
			AbstractNavTreeNode selected = selections.get(0);
			result = ErrorUtil.showConfirm(String.format(BundleUtil.get().getString(PREFIX+".Delete.Confirmation.Question"), selected.getName()), BundleUtil.get().getString(PREFIX+".Delete.Confirmation.Title"));
		}
		return result;
	}

	public DesignerContext getContext() { return this.context; }
	
	@Override
	public Icon getExpandedIcon() { 
		Icon ike  = openIcon;
		if(statusManager.getAlertState(resourceId)) {
			ike = IconUtil.applyBadge(ike, alertBadge);
		}
		return ike;
	}
	
	@Override
	public Icon getIcon() {
		Icon ike  = closedIcon;
		if(statusManager.getAlertState(resourceId)) {
			ike = IconUtil.applyBadge(ike, alertBadge);
		}
		return ike;
	}
	public int getResourceScope() { return this.resourceScope; }
	
	@Override
	public String getWorkspaceName() {
		return DiagramWorkspace.key;
	}

	// Use the state for Applications and Families to remember whether to 
	// configure production or isolation databases
	public DiagramState getState() { return this.state; }
	public void setState(DiagramState ds) { 
		this.state = ds;
		statusManager.setPendingState(resourceId, ds);
	}
	@Override
	public boolean isEditActionHandler() {return true;}
	
	@Override
	public boolean isEditable() {return true;}
	private boolean isRoot() {
		return getResourcePath().getFolderPath().isEmpty();
	}
	
	/**
	 * Query the block controller in the Gateway. The resources that it knows
	 * about may, or may not, coincide with those in the Designer. 
	 */
	public void listControllerResources() {
		try {
			List <SerializableResourceDescriptor> descriptors = requestHandler.listResourceNodes();
			for( SerializableResourceDescriptor descriptor : descriptors ) {
				log.info("Res: "+descriptor.getName()+" "+descriptor.getResourceId().getProjectName()+":"+descriptor.getResourceId().getResourcePath().getPath().toString()+" "+
						descriptor.getType().toString());
			}
		} 
		catch (Exception ex) {
			log.warnf("%s. startAction: ERROR: %s",CLSS,ex.getMessage(),ex);
			ErrorUtil.showError(CLSS+" Exception listing controller resources",ex);
		}
	}
	/**
	 * Search the project for all resources. This is for debugging.
	 * We filter out those that are global (have no module) as these
	 * are system things that we don't care about for the moment.
	 */
	public void listProjectBLTResources() {
		List <ProjectResource> resources = context.getProject().getResources();
		for( ProjectResource res : resources ) {
			if( res.getResourcePath().getModuleId()==null || res.getResourcePath().getModuleId().length()==0) continue;
			if( !res.getResourceType().getTypeId().toString().startsWith("blt")) continue;     // List only diagram resources
			log.info("Res: "+res.getResourceName()+" "+res.getResourceId().getProjectName()+":"+res.getResourceId().getResourcePath().getPath().toString()+" "+
					res.getResourceType().getTypeId().toString());
		}
	}
	public String nextFreeName(AbstractResourceNavTreeNode node,String root) {
		int childCount = node.getChildCount();
		if( childCount==0 ) return root;
		
		String newName = root;
		boolean foundMatch = true;
		int index = 0;
		while(foundMatch) {
			foundMatch = false;
			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				Optional<ProjectResource> option = child.getProjectResource();
				ProjectResource cres = option.get();
				if( cres.getResourceName().equals(newName)) {
					foundMatch=true;
					break;
				}
			}
			if (foundMatch) {
				index = index+1;
				newName = String.format("%s-%d", root,index);
			}
		}
		return newName;
	}
	/**
	 *  Note: We ignore locking. Previous attempts to use the superior version of this method
	 *        fail to acquire locks (without first doing a project save from the main menu).
	 */
	@Override
	public void onEdit(String newTextValue) {
		// Sanitize name
		if (!isValid(newTextValue) ) {
			ErrorUtil.showError(BundleUtil.get().getString(PREFIX+".InvalidName", newTextValue));
			return;
		}
		String oldName = getResourceId().getResourcePath().getName();
		try {
			log.infof("%s.onEdit: alterName from %s to %s",CLSS,oldName,newTextValue);
			alterName(newTextValue);
			statusManager.setPendingName(resourceId,newTextValue);
		}
		catch (IllegalArgumentException ex) {
			ErrorUtil.showError(CLSS+".onEdit: "+ex.getMessage());
		}
	}
	
	// Update the status of the NavTree node
	@Override
	public synchronized void onSelected() {
		UndoManager.getInstance().setSelectedContext(NavTreeFolder.class);
		statusManager.reportDirtyState(getResourceId());
	}
	
	// Rename the NavTree node. NOTE: This does not yet rename the
	// underlying resource. Calls setText() to set the display.
	@Override
	public void setName(String name) {
		super.setName(name);
	}
	/**
	 * Exclude cut and paste which are currently not supported.
	 */
	@Override
	protected void addEditActions(JPopupMenu menu) {
		menu.add(renameAction);
		menu.add(new DeleteNodeAction(this));
	}
	
	@Override 
	public List<AbstractNavTreeNode> loadChildren() {
		//log.infof("%s.loadChildren: %s (%s) ..................",CLSS,getName(),getResourcePath().getFolderPath());
		List<AbstractNavTreeNode> kids = new ArrayList<>();
		List<ProjectResource> resources = context.getProject().getResources();
		// Search for children of this node
		for(ProjectResource pr:resources) {
			if( pr.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
				//log.infof("%s.loadChildren: resource %s vs %s",CLSS,pr.getResourcePath().getParentPath(),parentPath);
				if(isChildNode(pr)) {
					kids.add(createChildNode(pr));
				}
			}
		}
		return kids;
	}
	/**
	 * Create a child node because we've discovered a resource that matches this instance as a parent
	 * based on its content matching the our UUID. If the node had been previously created, then 
	 * reuse it.
	 */
	@Override
	protected AbstractNavTreeNode createChildNode(ProjectResource res) {
		log.infof("%s.createChildNode: %s (%s) type:%s, depth=%d", CLSS,res.getResourceName(),res.getResourcePath().getPath().toString(),
				(res.isFolder()?"folder":"diagram"),getDepth());
		// If the project is disabled, then don't do anything
		if( !context.getProject().isEnabled()) return null;
		AbstractResourceNavTreeNode node = statusManager.findNode(res.getResourceId());
		if( node==null ) {
			if ( res.isFolder() )       {
				node = new NavTreeFolder(context, res);
			}
			else  {
				node = new DiagramTreeNode(context,res,workspace);
			} 
		}
		else {
			if( node instanceof DiagramTreeNode ) context.getProject().addProjectResourceListener((DiagramTreeNode)node);
		}
		node.install(this);
		if( node.getParent()==null) {
			log.errorf("%s.createChildNode: ERROR parent is null %s(%d)",CLSS,node.getName(),res.getResourceId());
		}
		return node;
	}


	/**
	 * Define the menu used for popups. This appears to be called each time a menu is called for ..
	 */
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		if( this.getParent()==null ) {
			log.errorf("%s.initPopupMenu: ERROR: Diagram (%d) has no parent",CLSS,hashCode());
		}
		context.getProject().addProjectResourceListener(this);
		if (isRoot()) { 
			if( context.getProject().isEnabled()) {
				DebugAction debugAction = new DebugAction();
				if( requestHandler.isControllerRunning() ) {
					startAction.setEnabled(false);
				} 
				else {
					stopAction.setEnabled(false);
				}
				menu.add(startAction);
				menu.add(stopAction);
				menu.add(debugAction);
				menu.addSeparator();
			}
		}
		if( getProjectResource()!=null ) {
			
			SetFolderStateAction ssaActive = new SetFolderStateAction(this,DiagramState.ACTIVE);
			SetFolderStateAction ssaDisable = new SetFolderStateAction(this,DiagramState.DISABLED);
			SetFolderStateAction ssaIsolated = new SetFolderStateAction(this,DiagramState.ISOLATED);
			JMenu setStateMenu = new JMenu(BundleUtil.get().getString(PREFIX+".SetApplicationState"));
			setStateMenu.add(ssaActive);
			setStateMenu.add(ssaDisable);
			setStateMenu.add(ssaIsolated);
			menu.add(setStateMenu);

			DiagramCreateAction diagramAction = new DiagramCreateAction(this);
			menu.add(diagramAction);
			ImportDiagramAction importAction = new ImportDiagramAction(menu.getRootPane(),this);
			menu.add(importAction);
			
			menu.add(folderCreateAction);
			menu.addSeparator();

			if( true /*something is selected*/ ) {
				copyBranchAction.setEnabled(true);
			} 
			else {
				copyBranchAction.setEnabled(false);
			}
			if( true /*something is in the clipboard*/ ) {
				pasteBranchAction.setEnabled(true);
			} 
			else {
				pasteBranchAction.setEnabled(false);
			}
			menu.add(copyBranchAction);
			menu.add(pasteBranchAction);	
		}
		else { 
			log.warnf("%s.initPopupMenu: ERROR: node %s(%s) has no project resource",CLSS,this.getName(),resourceId.getFolderPath());
		}
		
		if( !isRoot() ) {
			menu.addSeparator();
			addEditActions(menu);
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


	// This nodes of the tree is associated with a diagram. It's only other possible children
	// are other diagrams which are children of encapsulation blocks. At present these are not handled
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to a diagram.
	private SerializableDiagram recursivelyDeserializeDiagram(AbstractResourceNavTreeNode node) {
		Optional<ProjectResource>optional = node.getProjectResource();
		ProjectResource res = optional.get();
		SerializableDiagram sdiag = null;
		if( res!=null ) {
			log.infof("%s.recursivelyDeserializeDiagram: %s (%d)",CLSS,res.getProjectName(),res.getResourceId());
			sdiag = SerializableDiagram.deserializeDiagram(res);
			sdiag.setState(statusManager.getPendingState(res.getResourceId()));
		}
		return sdiag;
	}

	/**
	 * Do it.  (Note this will diagnosis names to avoid collisions).
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
	

	// Recursively descend the node tree, gathering up associated resources.
	// Deserialize them and add as proper children of the parent
	// @param node a tree node corresponding to an application.
	private SerializableFolder recursivelyDeserializeFolder(AbstractResourceNavTreeNode node) {
		Optional<ProjectResource>optional = node.getProjectResource();
		ProjectResource res = optional.get();
		SerializableFolder sfold = null;
		if( res!=null ) {
			log.infof("%s.recursivelyDeserializeFolder: %s (%s)",CLSS,res.getResourceName(),res.getResourceId().getResourcePath().getPath().toString());
			sfold = new SerializableFolder();
			sfold.setName(res.getResourceName());
			sfold.setPath(res.getFolderPath()+"/"+res.getResourceName());
			sfold.setFolders(new SerializableFolder[0]);
			sfold.setDiagrams(new SerializableDiagram[0]);

			@SuppressWarnings("rawtypes")
			Enumeration walker = node.children();
			while(walker.hasMoreElements()) {
				AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
				optional = child.getProjectResource();
				ProjectResource pr = optional.get();
				if( !pr.isFolder() ) {
					SerializableDiagram sd = recursivelyDeserializeDiagram(child);
					if( sd!=null ) sfold.addDiagram(sd);
				}
				else {
					SerializableFolder sf = recursivelyDeserializeFolder(child);
					if( sf!=null ) sfold.addFolder(sf);
				}
			}
		}
		return sfold;
	}


	// ============================================= private action classes ===========================================================

	// Note: At the time of export the folder is assumed to be completely up-to-date, including with external databases.
	private class FolderExportAction extends BaseAction {
		private static final long serialVersionUID = -3813337995419856624L;
		private final static String POPUP_TITLE = "Export Folder Tree";
		private final NavTreeFolder node;
		private final Component anchor;
		public FolderExportAction(Component c,NavTreeFolder gptn)  {
			super(PREFIX+".FolderExport",IconUtil.getIcon("export1")); 
			node=gptn;
			anchor=c;
		}

		public void actionPerformed(ActionEvent e) {

			if( node==null ) return;   // Do nothing
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						ExportDialog dialog = new ExportDialog(context.getFrame());
						dialog.setLocationRelativeTo(anchor);
						Point p = dialog.getLocation();
    					dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
						dialog.pack();
						dialog.setVisible(true);   // Returns when dialog is closed
						File output = dialog.getFilePath();
						boolean success = false;
						if( output!=null ) {
							log.debugf("%s.actionPerformed: dialog returned %s",CLSS,output.getAbsolutePath());
							try {
								if(output.exists()) {
									//output.delete();           // Remove existing file
									//output.createNewFile();
									output.setWritable(true);  // This doesn't seem to work (??)
								}
								else {
									output.createNewFile();
								}

								if( output.canWrite() ) {
									ObjectMapper mapper = new ObjectMapper();
									try{ 
										// Convert the view into a serializable object.
										
										SerializableFolder sap = recursivelyDeserializeFolder(node);
										String json = mapper.writeValueAsString(sap);
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
				ErrorUtil.showError(CLSS+" Exception exporting folder",err);
			}
		}
	}

	// Called during paste
	protected boolean copyChildren(AbstractResourceNavTreeNode fromNode) throws Exception {
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);

		if (fromNode == null) {
			ErrorUtil.showWarning("copy children with NULL node!");
			return false;
		}
		// Iterate over the children of the original node
		@SuppressWarnings("rawtypes")
		Enumeration walker = fromNode.children();
		while(walker.hasMoreElements()) {
			AbstractResourceNavTreeNode child = (AbstractResourceNavTreeNode)walker.nextElement();
			Optional<ProjectResource> optional = child.getProjectResource();
			ProjectResource res = optional.get();
			byte[] bytes = res.getData();
			String json = "";
			ProjectResourceId resid = null;

			if( !res.isFolder()) {

				SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
				if( sd!=null ) {
					renameHandler.convertPaths(sd,res.getFolderPath());
					ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
					for( Block blk:diagram.getBlocks()) {
						ProcessBlockView pbv = (ProcessBlockView)blk;
						if (pbv.isDiagnosis()) {
							renameDiagnosis(sd, pbv);

							continue;
						}
					}
					sd.setState(DiagramState.DISABLED);
					json = mapper.writeValueAsString(sd);
					resid = requestHandler.createResourceId(getResourceId().getProjectName(), sd.getPath().toString(), BLTProperties.DIAGRAM_RESOURCE_TYPE);
				}
				else {
					ErrorUtil.showWarning(String.format("Failed to deserialize child diagram (%s)",res.getResourceName()),"Copy Diagram");
					return false;
				}
			}
			else  {
				SerializableFolder sf = mapper.readValue(new String(bytes), SerializableFolder.class);
				if( sf!=null ) {
					renameHandler.convertPaths(sf,res.getFolderPath());
					json = mapper.writeValueAsString(sf);
					resid = requestHandler.createResourceId(getResourceId().getProjectName(), sf.getPath().toString(), BLTProperties.DIAGRAM_RESOURCE_TYPE);
				}
				else {
					ErrorUtil.showWarning(String.format("Failed to deserialize child folder (%s)",res.getResourceName()),"Copy Folder");
					return false;
				}
			}

			new ResourceCreateManager(getResourcePath().getFolderPath(),resid.getResourcePath().getName(),json.getBytes()).run();
			if (!copyChildren(child)) {
				return false;
			}
		}
		return true;
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

           Optional<ProjectResource> optional = parentNode.getProjectResource();
           ProjectResource res = optional.get();
           String data = ""+res.getResourceId();
           Transferable t =  new StringSelection(BLT_COPY_OPERATION + data);
				   
		   if (t != null) {
			   try { 
				   clipboard.setContents(t, null); 
			   } catch (Exception ex) {
					log.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
			   }
		   }
				   
		}
	}

	// paste the nodes from the clipboard into the tree at the selected location
	private class PasteAction extends BaseAction  implements UndoManager.UndoAction{
		private static final long serialVersionUID = 1L;
		private final NavTreeFolder parentNode;  // Bad naming, it isn't really a parent node, it's this one
		private String bundleString;
		private ProjectResource pasted = null;

		public PasteAction(NavTreeFolder pNode)  {
			super(PREFIX+".PasteNode",IconUtil.getIcon("paste"));
			this.bundleString = PREFIX+".PasteNode";
			this.parentNode = pNode;
		}

		@Override
		public boolean isGroupSequenceIndependent() {return false;}

		@Override
		public boolean undo() {
			try {
				context.getProject().deleteResource(pasted.getResourceId());
			}
			catch(ResourceNotFoundException rnfe) {
				log.warnf("%s: undo: Resource not found (%s)",CLSS,rnfe.getMessage());
			}
			return true;
		}

		@Override
		public String getDescription() { return " paste"; }  // This could be made more descriptive

		@Override
		public boolean execute() {
			boolean result = true;
			final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

			Transferable t = clipboard.getContents(null);
			if (t.isDataFlavorSupported(DataFlavor.stringFlavor)) {
				try { 
					String clipData = (String)t.getTransferData(DataFlavor.stringFlavor);
					if (clipData.startsWith(BLT_CUT_OPERATION)) {
						String data = clipData.substring(BLT_CUT_OPERATION.length());
						paste(data, true);
					}
					if (clipData.startsWith(BLT_COPY_OPERATION)) {
						String data = clipData.substring(BLT_COPY_OPERATION.length());
						paste(data, false);
					}
				} catch (Exception ex) {
					result = false;
					log.errorf("%s: actionPerformed: Unhandled Exception in PASTE (%s)",CLSS,ex.getMessage());
					ex.printStackTrace();
				}
			}
			return result;
		}



		public void actionPerformed(ActionEvent e) {
			execute();
		}	

		// The paste string represents a resourceID of a new resource/nav tree node
		public void paste(String data, boolean deleteOriginal) {
			ObjectMapper mapper = new ObjectMapper();
			ProjectResourceId clipId = null;
			try {
				clipId = (ProjectResourceId)mapper.readValue(data, ProjectResourceId.class);

			}
			catch(JsonParseException jpe) {
				log.warnf("%s: paste: Parse exception of paste object (%s)",CLSS,jpe.getMessage());
				return;
			}
			catch(JsonMappingException jme) {
				log.warnf("%s: paste: Json mapping exception of paste object (%s)",CLSS,jme.getMessage());
				return;
			}
			catch(IOException ioe) {
				log.warnf("%s: paste: IO exception of paste object (%s)",CLSS,ioe.getMessage());
				return;
			}
			//			*Note cut disabled for now.  Just copy & delete	
			Optional<ProjectResource> optional = context.getProject().getResource(clipId);
			ProjectResource res = optional.get();
			optional = parentNode.getProjectResource();
			ProjectResource dst = optional.get();
			String newName = nextFreeName((NavTreeFolder)parentNode,res.getResourceName());
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						try {	
							ObjectMapper mapper = new ObjectMapper();
							mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
							SerializableDiagram sd = null;

							String json = "";
							if( res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
								AbstractResourceNavTreeNode node = parentNode;
								if( (node.getProjectResource() != null && 
										parentNode.getResourcePath().getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)  )) {
									sd = mapper.readValue(new String(res.getData()), SerializableDiagram.class);
									if( sd!=null ) {
										ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
										renameHandler.convertPaths(sd,res.getFolderPath());  // converted UUIDs are thrown away because later CopyChildren also does it
										sd.setName(newName);
										for( Block blk:diagram.getBlocks()) {
											ProcessBlockView pbv = (ProcessBlockView)blk;
											if (pbv.isDiagnosis()) {
												renameDiagnosis(sd, pbv);
												continue;
											}
										}										
										sd.setState(DiagramState.DISABLED);
										json = mapper.writeValueAsString(sd);
										statusManager.setPendingState(node.getResourceId(), sd.getState());

									}
									else {
										ErrorUtil.showWarning(String.format("Failed to deserialize diagram (%s)",res.getResourceName()),"Paste Diagram");
										return;
									}
								} 
								else {
									ErrorUtil.showWarning("Tried to paste a diagram into an invalid location","Paste Diagram");
									return;
								}

							} 
							else {
								ErrorUtil.showWarning(String.format("Unexpected resource type(%s)",res.getResourceType()),"Paste Node");
								return;
							}
							// res is the project resource to copy - we just have to change the name
							ProjectResourceId oldId = res.getResourceId();
							ProjectResourceId newId = requestHandler.createResourceId(oldId.getProjectName(),
									oldId.getFolderPath()+"/"+name, oldId.getResourceType());

							ProjectResourceBuilder builder = ProjectResource.newBuilder();
							builder.setResourceId(newId);;
							builder.putData(res.getData());
							builder.setVersion(res.getVersion());
							builder.setApplicationScope(res.getApplicationScope());


							AbstractResourceNavTreeNode node = statusManager.findNode(res.getResourceId());  // so basically starting over here
							ProjectResource resource = builder.build();
							//							Copies children and assigns new ResourcePaths
							if (copyChildren(node) && deleteOriginal) { // copy children will rename diagnosis blocks
								ResourceDeleteManager deleter;
								deleter = new ResourceDeleteManager(node);
								deleter.acquireResourcesToDelete();
								deleter.run();
							}

							// Finally display the parent node
							//new ResourceCreateManager(resource,resource.getResourceName()).run();
							AbstractResourceNavTreeNode newNode = statusManager.findNode(resource.getResourceId());  // so basically starting over here

							UndoManager.getInstance().add(PasteAction.this,AbstractNavTreeNode.class);
							pasted = resource;
							((NavTreeFolder)parentNode).selectChild(new ResourcePath[] {resource.getResourcePath().getParent()} );

						}
						catch( IOException ioe) {
							// Should never happen, we just picked this off a chooser
							log.warnf("%s: actionPerformed, IOException(%s)",CLSS,ioe.getLocalizedMessage()); 
							ioe.printStackTrace();
						}
						catch (Exception ex) {
							log.errorf("%s: actionPerformed: Unhandled Exception (%s)",CLSS,ex.getMessage());
							ex.printStackTrace();
						}
					}
				});
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception cloning diagram",err);
			}	
		}
	}
		
	// From the root node, recursively log the contents of the tree
	private class DebugAction extends BaseAction {


		private static final long serialVersionUID = 1L;
		public DebugAction()  {
			super(PREFIX+".Debug",IconUtil.getIcon("bug_yellow"));
		}

		public void actionPerformed(ActionEvent e) {
			log.info("============================ BLT Resources (Designer) =========================");
			listProjectBLTResources();
			log.info("============================ BLT Resources (Gateway)  =========================");
			listControllerResources();
			log.infof("================================ (proj = %s )==============================",context.getProject().getName());
		}
	}

	// Delete the this node and all its descendants. 
	// Note: On a "save" action, the descendants are removed also.
	private class DeleteNodeAction extends BaseAction implements UndoManager.UndoAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode node; 
		ResourceDeleteManager deleter;
		private String bundleString;
		private ProjectResourceId resid = null;    // for the root

		public DeleteNodeAction(NavTreeFolder resourceNode)  {
			super(PREFIX+".DeleteNode",IconUtil.getIcon("delete"));
			this.node = resourceNode;
			this.bundleString = PREFIX+".NodeNoun";
			this.deleter = new ResourceDeleteManager(node);
		}

		public void actionPerformed(ActionEvent e) {
			AbstractNavTreeNode p = node.getParent();
			Optional<ProjectResource> optional = node.getProjectResource();
			ProjectResource res = optional.get();
			resid = res.getResourceId();
			log.infof("%s.DeleteNodeAction: %s, resource %s.",CLSS,node.getName(),resid.getFolderPath());
			List<AbstractResourceNavTreeNode>selected = new ArrayList<>();
			selected.add(node);
			if(confirmDelete(selected)) {
				if( res.getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
					bundleString = PREFIX+".FolderNoun";
				}
				deleter.acquireResourcesToDelete();
				if( execute() ) {
					UndoManager.getInstance().add(this,NavTreeFolder.class);
					
					if( p instanceof NavTreeFolder )  {
						NavTreeFolder parentNode = (NavTreeFolder)p;
						parentNode.recreate();
						parentNode.expand();
					}
					deleter.run();
				}
				else {
					ErrorUtil.showError("Node locked, delete failed");
				}
			}
		}

		// Marks the project resources for deletion.
		@Override
		public boolean execute() {
			deleter.run();
			return true;
		}

		@Override
		public boolean isGroupSequenceIndependent() {return false;}

		@Override
		public boolean undo() {
			deleter.run();
			return true;
		}

		@Override
		public String getDescription() { return new String(BundleUtil.get().getStringLenient(bundleString) + " delete"); }

	}

	// Create a new diagram
	private class DiagramCreateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode currentNode;
		public DiagramCreateAction(AbstractResourceNavTreeNode parentNode)  {
			super(PREFIX+".NewDiagram",diagramIcon);  // preferences
			this.currentNode = parentNode;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				String newName = BundleUtil.get().getString(PREFIX+".NewDiagram.Default.Name");
				if( newName==null) newName = "New Diagram";  // Missing string resource
				ProjectResourceId resid = requestHandler.createResourceId(currentNode.getResourceId().getProjectName(), currentNode.getResourcePath().getParentPath()+"/"+newName, BLTProperties.DIAGRAM_RESOURCE_TYPE);
				SerializableDiagram sd = new SerializableDiagram();
				sd.setName(newName);
				sd.setPath(currentNode.getResourcePath().getFolderPath()+"/"+newName);
				sd.setState(DiagramState.DISABLED);
				
				byte[] bytes = sd.serialize();
				log.infof("%s.DiagramCreateAction. create new %s(%s), %s (%d bytes)",CLSS,BLTProperties.DIAGRAM_RESOURCE_TYPE.getTypeId(),
						newName,currentNode.getResourcePath().getParentPath()+"/"+newName,bytes.length);

				CreateResourceDialogBuilder builder = CreateResourceDialog.newBuilder();
				builder.setContext(context);
				builder.setNoun("diagram");
				builder.setDefaultName(newName);
				builder.setFolder(currentNode.getResourcePath());
				builder.setResourceBuilder(b->b.setFolder(false).setApplicationScope(ApplicationScope.DESIGNER).putData(bytes));
				builder.setTitle("New Dialog");
				builder.buildAndDisplay();
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception creating diagram",err);
			}
		}
	}

	
	// Create a folder child of the current node
	private class FolderCreateAction extends NewFolderAction {
		private static final long serialVersionUID = 1L;
		public FolderCreateAction(NavTreeFolder parentNode)  {
			super(parentNode.getContext(),BLTProperties.DIAGRAM_RESOURCE_TYPE,parentNode.getResourceScope(),(ProjectResource)getProjectResource().orElse(null),parentNode);
		}
	}

	private class ImportDiagramAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final AbstractResourceNavTreeNode parentNode;
		private final Component anchor;
		private final static String POPUP_TITLE = "Import Diagram";
		public ImportDiagramAction(Component c,AbstractResourceNavTreeNode pNode)  {
			super(PREFIX+".ImportDiagram",IconUtil.getIcon("import1"));  // preferences
			this.parentNode = pNode;
			this.anchor = c;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				EventQueue.invokeLater(new Runnable() {
					public void run() {
						try {
							String title = BundleUtil.get().getString(PREFIX+".Import.Application.DialogTitle");
							String label = BundleUtil.get().getString(PREFIX+".Import.Application.NameLabel");
							ImportDialog dialog = new ImportDialog(context.getFrame(),label,title);
							dialog.setLocationRelativeTo(anchor);
							Point p = dialog.getLocation();
	    					dialog.setLocation((int)(p.getX()-OFFSET),(int)(p.getY()-OFFSET));
							dialog.pack();
							dialog.setVisible(true);   // Returns when dialog is closed
							File input = dialog.getFilePath();

							if( input!=null ) {
								if( input.exists() && input.canRead()) {
									try {
										// Note: Requires Java 1.7
										byte[] bytes = Files.readAllBytes(input.toPath());
										// It would be nice to simply convert to a resource.
										// Unfortunately we have to replace all UUIDs with new ones
										ObjectMapper mapper = new ObjectMapper();
										mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
										SerializableDiagram sd = mapper.readValue(new String(bytes), SerializableDiagram.class);
										if( sd!=null ) {
											log.infof("%s:ImportDiagramAction imported diagram:\n%s", CLSS,sd.getName());
											renameHandler.convertPaths(sd,getResourcePath().getFolderPath());
											sd.setState(DiagramState.DISABLED);
											String json = mapper.writeValueAsString(sd);

											log.debugf("%s:ImportDiagramAction saved resource as:\n%s", CLSS,json);
											ProjectResourceId resid = requestHandler.createResourceId(getResourceId().getProjectName(), getResourceId().getFolderPath(), BLTProperties.DIAGRAM_RESOURCE_TYPE);
											new ResourceCreateManager(getResourcePath().getFolderPath(),sd.getName(),sd.serialize()).run();	
											parentNode.selectChild(new ResourcePath[] {getResourcePath()} );
											statusManager.addToUnsavedList(getResourcePath());
											statusManager.setPendingState(resid, sd.getState());
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
				ErrorUtil.showError(CLSS+" Exception importing diagram",err);
			}
		}
	}
	
	/**
	 * Recursively set the state of every diagram under the application to the selected value.
	 * If the selected value is ISOLATED, then we also update the external database from
	 * the project resources. If there is a state change, we save the diagram to keep
	 * project resource/gateway/designer all in sync.
	 */
	private class SetFolderStateAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final NavTreeFolder app;
		private final DiagramState treeState;
		public SetFolderStateAction(NavTreeFolder applicationNode,DiagramState s)  {
			super(PREFIX+".SetFolderState."+s.name());
			this.app = applicationNode;
			this.treeState = s;
		}

		// We need to set the state both locally and in the gateway
		public void actionPerformed(ActionEvent e) {
			
			// Set the nodes in the navtree.
			recursivelyUpdateNodeState(app,treeState);
		}
		// Set the state locally to match and save changes to disk. It's redundant to inform the Gateway.
		public void recursivelyUpdateNodeState(AbstractNavTreeNode node,DiagramState diagramState) {
			if( node==null) return;
			if( node instanceof DiagramTreeNode ) {
				DiagramTreeNode dtn = (DiagramTreeNode)node;
				dtn.setDiagramState(diagramState);
			}
			@SuppressWarnings("unchecked")
			Enumeration<AbstractNavTreeNode>  childWalker = node.children();
			while(childWalker.hasMoreElements()) {
				recursivelyUpdateNodeState(childWalker.nextElement(),diagramState);
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
			try{
				requestHandler.startController();
				this.setEnabled(false);
				stopAction.setEnabled(true);
			} 
			catch (Exception ex) {
				log.warnf("%s: startAction: ERROR: %s",CLSS,ex.getMessage(),ex);
				ErrorUtil.showError(CLSS+" Exception starting the controller",ex);
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
				requestHandler.stopController();
				this.setEnabled(false);
				startAction.setEnabled(true);
			}
			catch(Exception ex) {
				log.warnf("%s: stopAction: ERROR: %s",CLSS,ex.getMessage(),ex);
				ErrorUtil.showError(CLSS+" Exception stopping the controller",ex);
			}
		}
	}
	

	@SuppressWarnings("unchecked")
	private void parseNodeForConflicts(List<String> nameList, AbstractResourceNavTreeNode theNode, StringBuffer buf) {
		if (theNode.getProjectResource().get().getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
			Optional<ProjectResource> optional = theNode.getProjectResource();
			ProjectResource res = optional.get();
			String json = new String(res.getData());
			SerializableDiagram sd = null;
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			try {
				sd = mapper.readValue(json,SerializableDiagram.class);
				// Synchronize names as the resource may have been re-named and/or
				// state changed since it was serialized
				sd.setName(res.getResourceName());
				sd.setState(statusManager.getPendingState(resourceId));
				
				// For a small number of blocks on Pete's system, we've removed the name attribute and re-instated.
				// The correct name may be in the property
				ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
				for( Block blk:diagram.getBlocks()) {
					ProcessBlockView pbv = (ProcessBlockView)blk;
					// Re-instate name from property, if necessary
					if( BlockConstants.DEFAULT_BLOCK_NAME.equals(pbv.getName())&& pbv.getProperty(BlockConstants.BLOCK_PROPERTY_NAME)!=null) {
						pbv.setName(pbv.getProperty(BlockConstants.BLOCK_PROPERTY_NAME).getValue().toString());
					}
					// For now only check final diagnosis blocks
					if (pbv.isDiagnosis()) {
						String key = (pbv.getClassName() + pbv.getName()).toLowerCase();
						if (nameList.contains(key)) {
							buf.append( "Duplicate " + pbv.getClassName() + " block named " + pbv.getName() + " found in diagram " + diagram.getName() + "\n");
						} 
						else {
							nameList.add(key);
						}
					}
				}
			} 
			catch (JsonParseException jpe) {
				buf.append(jpe.getLocalizedMessage());
				buf.append("\n");
				log.warnf("%s: open parse exception (%s)",CLSS,jpe.getLocalizedMessage());
			} 
			catch (JsonMappingException jme) {
				buf.append(jme.getLocalizedMessage());
				buf.append("\n");
				log.warnf("%s: open mapping exception (%s)",CLSS,jme.getLocalizedMessage());
			} 
			catch (IOException ioe) {
				buf.append(ioe.getLocalizedMessage());
				buf.append("\n");
				log.warnf("%s: open io exception (%s)",CLSS,ioe.getLocalizedMessage());
			}
			
		}
		// Not-a-diagram
		else {
			Enumeration<AbstractResourceNavTreeNode> nodeWalker = theNode.children();
			while(nodeWalker.hasMoreElements()) {
				AbstractResourceNavTreeNode aNode = nodeWalker.nextElement();
				parseNodeForConflicts(nameList, aNode, buf);
			}
		}
	}


	/**
	 * @param res
	 * @return true if this node is a direct child of the current node
	 */
	private boolean isChildNode(ProjectResource res) {
		boolean result = false;
		ResourcePath parentPath = getResourcePath();
		ResourcePath rp = res.getResourcePath();
		if( parentPath.isParentOf(rp)) {
			result = true;
		}
		return result;
	}
    // ************************ ProjectResourceListener *****************************
	/**
	 * We get here due to:
	 *    1) Name changes to resource
	 *    2) New resource from menu selection
	 * In either case, we mark these "unsaved"
	 */
	@Override
	public void resourcesCreated(String projectName,List<ChangeOperation.CreateResourceOperation> ops) {
		for(ChangeOperation.CreateResourceOperation op:ops ) {
			ProjectResourceId id = op.getResourceId();
			log.infof("%s.resourcesCreated: %s",CLSS,id.getFolderPath());
			statusManager.addToUnsavedList(id.getResourcePath());
			statusManager.reportDirtyState(id);
		}
	}
	/**
	 * The updates that we are interested in are:
	 *    1) Name changes to this resource
	 * We can ignore deletions because we delete the model resource
	 * by deleting the panel resource.
	 */
	@Override
	public void resourcesDeleted(String projectName,List<ChangeOperation.DeleteResourceOperation> ops) {
		log.debug(CLSS+".resourcesDeleted (ignore)");
	}
	@Override
	public void resourcesModified(String projectName, List<ChangeOperation.ModifyResourceOperation> changes) {
		// Take care of any special status before invoking the super-class method.
		for(ChangeOperation.ModifyResourceOperation op:changes ) {
			if( op.getResourceId().equals(resourceId) ) {
				log.infof("%s.resourcesModified: %s",CLSS,op,getName(),resourceId.getFolderPath());
				ProjectResource res = op.getResource();
				super.onResourceModified(res);
			}
		}
	}
	
	// ************************ NavTreeNodeInterface **************************
	// These methods are called by the NodeStatusManager
	@Override
	public void prepareForDeletion() { uninstall(); }
	
	/**
	 * Update the node italic/plain in background.
	 * Note: This method should ONLY be called from the node status manager.
	 */
	@Override
	public void updateUI(boolean dty) {
		log.infof("%s.updateUI: %s dirty = %s",CLSS,resourceId.getResourcePath().getPath().toString(),(dty?"true":"false"));
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				setItalic(dty);
				refresh();
			}
		});
	}
}
