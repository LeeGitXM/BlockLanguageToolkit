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
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;
import java.util.Objects;
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
import com.ils.blt.designer.workspace.CopyPasteHandler;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.util.action.BaseAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.model.ApplicationScope;
import com.inductiveautomation.ignition.common.project.ChangeOperation;
import com.inductiveautomation.ignition.common.project.ProjectResourceListener;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
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
import com.inductiveautomation.ignition.designer.navtree.model.ResourceDeleteAction;

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
public class NavTreeFolder extends FolderNode implements ProjectResourceListener {
	private static final String CLSS = "NavTreeFolder";
	public static final String BLT_CUT_OPERATION = "BLTCUT";
	public static final String BLT_COPY_OPERATION = "BLTCOPY";
	private static final int OFFSET = 100;
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private final static LoggerEx log = LogUtil.getLogger(NavTreeFolder.class.getPackageName());
	protected final DesignerContext context;
	private DiagramState state = DiagramState.ACTIVE;  // Used for Applications and Families
	private final PasteAction pasteAction;
	private final StartAction startAction = new StartAction();
	private final StopAction stopAction = new StopAction();
	private final DiagramWorkspace workspace;
	private final SerializableNodeRenameHandler renameHandler;
	private final NodeStatusManager statusManager;
	private final CreateFolderAction folderCreateAction;
	private final ApplicationRequestHandler requestHandler;
	private final ImageIcon brainIcon;
	private final ImageIcon defaultIcon = IconUtil.getIcon("folder_closed");
	private final ImageIcon openIcon;
	private final ImageIcon closedIcon;
	private final ImageIcon diagramIcon;
	private final CopyPasteHandler cpHandler;

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
		this.cpHandler = new CopyPasteHandler(ctx,this);
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
		pasteAction = new PasteAction(this);
		folderCreateAction = new CreateFolderAction(this);
		
		workspace = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getWorkspace();
		statusManager = NodeStatusManager.getInstance();
		brainIcon =iconFromPath("Block/icons/navtree/brain_cogs.png");

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
		if(isRoot()) ike = brainIcon;
		return ike;
	}
	
	@Override
	public Icon getIcon() {
		Icon ike  = closedIcon;
		if(isRoot()) ike = brainIcon;
		return ike;
	}
	public int getResourceScope() { return this.resourceScope; }
	
	@Override
	public String getWorkspaceName() {
		return DiagramWorkspace.key;
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
		updateUI();
	}
	
	/**
	 * Exclude cut and paste which are currently not supported.
	 */
	@Override
	protected void addEditActions(JPopupMenu menu) {
		menu.add(renameAction);
		menu.add(new DeleteFolderAction(this));
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
		Collections.sort(kids,new SortByName());
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
		AbstractResourceNavTreeNode node = statusManager.getNode(res.getResourceId());
		if( node==null ) {
			if ( res.isFolder() )       {
				node = new NavTreeFolder(context, res);
			}
			else  {
				node = new DiagramTreeNode(context,res,workspace);
			} 
			statusManager.createResourceStatus(node, res.getResourceId());
		}
		else {
			if( node instanceof DiagramTreeNode ) {
				context.getProject().addProjectResourceListener((DiagramTreeNode)node);
			}
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
			JMenu setStateMenu = new JMenu(BundleUtil.get().getString(PREFIX+".SetFolderState"));
			setStateMenu.add(ssaActive);
			setStateMenu.add(ssaDisable);
			setStateMenu.add(ssaIsolated);
			menu.add(setStateMenu);

			CreateDiagramAction diagramAction = new CreateDiagramAction(this);
			menu.add(diagramAction);
			ImportDiagramAction importAction = new ImportDiagramAction(context,this);
			menu.add(importAction);
			
			menu.add(folderCreateAction);
			menu.addSeparator();

			pasteAction.setEnabled(cpHandler.canPaste());

			menu.add(pasteAction);	
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

	
	// copy the currently selected node UUID to the clipboard
	private class CopyAction extends BaseAction {
		private static final long serialVersionUID = -1720155293900962586L;

		public CopyAction()  {
			super(PREFIX+".CopyNode",IconUtil.getIcon("copy"));
		}

		public void actionPerformed(ActionEvent e) {
			cpHandler.doCopyFolder();
		}
	}

	// paste the nodes from the clipboard into the tree as children of this node
	private class PasteAction extends BaseAction  {

		public PasteAction(NavTreeFolder pNode)  {
			super(PREFIX+".PasteNode",IconUtil.getIcon("paste"));
		}

		public void actionPerformed(ActionEvent e) {
			cpHandler.doPaste();
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

    private class DeleteFolderAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
    	private String noun;
    	private final AbstractResourceNavTreeNode node;
    	
	    public DeleteFolderAction(AbstractResourceNavTreeNode tnode)  {
	    	super(PREFIX+".DeleteNode",IconUtil.getIcon("delete")); 
	    	this.node = tnode;
	    	this.noun = PREFIX+".DiagramNoun";
	    }
	    public void actionPerformed(ActionEvent e) {
	    	List<AbstractResourceNavTreeNode> nodes = new ArrayList<>();
	    	nodes.add(node);
	    	List<DiagramTreeNode> diagrams = new ArrayList<>();
	    	accumulateDiagramsToBeDeleted(node,diagrams);
	    	for(DiagramTreeNode diagram:diagrams) {
	    		diagram.closeAndCommit();
	    	}
	    	ResourceDeleteAction deleter = new ResourceDeleteAction(context,nodes,noun);
	    	deleter.execute();
	    }
	}
    
    private void accumulateDiagramsToBeDeleted( AbstractResourceNavTreeNode root, List<DiagramTreeNode> diagrams) {
    	if( root instanceof DiagramTreeNode ) {
    		diagrams.add((DiagramTreeNode)root);
    	}
    	else {
    		Enumeration<AbstractResourceNavTreeNode> walker = root.children();
    		while(walker.hasMoreElements()) {
    			accumulateDiagramsToBeDeleted(walker.nextElement(),diagrams);
    		}
    	}
    }

	// Create a new diagram
	private class CreateDiagramAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final NavTreeFolder parentNode;
		private String newName = BundleUtil.get().getString(PREFIX+".NewDiagram.Default.Name");
		
		public CreateDiagramAction(NavTreeFolder currentNode)  {
			super(PREFIX+".NewDiagram",diagramIcon);  // preferences
			this.parentNode = currentNode;
		}

		public void actionPerformed(ActionEvent e) {
			Objects.requireNonNull(parentNode);
			try {
				SerializableDiagram sd = new SerializableDiagram();
				sd.setName(newName);
				sd.setPath(parentNode.getResourcePath().getFolderPath()+"/"+newName);
				sd.setState(DiagramState.DISABLED);
				
				byte[] bytes = sd.serialize();
				log.infof("%s.DiagramCreateAction. create new %s(%s), %s (%d bytes)",CLSS,BLTProperties.DIAGRAM_RESOURCE_TYPE.getTypeId(),
						newName,parentNode.getResourcePath().getParentPath()+"/"+newName,bytes.length);

				NewResourceDialog.NewResourceDialogBuilder builder = NewResourceDialog.newBuilder();
				builder.setContext(context);
				builder.setNoun(BundleUtil.get().getString(PREFIX+".DiagramNoun"));
				builder.setDefaultName(newName);
				builder.setParent(parentNode.getResourcePath());
				builder.setResourceBuilder(b->b.setFolder(false).setApplicationScope(ApplicationScope.DESIGNER).putData(bytes));
				builder.setTitle(BundleUtil.get().getString(PREFIX+".NewDiagram.Title"));
				builder.buildAndDisplay();
			} 
			catch (Exception err) {
				ErrorUtil.showError(CLSS+" Exception creating diagram",err);
			}
		}
	}
	
	// Create a folder child of the current node
	private class CreateFolderAction extends BaseAction {
		private static final long serialVersionUID = -1820363032385963659L;
		protected NavTreeFolder parentNode;
		
		public CreateFolderAction(NavTreeFolder currentNode)  {
			super(PREFIX+".NewFolder",defaultIcon);  // preferences
			this.parentNode = currentNode;
		}
		@Override
		public void actionPerformed(ActionEvent e) {
			log.infof("%s.FolderCreateAction. create new folder",CLSS);
			Objects.requireNonNull(parentNode);
			NewResourceDialog.NewResourceDialogBuilder builder = NewResourceDialog.newBuilder();
			builder.setContext(context);
			builder.setNoun(BundleUtil.get().getString(PREFIX+".FolderNoun"));
			builder.setDefaultName(BundleUtil.get().getString(PREFIX+".NewFolder.Default.Name"));
			builder.setParent(parentNode.getResourcePath());
			builder.setResourceBuilder(b->b.setFolder(true).setApplicationScope(ApplicationScope.DESIGNER));  // No data
			builder.setTitle(BundleUtil.get().getString(PREFIX+".NewFolder.Title"));
			builder.buildAndDisplay();
		}
	}

	private class ImportDiagramAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		private final DesignerContext context;
		private final AbstractResourceNavTreeNode parentNode;
		
		public ImportDiagramAction(DesignerContext ctx,AbstractResourceNavTreeNode pNode)  {
			super(PREFIX+".ImportDiagram",IconUtil.getIcon("import1"));  // preferences
			this.context = ctx;
			this.parentNode = pNode;
		}

		public void actionPerformed(ActionEvent e) {
			Objects.requireNonNull(parentNode);
			ImportDialog.ImportDialogBuilder builder = ImportDialog.newBuilder();
			builder.setContext(context);
			builder.setNoun(BundleUtil.get().getString(PREFIX+".DiagramNoun"));
			builder.setDefaultName(BundleUtil.get().getString(PREFIX+".ImportDiagram.Default.Name"));
			builder.setParent(parentNode.getResourcePath());
			builder.setParent(NavTreeFolder.this);
			builder.setResourceBuilder(b->b.setFolder(false).setApplicationScope(ApplicationScope.DESIGNER));  // No data
			builder.setTitle(PREFIX+".ImportDiagram.Title");
			builder.buildAndDisplay();
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
				ProcessDiagramView diagram = new ProcessDiagramView(context,res.getResourceId(),sd);
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
	@Override
	public boolean isChanged() {
		boolean changed =  statusManager.isModified(resourceId);
		//log.infof("%s.isChanged: %s modified = %s",CLSS,resourceId.getResourcePath().getPath().toString(),(changed?"true":"false"));
		return changed;
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
	// ************************ For sorting children by name *****************************
	private class SortByName implements Comparator<AbstractNavTreeNode>
	{
	    @Override
		public int compare(final AbstractNavTreeNode object1, final AbstractNavTreeNode object2) {
	          return object1.getName().compareTo(object2.getName());
	      }
	}
	
    // ************************ ProjectResourceListener *****************************
	/**
	 * We get here due to:
	 *    1) Name changes to resource
	 *    2) New resource from menu selection
	 * This is triggered by modification to a project.
	 */
	@Override
	public void resourcesCreated(String projectName,List<ChangeOperation.CreateResourceOperation> ops) {
		for(ChangeOperation.CreateResourceOperation op:ops ) {
			ProjectResourceId id = op.getResourceId();
			log.infof("%s.resourcesCreated: %s (%s)",CLSS,id.getFolderPath(),id.getResourcePath().getName());
			statusManager.setPendingName(id, id.getResourcePath().getName());
			updateUI();
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
			ProjectResourceId id = op.getResourceId();
			log.infof("%s.resourcesModified: %s",CLSS,id.getFolderPath());
			updateUI();
		}
	}
	
	// ************************ NavTreeNodeInterface **************************
	// These methods are called by the NodeStatusManager
	
	/**
	 * Update the node name italic/plain in nav tree.
	 * Note: This method should ONLY be called from the node status manager.
	 */
	public void updateUI() {
		boolean modified = isChanged();
		log.infof("%s.updateUI: %s modified = %s",CLSS,resourceId.getResourcePath().getPath().toString(),(modified?"true":"false"));
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				setItalic(modified);
				refresh();
				new WorkspaceRepainter().run();
			}
		});
	}
}
