/**
 *   (c) 2013  ILS Automation. All rights reserved.
 *  
 *  Based on sample code provided by Inductive Automation.
 */
package com.ils.blt.designer.navtree;

import java.awt.event.ActionEvent;
import java.util.List;
import java.util.UUID;

import javax.swing.JPopupMenu;
import javax.swing.tree.TreePath;

import com.ils.blt.designer.workspace.DiagnosticsWorkspace;
import com.ils.diagnostics.common.DTProperties;
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
public class DiagnosticsFolderNode extends FolderNode {
	private static final String TAG = "DiagnosticsFolderNode:";
	private static final String NAV_PREFIX = "NavTree";       // Required for some defaults
	private static final String BUNDLE_NAME = "navtree";      // Name of properties file
	private static final int DIAGRAM_DEPTH = 2;                   // For a two-tier menu
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	// These are the various actions beyond defaults
	private ClearAction clearAction = null;
	private DebugAction debugAction = null;
	private ApplicationAction applicationAction = null;
	private FamilyAction familyAction = null;
	protected DiagramAction diagramAction = null;

	/** 
	 * Create a new folder node representing the root folder
	 * @param context the designer context
	 */
	public DiagnosticsFolderNode(DesignerContext ctx) {
		super(ctx, DTProperties.MODULE_ID, ApplicationScope.GATEWAY,DTProperties.ROOT_FOLDER_UUID);
		BundleUtil.get().addBundle(NAV_PREFIX,getClass(),BUNDLE_NAME);
		setText(BundleUtil.get().getString(NAV_PREFIX+".RootFolderName"));
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
	public DiagnosticsFolderNode(DesignerContext context,ProjectResource resource) {
		super(context, resource);
		setIcon(IconUtil.getIcon("folder_closed"));
	}

	private boolean isRootFolder() {
		return getFolderId().equals(DTProperties.ROOT_FOLDER_UUID);
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
			node = new DiagnosticsFolderNode(context, res);
			node.install(this);
			log.debug(TAG+"createChildFolder:"+this.pathToRoot()+"->"+node.pathToRoot());
			return node;
		}
		else if (DTProperties.PANEL_RESOURCE_TYPE.equals(res.getResourceType())) {
			node = new DiagnosticsNode(context,res,res.getName());
			node.install(this);
			log.debug(TAG+"createChildPanel:"+this.pathToRoot()+"->"+node.pathToRoot());
			return node;
		} 
		else {
			throw new IllegalArgumentException();
		}
	}
	
	@Override
	public String getWorkspaceName() {
		return DiagnosticsWorkspace.getInstance().getKey();
	}
	
	@Override
	public boolean isEditActionHandler() {
		return true;
	}
	/**
	 * Define the menu used for popups. This appears to be called only once for each node.
	 */
	@Override
	protected void initPopupMenu(JPopupMenu menu, TreePath[] paths,List<AbstractNavTreeNode> selection, int modifiers) {
		setupEditActions(paths, selection);
		
		if (isRootFolder()) { 
			applicationAction = new ApplicationAction(this.folderId);
			clearAction = new ClearAction();
			debugAction = new DebugAction();
			menu.add(applicationAction);
			menu.addSeparator();
			menu.add(clearAction);
			menu.add(debugAction);
		}
		else if( getDepth()==DIAGRAM_DEPTH) {
			diagramAction = new DiagramAction();
			menu.add(diagramAction);
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
			if (node instanceof DiagnosticsNode) {
				((DiagnosticsNode) node).closeAndCommit();
			}
		}

		ResourceDeleteAction delete = new ResourceDeleteAction(context,
				(List<AbstractResourceNavTreeNode>) children,
				reason.getActionWordKey(), (getDepth()==1? (NAV_PREFIX+".ApplicationNoun"):(NAV_PREFIX+".FamilyNoun")));
		if (delete.execute()) {
			UndoManager.getInstance().add(delete, DiagnosticsFolderNode.class);
		}
	}

	@Override
	public void onSelected() {
		UndoManager.getInstance()
				.setSelectedContext(DiagnosticsFolderNode.class);
	}
	
	// ==================================== Action Classes ==================================
	// From the root node, delete all garbage workspace resources
	private class ClearAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public ClearAction()  {
			super(NAV_PREFIX+".Clean",IconUtil.getIcon("trafficlight_yellow"));
		}

		
		public void actionPerformed(ActionEvent e) {
			List <ProjectResource> resources = context.getProject().getResources();
			for( ProjectResource panel : resources ) {
				if( panel.getResourceType().equalsIgnoreCase(DTProperties.PANEL_RESOURCE_TYPE) &&
					panel.getData().length!=16) {
					log.info("   --- deleting panel "+panel.getResourceId()+" "+panel.getName()+" illegal contents----"); 
					context.getProject().deleteResource(panel.getResourceId());
				}
			}
			resources = context.getProject().getResources();
			for( ProjectResource res : resources ) {
				if( res.getResourceType().equalsIgnoreCase(DTProperties.MODEL_RESOURCE_TYPE)) {
					log.info("Found model resource "+res.getResourceId()+" "+res.getName()+"(parent="+res.getParentUuid()+")");
					boolean hasParent = false;
					List <ProjectResource> panels = context.getProject().getResources();
					for( ProjectResource panel : panels ) {
						if( panel.getResourceType().equalsIgnoreCase(DTProperties.PANEL_RESOURCE_TYPE) &&
							panel.getDataAsUUID().equals(res.getParentUuid())) {
							hasParent = true;
							break;
						}
					}
					if( !hasParent ) {
						log.info("Model resource "+res.getResourceId()+" is an orphan, deleting ...");
						context.getProject().deleteResource(res.getResourceId());
					}
				}
			}
		}
	}
	// From the root node, recursively log the contents of the tree
	private class DebugAction extends BaseAction {
		private static final long serialVersionUID = 1L;
		public DebugAction()  {
			super(NAV_PREFIX+".Debug",IconUtil.getIcon("bug_yellow"));
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
	    	super(NAV_PREFIX+".NewApplication",IconUtil.getIcon("folder_new"));
	    	this.parent = parentUUID;
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(NAV_PREFIX+".DefaultNewApplicationName");
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
	    	super(NAV_PREFIX+".NewFamily",IconUtil.getIcon("folder_new"));
	    	this.parent = parentUUID;
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(NAV_PREFIX+".DefaultNewFamilyName");
				if( newName==null) newName = "New Folks";  // Missing Resource
				context.addFolder(newId,moduleId,ApplicationScope.GATEWAY,newName,parent);
				selectChild(newId);
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
			}
		}
	}
    private class DiagramAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public DiagramAction()  {
	    	super(NAV_PREFIX+".NewDiagram",IconUtil.getIcon("folder_new"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(NAV_PREFIX+".DefaultNewDiagramName");
				if( newName==null) newName = "New Diag";  // Missing string resource
				UUID uuid = UUID.randomUUID();
				byte[] bytes = uuid.toString().getBytes();
				
				ProjectResource resource = new ProjectResource(newId,
						DTProperties.MODULE_ID, DTProperties.PANEL_RESOURCE_TYPE,
						newName, ApplicationScope.GATEWAY, bytes);
				resource.setParentUuid(getFolderId());
				context.updateResource(resource);
				selectChild(newId);
			} 
			catch (Exception err) {
				ErrorUtil.showError(err);
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
}
