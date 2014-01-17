/**iagram
 *   (c) 2013  ILS Automation. All rights reseiagramrved.
 *  
 *  Based on sample code provided by Inductive Automation.
 */
package com.ils.blt.designer.navtree;

import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.util.List;
import java.util.UUID;

import javax.swing.JPopupMenu;
import javax.swing.tree.TreePath;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.BLTDesignerHook;
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
	protected DiagramAction diagramAction = null;
	protected ExportAction exportAction = null;
	protected ImportAction importAction = null;
	private final DiagramWorkspace workspace; 
	

	/** 
	 * Create a new folder node representing the root folder
	 * @param context the designer context
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
			applicationAction = new ApplicationAction(this.folderId);
			debugAction = new DebugAction();
			menu.add(applicationAction);
			menu.addSeparator();
			menu.add(debugAction);
		}
		else if( getDepth()==DIAGRAM_DEPTH) {
			diagramAction = new DiagramAction();
			importAction = new ImportAction();
			menu.add(diagramAction);
			menu.add(importAction);
			menu.addSeparator();
			addEditActions(menu);
			
		}
		else {   // Depth == 2 and DIAGRAM_DEPTH==3
			familyAction = new FamilyAction(this.folderId);
			exportAction = new ExportAction();
			menu.add(familyAction);
			menu.add(exportAction);
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
		//final GsonBuilder gsonBuilder = new GsonBuilder();
		
		//gsonBuilder.registerTypeAdapter(SerializableDiag.class, new DiagramSerializer());
		//gsonBuilder.setPrettyPrinting();
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
    //  TODO: Need file chooser and export
    private class ExportAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public ExportAction()  {
	    	super(PREFIX+".ExportDiagram",IconUtil.getIcon("export1"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".DefaultExportDiagramName");
				log.infof("%s: export diagram action ...",TAG);
				if( newName==null) newName = "Exported Diag";  // Missing string resource
				SerializableDiagram diagram = new SerializableDiagram();
				diagram.setName(newName);

				String json = serializeDiagram(diagram);
				
				log.infof("%s: DiagramAction. json=%s",TAG,json);
				byte[] bytes = json.getBytes();
				log.debugf("%s: DiagramAction. export %s resource %d (%d bytes)",TAG,BLTProperties.MODEL_RESOURCE_TYPE,
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
    //  TODO: Need file chooser and import
    private class ImportAction extends BaseAction {
    	private static final long serialVersionUID = 1L;
	    public ImportAction()  {
	    	super(PREFIX+".ImportDiagram",IconUtil.getIcon("import1"));  // preferences
	    }
	    
		public void actionPerformed(ActionEvent e) {
			try {
				final long newId = context.newResourceId();
				String newName = BundleUtil.get().getString(PREFIX+".DefaultImportDiagramName");
				if( newName==null) newName = "Imported Diag";  // Missing string resource
				SerializableDiagram diagram = new SerializableDiagram();
				diagram.setName(newName);
				log.infof("%s: import diagram action ...",TAG);
				String json = serializeDiagram(diagram);
				log.debugf("%s: DiagramAction. json=%s",json);
				byte[] bytes = json.getBytes();    
				log.debugf("%s: DiagramAction. import %s resource %d (%d bytes)",TAG,BLTProperties.MODEL_RESOURCE_TYPE,
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
