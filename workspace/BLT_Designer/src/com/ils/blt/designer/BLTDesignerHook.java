/**
 *   (c) 2013-2022 ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;


import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.io.IOException;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JRootPane;
import javax.swing.SwingUtilities;

import org.apache.log4j.Logger;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.ApplicationScriptFunctions;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.navtree.NavTreeFolder;
import com.ils.blt.designer.search.BLTSearchProvider;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceBackgroundRepainter;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.ils.common.component.DiagramViewer;
import com.ils.common.component.recmap.RecommendationMap;
import com.inductiveautomation.factorypmi.designer.palette.model.DefaultPaletteItemGroup;
import com.inductiveautomation.ignition.client.util.action.StateChangeAction;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.StringPath;
import com.inductiveautomation.ignition.common.gson.JsonElement;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceBuilder;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourceNamingException;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.AbstractDesignerModuleHook;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.SaveContext;
import com.inductiveautomation.ignition.designer.model.menu.JMenuMerge;
import com.inductiveautomation.ignition.designer.model.menu.MenuBarMerge;
import com.inductiveautomation.ignition.designer.model.menu.WellKnownMenuConstants;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
import com.inductiveautomation.ignition.designer.project.DesignableProject;
import com.inductiveautomation.vision.api.designer.VisionDesignerInterface;
import com.inductiveautomation.vision.api.designer.palette.JavaBeanPaletteItem;
import com.inductiveautomation.vision.api.designer.palette.Palette;
import com.inductiveautomation.vision.api.designer.palette.PaletteItemGroup;
import com.jidesoft.docking.DockingManager;


public class BLTDesignerHook extends AbstractDesignerModuleHook  {
	private static final String CLSS = "BLTDesignerHook";
	private static final String INTERFACE_MENU_TITLE  = "External Interface Configuration";
	private static final String VALIDATION_MENU_TITLE = "Validate Diagrams";
	public static String BLOCK_BUNDLE_NAME   = "block";        // Properties file is block.properties
	public static String HOOK_BUNDLE_NAME   = "designer";      // Properties file is designer.properties
	public static String PREFIX = BLTProperties.BUNDLE_PREFIX; // Properties is accessed by this prefix

	private NavTreeFolder rootNode = null;
	private static DesignerContext context = null;
	private final LoggerEx log;
	private DiagramWorkspace workspace = null;
	private ApplicationRequestHandler appRequestHandler = null;
	private BLTSearchProvider searchProvider = null;
	private boolean diagramsAttached = true;
	
	// Register separate properties files for designer things and block things
	static {
		BundleUtil.get().addBundle(BLTProperties.BUNDLE_PREFIX,BLTDesignerHook.class,HOOK_BUNDLE_NAME);
		BundleUtil.get().addBundle(BLTProperties.BLOCK_PREFIX,BLTDesignerHook.class,BLOCK_BUNDLE_NAME);
	}
	
	public BLTDesignerHook() {
		log = LogUtil.getLogger(getClass().getPackageName());
	}
	
	public static DesignerContext getContext() { return context; }
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTProperties.DIAGRAM_SCRIPT_PACKAGE,ApplicationScriptFunctions.class);
	}


	// Insert a menu to allow control of database and tag provider.
    @Override
    public MenuBarMerge getModuleMenu() {
    	//log.infof("DesignerHook in Menu merge %s",(diagramsAttached?"TRUE":"FALSE"));
    	MenuBarMerge merge = new MenuBarMerge(BLTProperties.MODULE_ID);  // as suggested in javadocs
    	merge.addSeparator();
    	merge.addSeparator();

    	JMenuMerge toolsMenu = new JMenuMerge(WellKnownMenuConstants.TOOLS_MENU_NAME);
    	Action setupAction = new AbstractAction(INTERFACE_MENU_TITLE) {
    		private static final long serialVersionUID = 5374667367733312464L;
    		public void actionPerformed(ActionEvent ae) {
    			SwingUtilities.invokeLater(new SetupDialogRunner());
    		}
    	};

    	StateChangeAction attachAction = new StateChangeAction(BLTProperties.BUNDLE_PREFIX+".Menu.Tools.Attach") {
    		private static final long serialVersionUID = 5374556367733312464L; 
    		public void itemStateChanged(ItemEvent event) {
    			diagramsAttached = (event.getStateChange()==ItemEvent.SELECTED);
    		}
    	};
    	attachAction.setSelected(true);
    	toolsMenu.addCheckBox(attachAction);

    	Action validateAction = new AbstractAction(VALIDATION_MENU_TITLE) {
    		private static final long serialVersionUID = 5374667367733312464L;
    		public void actionPerformed(ActionEvent ae) {
    			SwingUtilities.invokeLater(new ValidationDialogRunner());
    		}
    	};

    	toolsMenu.add(validateAction);

    	merge.add(WellKnownMenuConstants.TOOLS_MENU_LOCATION, toolsMenu);
    	merge.addSeparator();
    	merge.addSeparator();
    	return merge;
    }

	@Override
	public void startup(DesignerContext ctx, LicenseState activationState) throws Exception {
		context = ctx;
		appRequestHandler = new ApplicationRequestHandler();
		ResourceCreateManager.setContext(ctx);
		ResourceDeleteManager.setContext(ctx);
		ResourceUpdateManager.setContext(ctx);
		ResourceSaveManager.setContext(ctx);
		WorkspaceBackgroundRepainter.setContext(ctx);
		WorkspaceRepainter.setContext(ctx);
		context.addBeanInfoSearchPath("com.ils.blt.designer.component.beaninfos");
		searchProvider = new BLTSearchProvider(context);
		context.registerSearchProvider(searchProvider);
			
		// Place icons for our custom widgets on the Vision palette
		VisionDesignerInterface vdi = 
					(VisionDesignerInterface) context.getModule(VisionDesignerInterface.VISION_MODULE_ID);

		if (vdi != null) {
			final Palette palette = vdi.getPalette();

			// Populate the palette
			PaletteItemGroup group = null;
			String paletteName = BundleUtil.get().getString(PREFIX+".Palette.Name");
			if ((group = palette.getGroup(paletteName)) == null) {
				group = palette.addGroup(paletteName);
			}
	
			if( group instanceof DefaultPaletteItemGroup ) {
				// The icon is located in vis-designer/images/incors
				((DefaultPaletteItemGroup) group).setIcon(IconUtil.getIcon("add_child"));

			}
			else {
				log.infof("%s.startup: Group not a DefaultPaletteItemGroup, is %s",CLSS,group.getClass().getName());
			}
			JavaBeanPaletteItem jbpi = null;
			try {
				jbpi = new JavaBeanPaletteItem(DiagramViewer.class) {
					public String getShortDescription() { return BundleUtil.get().getString(BLTProperties.BLOCK_PREFIX+".DiagramViewer.Desc"); }
					public String getDisplayName() { return BundleUtil.get().getString(BLTProperties.BLOCK_PREFIX+".DiagramViewer.Display"); }
				};
				group.addPaletteItem(jbpi);
				jbpi =new JavaBeanPaletteItem(RecommendationMap.class){
					public String getShortDescription() { return BundleUtil.get().getString(BLTProperties.BLOCK_PREFIX+".RecommendationMap.Desc"); }
					public String getDisplayName() { return BundleUtil.get().getString(BLTProperties.BLOCK_PREFIX+".RecommendationMap.Display"); }
				};
				group.addPaletteItem(jbpi);
			}
			catch(Exception ie ) {
				log.warnf("%s: Error creating vision palette entries (%s)",CLSS,ie.getMessage());
			}
			
		}
		
		// Setup the diagram workspace
		workspace = new DiagramWorkspace(context);
		ProjectResource rootResource = findRootResource();
		if( rootResource==null )  rootResource = createRootResource();
		rootNode = new NavTreeFolder(context,rootResource);
		context.getProjectBrowserRoot().addChild(rootNode);
		context.registerResourceWorkspace(workspace);
		NodeStatusManager.getInstance().createRootResourceStatus(rootNode);
		// Instantiate the notification handler so that we have notifications
		// ready when diagrams are displayed. The constructor is sufficient.
		NotificationHandler.getInstance().setHook(this);
		// Query the gateway for latest notifications from all blocks
		appRequestHandler.triggerStatusNotifications(context.getProjectName());
		LogUtil.getLogger(Logger.getRootLogger().getName());  // Cause the logger to be created
		log.infof("%s.startup: ===== Complete ======",CLSS);
		
		listProjectResources();
	}
	
	public DiagramWorkspace getWorkspace() { return workspace; }

	// Before the massive save, make sure that all dirty nodes have been
	// serialized into project resources.
	@Override
	public void notifyProjectSaveStart(SaveContext save) {
		log.infof("%s.notifyProjectSaveStart --------",CLSS);
		
		ResourceSaveManager saver = new ResourceSaveManager(getWorkspace(),rootNode);
		//saver.saveSynchronously();
	}
	
	
	/**
	 * Iterate over all the dockable frames. Close any that are not useful.
	 */
	public void resetPanelsForDiagnostics() {
		DockingManager dockManager = context.getDockingManager();
		for(String name:dockManager.getAllFrameNames()) {
			if( name.equalsIgnoreCase("OPC Browser")            ||
				name.equalsIgnoreCase("Charts")                 ||
				name.equalsIgnoreCase("DocEditor")              ||
				name.equalsIgnoreCase("QueryBrowser")           ||
				name.equalsIgnoreCase("Fill-and-Stroke")        ||
				name.equalsIgnoreCase("Palette - Collapsible")  ||
				name.equalsIgnoreCase("Palette - Tabbed")          ) {
				dockManager.hideFrame(name);
				log.infof("%s: Hiding frame=%s",CLSS,name);
			}
			else {
				log.infof("%s: Leaving frame=%s",CLSS,name);
			}
		}
		log.infof("%s: Workspace=%s",CLSS,dockManager.getWorkspace().getName());
		//Workspace wksp = dockManager.getWorkspace();
		// There is only 1 child of the workspace - the workspace mananger
	}

	public boolean attachDiagrams() { return diagramsAttached; }
	@Override
	public String getResourceCategoryKey(ProjectResourceId resourceId) {
		// There is only one resource category that we are exporting
		if( resourceId.getResourceType().getTypeId().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
			return PREFIX+".Export.Diagram.Category";
		}
		else { 
			return PREFIX+".Export.Generic.Category";   // Folders
		}
	}
	
	@Override
	public void shutdown() {
		super.shutdown();
	}


	/**
     * Display a popup dialog for configuration of dialog execution parameters.
     * Run in a separate thread, as a modal dialog in-line here will freeze the UI.
     */
    private class SetupDialogRunner implements Runnable {

        public void run() {
            log.debugf("%s.Launching setup dialog...",CLSS);
            ExternalInterfaceConfigurationDialog setup = new ExternalInterfaceConfigurationDialog(context);
            setup.pack();
            setup.setVisible(true);
        }
    }
    /**
     * Display a popup dialog for configuration of dialog execution parameters.
     * Run in a separate thread, as a modal dialog in-line here will freeze the UI.
     */
    private class ValidationDialogRunner implements Runnable {

        public void run() {
            log.debugf("%s.Launching setup dialog...",CLSS);
            ValidationDialog validator = new ValidationDialog(context);
            validator.pack();
            validator.setVisible(true);
        }
    }

	public String parseChildForDiagnosisName(AbstractResourceNavTreeNode theNode, String name) {
		String ret = "";
		if (theNode.getResourceId().getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
			Optional<ProjectResource> optional = theNode.getProjectResource();
			ProjectResource res = optional.get();
			String json = new String(res.getData());
			SerializableDiagram sd = null;
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			try {
				sd = mapper.readValue(json,SerializableDiagram.class);
			} 
			catch (JsonParseException jpe) {
				log.warnf("%s: open parse exception (%s)",CLSS,jpe.getLocalizedMessage());
			} 
			catch (JsonMappingException jme) {
				log.warnf("%s: open mapping exception (%s)",CLSS,jme.getLocalizedMessage());
			} 
			catch (IOException ioe) {
				log.warnf("%s: open io exception (%s)",CLSS,ioe.getLocalizedMessage());
			}
			ProcessDiagramView diagram = new ProcessDiagramView(res.getResourceId(),sd, context);
			for( Block blk:diagram.getBlocks()) {
				ProcessBlockView pbv = (ProcessBlockView)blk;
				if (pbv.isDiagnosis()) {
					if (pbv.getName().equalsIgnoreCase(name)) {
						ret += "Duplicate " + pbv.getClassName() + " block named " + pbv.getName() + " found in diagram " + diagram.getName() + "\r\n";
					}
				}
			}
		} 
		else {
			Enumeration<AbstractResourceNavTreeNode> enumer = theNode.children();
			while(enumer.hasMoreElements()) {
				AbstractResourceNavTreeNode aNode = enumer.nextElement();
				ret += parseChildForDiagnosisName(aNode, name);
			}
		}
		return ret;
	}
	/*
	 * The root resource has an empty path and a blt.diagram type.
	 * No need to add the module root as it will be created automatically.
	 * This resource is paired with the root of the BLT nav tree.
	 */
	private ProjectResource createRootResource() {
		ProjectResourceBuilder builder = ProjectResource.newBuilder();
		ResourceType rtype = BLTProperties.DIAGRAM_RESOURCE_TYPE;   // blt.diagram
		ProjectResourceId resourceId = new ProjectResourceId(context.getProjectName(),rtype,null);
		builder.setResourceId(resourceId);
		ResourcePath path = new ResourcePath(rtype,StringPath.ROOT);
		builder.setResourcePath(path);
		builder.setFolder(true);
		builder.setProjectName(context.getProjectName());
		ProjectResource pr = builder.build();
		DesignableProject dp = context.getProject();
		try {
			dp.createResource(pr);
		}
		catch(ResourceNamingException rne) {
			log.warnf("%s.createProjectResource: naming exception (%s)",CLSS,rne.getLocalizedMessage());
		}
		return pr;
	}
	
	private ProjectResource findRootResource() {
		ProjectResource root = null;
		List<ProjectResource> resources = context.getProject().getResources();
		for(ProjectResource pr:resources) {
			ResourceType rt = pr.getResourceType();
			if( rt.equals(BLTProperties.DIAGRAM_RESOURCE_TYPE) &&
				pr.getResourcePath().getFolderPath().isEmpty() ) {
				root = pr;
				break;
			}
		}
		return root;
	}
	/*
	 * For debugging - list the resources attributed to the current project
	 */
	private void listProjectResources() {
		log.infof("%s.listProjectResources: ===========",CLSS);
		List<ProjectResource> resources = context.getProject().getResources();
		for(ProjectResource pr:resources) {
			ResourceType rt = pr.getResourceType();
			if( pr.getResourcePath().getPath()==null ) {
				log.infof("    %s: null path type(%s,%s)",pr.getResourceName(),(rt==null?"":rt.getModuleId()),(rt==null?"":rt.getTypeId()));
			}
			else {
				String parent = "null";
				if(pr.getResourcePath().getParentPath()!=null ) parent = pr.getResourcePath().getParentPath();
				log.infof("    %s:%s%s\t, parent=%s, type(%s,%s)",
						pr.getResourceName(),
						pr.getResourcePath().getPath().toString(),
						(pr.isFolder()?" (folder)":""),
						parent,
						(rt==null?"":rt.getModuleId()),(rt==null?"":rt.getTypeId()) );
				Map<String,JsonElement> attributes = pr.getAttributes();
				for(String key:attributes.keySet()) {
					log.infof("        %s (%s)",key,attributes.get(key).toString());
				}
			}
		}
		log.infof("%s.listProjectResources: ===== Complete ======",CLSS);
	}
}
