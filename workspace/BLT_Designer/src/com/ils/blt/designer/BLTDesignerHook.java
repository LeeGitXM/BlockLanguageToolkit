/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;


import com.ils.blt.client.component.DiagramViewer;
import com.ils.blt.client.component.RecommendationMap;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.ApplicationScriptFunctions;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.factorypmi.designer.palette.model.DefaultPaletteItemGroup;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.AbstractDesignerModuleHook;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.SaveContext;
import com.inductiveautomation.vision.api.designer.VisionDesignerInterface;
import com.inductiveautomation.vision.api.designer.palette.JavaBeanPaletteItem;
import com.inductiveautomation.vision.api.designer.palette.Palette;
import com.inductiveautomation.vision.api.designer.palette.PaletteItemGroup;
import com.jidesoft.docking.DockingManager;

public class BLTDesignerHook extends AbstractDesignerModuleHook  {
	private static final String TAG = "BLTDesignerHook";
	public static String BLOCK_BUNDLE_NAME   = "block";        // Properties file is block.properties
	public static String HOOK_BUNDLE_NAME   = "designer";      // Properties file is designer.properties
	public static String PREFIX = BLTProperties.BUNDLE_PREFIX; // Properties is accessed by this prefix

	private GeneralPurposeTreeNode rootNode = null;
	private DesignerContext context = null;
	private final LoggerEx log;
	private DiagramWorkspace workspace = null;
	private ApplicationRequestHandler appRequestHandler = null;
	private final NodeStatusManager nodeStatusManager;
	
	// Register separate properties files for designer things and block things
	static {
		BundleUtil.get().addBundle(BLTProperties.BUNDLE_PREFIX,BLTDesignerHook.class,HOOK_BUNDLE_NAME);
		BundleUtil.get().addBundle(BLTProperties.BLOCK_PREFIX,BLTDesignerHook.class,BLOCK_BUNDLE_NAME);
	}
	
	public BLTDesignerHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		nodeStatusManager = new NodeStatusManager();
	}
	
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTProperties.APPLICATION_SCRIPT_PACKAGE,ApplicationScriptFunctions.class);
	}
	
	@Override
	public void startup(DesignerContext ctx, LicenseState activationState) throws Exception {
		this.context = ctx;
		WorkspaceRepainter.setContext(ctx);
		appRequestHandler = new ApplicationRequestHandler();
		context.addBeanInfoSearchPath("com.ils.blt.designer.component.beaninfos");
		
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
				log.infof("%s: Group not a DefaultPaletteItemGroup, is %s",TAG,group.getClass().getName());
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
				log.warnf("%s: Error creating vision palette entries (%s)",TAG,ie.getMessage());
			}

		}
		
		// Setup the diagram workspace
		workspace = new DiagramWorkspace(context);
		rootNode = new GeneralPurposeTreeNode(context);
		context.getProjectBrowserRoot().getProjectFolder().addChild(rootNode);
		context.registerResourceWorkspace(workspace);
	}
	
	public NodeStatusManager getNavTreeStatusManager() { return nodeStatusManager; }
	
	public DiagramWorkspace getWorkspace() { return workspace; }

	// Before the massive save, make sure that all dirty nodes have been
	// serialized into project resources.
	@Override
	public void notifyProjectSaveStart(SaveContext save) {
		log.infof("%s: NotifyProjectSaveStart",TAG);
		rootNode.saveAll();
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
				log.infof("%s: Hiding frame=%s",TAG,name);
			}
			else {
				log.infof("%s: Leaving frame=%s",TAG,name);
			}
		}
		log.infof("%s: Workspace=%s",TAG,dockManager.getWorkspace().getName());
		//Workspace wksp = dockManager.getWorkspace();
		// There is only 1 child of the workspace - the workspace mananger
	}

	
	public ApplicationRequestHandler getApplicationRequestHandler() { return appRequestHandler; }
	@Override
	public String getResourceCategoryKey(Project project,ProjectResource resource) {
		// There is only one resource category that we are exporting
		if( resource.getResourceType().equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
			return PREFIX+".Export.Application.Category";
		}
		else if( resource.getResourceType().equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
			return PREFIX+".Export.Family.Category";
		}
		else if( resource.getResourceType().equalsIgnoreCase(BLTProperties.DIAGRAM_RESOURCE_TYPE) ) {
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
}
