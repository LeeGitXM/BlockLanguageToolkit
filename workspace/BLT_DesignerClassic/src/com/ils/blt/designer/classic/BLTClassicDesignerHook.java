/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.classic;


import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JRootPane;
import javax.swing.SwingUtilities;

import com.ils.blt.client.component.diagview.DiagramViewer;
import com.ils.blt.client.component.recmap.RecommendationMap;
import com.ils.blt.common.ModuleRequestHandler;
import com.ils.blt.common.ModuleScriptFunctions;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.ResourceCreateManager;
import com.ils.blt.designer.ResourceDeleteManager;
import com.ils.blt.designer.ResourceSaveManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.SetupDialog;
import com.ils.blt.designer.ValidationDialog;
import com.ils.blt.designer.classic.workspace.ClassicDiagramWorkspace;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
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
import com.inductiveautomation.ignition.designer.model.menu.JMenuMerge;
import com.inductiveautomation.ignition.designer.model.menu.MenuBarMerge;
import com.inductiveautomation.ignition.designer.model.menu.WellKnownMenuConstants;
import com.inductiveautomation.vision.api.designer.VisionDesignerInterface;
import com.inductiveautomation.vision.api.designer.palette.JavaBeanPaletteItem;
import com.inductiveautomation.vision.api.designer.palette.Palette;
import com.inductiveautomation.vision.api.designer.palette.PaletteItemGroup;
import com.jidesoft.docking.DockingManager;

public class BLTClassicDesignerHook extends AbstractDesignerModuleHook  {
	private static final String TAG = "BLTDesignerHook";
	public static String HOOK_BUNDLE_NAME   = "designer";      // Properties file is designer.properties
	public static String PREFIX = BLTProperties.CUSTOM_PREFIX; // Properties is accessed by this prefix

	private GeneralPurposeTreeNode rootNode = null;
	private DesignerContext context = null;
	private final LoggerEx log;
	private ClassicDiagramWorkspace workspace = null;
	private ToolkitRequestHandler appRequestHandler = null;
	private final NodeStatusManager nodeStatusManager;
	
	// Register separate properties files for designer things and block things
	static {
		BundleUtil.get().addBundle(BLTProperties.CUSTOM_PREFIX,BLTClassicDesignerHook.class,HOOK_BUNDLE_NAME);
	}
	
	public BLTClassicDesignerHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		nodeStatusManager = new NodeStatusManager();
		appRequestHandler = new ModuleRequestHandler();
	}
	
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTProperties.CLASSIC_SCRIPT_PACKAGE,ModuleScriptFunctions.class);
	}
	
	// Insert a menu to allow control of database and tag provider.
    @Override
    public MenuBarMerge getModuleMenu() {
    	JMenuMerge controlMenu = new JMenuMerge(WellKnownMenuConstants.VIEW_MENU_NAME);
    	MenuBarMerge merge = new MenuBarMerge(BLTProperties.MODULE_ID);  // as suggested in javadocs
    	if( !menuExists(context.getFrame(),BLTProperties.INTERFACE_MENU_TITLE) ) {
    		merge.addSeparator();

    		Action setupAction = new AbstractAction(BLTProperties.INTERFACE_MENU_TITLE) {
    			private static final long serialVersionUID = 5374667367733312464L;
    			public void actionPerformed(ActionEvent ae) {
    				SwingUtilities.invokeLater(new SetupDialogRunner());
    			}
    		};
    		controlMenu.add(setupAction);
    	}
    	if( !menuExists(context.getFrame(),BLTProperties.VALIDATION_MENU_TITLE) ) {
    		Action validateAction = new AbstractAction(BLTProperties.VALIDATION_MENU_TITLE) {
    			private static final long serialVersionUID = 5374667367733312464L;
    			public void actionPerformed(ActionEvent ae) {
    				SwingUtilities.invokeLater(new ValidationDialogRunner());
    			}
    		};
    		controlMenu.add(validateAction);
    	}
  
        merge.add(WellKnownMenuConstants.VIEW_MENU_LOCATION, controlMenu);
        return merge;
    }
	
	@Override
	public void startup(DesignerContext ctx, LicenseState activationState) throws Exception {
		this.context = ctx;
		
		AuxiliaryDataRestoreManager.setContext(ctx);
		AuxiliaryDataSaveManager.setContext(ctx);
		ResourceCreateManager.setup(ctx);
		ResourceDeleteManager.setup(ctx,nodeStatusManager);
		ResourceUpdateManager.setup(ctx,nodeStatusManager,appRequestHandler);
		ResourceSaveManager.setup(ctx,nodeStatusManager,appRequestHandler);
		
		
		context.addBeanInfoSearchPath("com.ils.blt.designer.component.beaninfos");
		
		// Place icons for our custom widgets on the Vision palette
		VisionDesignerInterface vdi = 
					(VisionDesignerInterface) context.getModule(VisionDesignerInterface.VISION_MODULE_ID);

		if (vdi != null) {
			final Palette palette = vdi.getPalette();

			// Populate the palette
			PaletteItemGroup group = null;
			String paletteName = BundleUtil.get().getStringLenient(PREFIX+".Palette.Name");
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
					public String getShortDescription() { return BundleUtil.get().getStringLenient(BLTProperties.CUSTOM_PREFIX+".DiagramViewer.Desc"); }
					public String getDisplayName() { return BundleUtil.get().getStringLenient(BLTProperties.CUSTOM_PREFIX+".DiagramViewer.Display"); }
				};
				group.addPaletteItem(jbpi);
				jbpi =new JavaBeanPaletteItem(RecommendationMap.class){
					public String getShortDescription() { return BundleUtil.get().getStringLenient(BLTProperties.CUSTOM_PREFIX+".RecommendationMap.Desc"); }
					public String getDisplayName() { return BundleUtil.get().getStringLenient(BLTProperties.CUSTOM_PREFIX+".RecommendationMap.Display"); }
				};
				group.addPaletteItem(jbpi);
			}
			catch(Exception ie ) {
				log.warnf("%s: Error creating vision palette entries (%s)",TAG,ie.getMessage());
			}
			
		    // Initialize all the script modules from parameters stored in the ORM.
			// We use all combinations of classes/flavors.
		    ScriptExtensionManager sem = ScriptExtensionManager.getInstance();
		    for( String flavor: sem.getFlavors() ) {
		    	for(String clss: sem.getClassNames() ) {
		    		String key = ScriptExtensionManager.makeKey(clss, flavor);
			    	String pythonPath = appRequestHandler.getToolkitProperty(key);
			    	if( pythonPath!=null ) sem.addScript(clss,flavor, pythonPath);
			    }
		    }
		}
		
		// Setup the diagram workspace
		workspace = new ClassicDiagramWorkspace(context);
		rootNode = new ClassicTreeNode(context,workspace,appRequestHandler,nodeStatusManager);
		context.getProjectBrowserRoot().getProjectFolder().addChild(rootNode);
		context.registerResourceWorkspace(workspace);
		nodeStatusManager.createRootResourceStatus(rootNode);
		// Instantiate the notification handler so that we have notifications
		// ready when diagrams are displayed. The constructor is sufficient.
		NotificationHandler.getInstance();
		// Query the gateway for latest notifications from all blocks
		appRequestHandler.triggerStatusNotifications();
		WorkspaceRepainter.setup(ctx,workspace);
	}
	
	public NodeStatusManager getNavTreeStatusManager() { return nodeStatusManager; }
	
	public ClassicDiagramWorkspace getWorkspace() { return workspace; }

	// Before the massive save, make sure that all dirty nodes have been
	// serialized into project resources.
	@Override
	public void notifyProjectSaveStart(SaveContext save) {
		log.infof("%s: NotifyProjectSaveStart",TAG);
		ResourceSaveManager saver = new ResourceSaveManager(getWorkspace(),rootNode);
		saver.saveSynchronously();
		nodeStatusManager.cleanAll();
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

	@Override
	public String getResourceCategoryKey(Project project,ProjectResource resource) {
		// There is only one resource category that we are exporting
		if( resource.getResourceType().equalsIgnoreCase(BLTProperties.APPLICATION_RESOURCE_TYPE) ) {
			return PREFIX+".Export.Application.Category";
		}
		else if( resource.getResourceType().equalsIgnoreCase(BLTProperties.FAMILY_RESOURCE_TYPE) ) {
			return PREFIX+".Export.Family.Category";
		}
		else if( resource.getResourceType().equalsIgnoreCase(BLTProperties.CLASSIC_DIAGRAM_RESOURCE_TYPE) ) {
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
	// Search the menu tree to see if the same menu has been added by another module
	private boolean menuExists(Frame frame,String title) {
		for(Component c:context.getFrame().getComponents() ) {
    		if( c instanceof JRootPane ) {
    			JRootPane root = (JRootPane)c;
    			JMenuBar bar = root.getJMenuBar();
    			if( bar!=null ) {
    				int count = bar.getMenuCount();
    				int index = 0;
    				while( index<count) {
    					JMenu menu = bar.getMenu(index);
    					if( menu.getName().equalsIgnoreCase(WellKnownMenuConstants.VIEW_MENU_NAME)) {
    						int nitems = menu.getItemCount();
    						int jndex = 0;
    						log.tracef("%s: found VIEW menu",TAG);
    						while(jndex<nitems ) {
    							JMenuItem item = menu.getItem(jndex);
    							if( item!=null ) {
    								String name = item.getText();
        							log.tracef("%s: found %s",TAG,name);
        							if( title.equalsIgnoreCase(name)) return true;
    							}
    							jndex++;
    						}
    						break;
    					}
    					index++;
    				}
    			}
    		}
    	}
		
		return false;
	}
	 /**
     * Display a popup dialog for configuration of dialog execution parameters.
     * Run in a separate thread, as a modal dialog in-line here will freeze the UI.
     */
    private class SetupDialogRunner implements Runnable {

        public void run() {
            log.debugf("%s.Launching setup dialog...",TAG);
            SetupDialog setup = new SetupDialog(context);
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
            log.debugf("%s.Launching setup dialog...",TAG);
            ValidationDialog validator = new ValidationDialog(context);
            validator.pack();
            validator.setVisible(true);
        }
    }
}