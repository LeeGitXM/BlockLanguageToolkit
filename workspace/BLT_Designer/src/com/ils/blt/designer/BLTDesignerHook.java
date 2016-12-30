/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;


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

import com.ils.blt.client.ClientScriptExtensionManager;
import com.ils.blt.client.component.diagview.DiagramViewer;
import com.ils.blt.client.component.recmap.RecommendationMap;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.ApplicationScriptFunctions;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.AbstractScriptExtensionManager;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.search.BLTSearchProvider;
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
import com.inductiveautomation.ignition.designer.model.menu.JMenuMerge;
import com.inductiveautomation.ignition.designer.model.menu.MenuBarMerge;
import com.inductiveautomation.ignition.designer.model.menu.WellKnownMenuConstants;
import com.inductiveautomation.vision.api.designer.VisionDesignerInterface;
import com.inductiveautomation.vision.api.designer.palette.JavaBeanPaletteItem;
import com.inductiveautomation.vision.api.designer.palette.Palette;
import com.inductiveautomation.vision.api.designer.palette.PaletteItemGroup;
import com.jidesoft.docking.DockingManager;

public class BLTDesignerHook extends AbstractDesignerModuleHook  {
	private static final String TAG = "BLTDesignerHook";
	private static final String INTERFACE_MENU_TITLE  = "External Interface Configuration";
	private static final String VALIDATION_MENU_TITLE = "Validate Diagrams";
	public static String BLOCK_BUNDLE_NAME   = "block";        // Properties file is block.properties
	public static String HOOK_BUNDLE_NAME   = "designer";      // Properties file is designer.properties
	public static String PREFIX = BLTProperties.BUNDLE_PREFIX; // Properties is accessed by this prefix

	private GeneralPurposeTreeNode rootNode = null;
	private DesignerContext context = null;
	private final LoggerEx log;
	private DiagramWorkspace workspace = null;
	private ApplicationRequestHandler appRequestHandler = null;
	private NodeStatusManager nodeStatusManager = null;
	private BLTSearchProvider searchProvider = null;
	
	// Register separate properties files for designer things and block things
	static {
		BundleUtil.get().addBundle(BLTProperties.BUNDLE_PREFIX,BLTDesignerHook.class,HOOK_BUNDLE_NAME);
		BundleUtil.get().addBundle(BLTProperties.BLOCK_PREFIX,BLTDesignerHook.class,BLOCK_BUNDLE_NAME);
	}
	
	public BLTDesignerHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTProperties.DIAGRAM_SCRIPT_PACKAGE,ApplicationScriptFunctions.class);
	}
	
	// Insert a menu to allow control of database and tag provider.
    @Override
    public MenuBarMerge getModuleMenu() {
 	
        MenuBarMerge merge = new MenuBarMerge(BLTProperties.MODULE_ID);  // as suggested in javadocs
        merge.addSeparator();

        Action setupAction = new AbstractAction(INTERFACE_MENU_TITLE) {
            private static final long serialVersionUID = 5374667367733312464L;
            public void actionPerformed(ActionEvent ae) {
                SwingUtilities.invokeLater(new SetupDialogRunner());
            }
        };
        
        Action validateAction = new AbstractAction(VALIDATION_MENU_TITLE) {
            private static final long serialVersionUID = 5374667367733312464L;
            public void actionPerformed(ActionEvent ae) {
                SwingUtilities.invokeLater(new ValidationDialogRunner());
            }
        };

        JMenuMerge controlMenu = new JMenuMerge(WellKnownMenuConstants.VIEW_MENU_NAME);
        if( !menuExists(context.getFrame(),INTERFACE_MENU_TITLE) ) {
        	controlMenu.add(setupAction);
        }
        controlMenu.add(validateAction);
        merge.add(WellKnownMenuConstants.VIEW_MENU_LOCATION, controlMenu);
        return merge;
    }
	
	@Override
	public void startup(DesignerContext ctx, LicenseState activationState) throws Exception {
		this.context = ctx;
		appRequestHandler = new ApplicationRequestHandler();
		nodeStatusManager = new NodeStatusManager(context,appRequestHandler);
		AuxiliaryDataRestoreManager.setContext(ctx);
		AuxiliaryDataSaveManager.setContext(ctx);
		ResourceCreateManager.setContext(ctx);
		ResourceDeleteManager.setContext(ctx);
		ResourceUpdateManager.setContext(ctx);
		ResourceSaveManager.setContext(ctx);
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
			
		    // Initialize all the script modules from parameters stored in the ORM.
			// We use all combinations of classes/flavors.
		    ClientScriptExtensionManager sem = ClientScriptExtensionManager.getInstance();
		    for( String flavor: sem.getFlavors() ) {
		    	for(String clss: sem.getClassNames() ) {
		    		String key = AbstractScriptExtensionManager.makeKey(clss, flavor);
			    	String pythonPath = appRequestHandler.getToolkitProperty(key);
			    	if( pythonPath!=null ) {
			    		sem.setModulePath(key, pythonPath);
			    		sem.addScript(clss,flavor, pythonPath);
			    	}
			    }
		    }
		}
		
		// Setup the diagram workspace
		workspace = new DiagramWorkspace(context);
		rootNode = new GeneralPurposeTreeNode(context);
		context.getProjectBrowserRoot().getProjectFolder().addChild(rootNode);
		context.registerResourceWorkspace(workspace);
		nodeStatusManager.createRootResourceStatus(rootNode);
		// Instantiate the notification handler so that we have notifications
		// ready when diagrams are displayed. The constructor is sufficient.
		NotificationHandler.getInstance();
		// Query the gateway for latest notifications from all blocks
		appRequestHandler.triggerStatusNotifications();
	}
	
	public NodeStatusManager getNavTreeStatusManager() { return nodeStatusManager; }
	
	public DiagramWorkspace getWorkspace() { return workspace; }

	// Before the massive save, make sure that all dirty nodes have been
	// serialized into project resources.
	@Override
	public void notifyProjectSaveStart(SaveContext save) {
		log.infof("%s: NotifyProjectSaveStart",TAG);
		ResourceSaveManager saver = new ResourceSaveManager(getWorkspace(),rootNode);
		saver.saveSynchronously();
		nodeStatusManager.updateAll();
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
            log.debugf("%s.Launching setup dialog...",TAG);
            ValidationDialog validator = new ValidationDialog(context);
            validator.pack();
            validator.setVisible(true);
        }
    }
}
