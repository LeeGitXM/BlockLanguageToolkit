/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.schematic;


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

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ModuleScriptFunctions;
import com.ils.blt.common.ToolkitRequestHandler;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.ResourceCreateManager;
import com.ils.blt.designer.ResourceDeleteManager;
import com.ils.blt.designer.ResourceSaveManager;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.ValidationDialog;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.schematic.workspace.SchematicDiagramWorkspace;
import com.ils.blt.designer.search.BLTSearchProvider;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.AbstractDesignerModuleHook;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.SaveContext;
import com.inductiveautomation.ignition.designer.model.menu.JMenuMerge;
import com.inductiveautomation.ignition.designer.model.menu.MenuBarMerge;
import com.inductiveautomation.ignition.designer.model.menu.WellKnownMenuConstants;
import com.jidesoft.docking.DockingManager;

public class BLTSchematicDesignerHook extends AbstractDesignerModuleHook  {
	private static final String TAG = "BLTDesignerHook";

	public static String HOOK_BUNDLE_NAME   = "designer";      // Properties file is designer.properties
	public static String PREFIX = BLTProperties.BUNDLE_PREFIX; // Properties is accessed by this prefix

	private GeneralPurposeTreeNode rootNode = null;
	private DesignerContext context = null;
	private final LoggerEx log;
	private SchematicDiagramWorkspace workspace = null;
	private ToolkitRequestHandler appRequestHandler = null;
	private final NodeStatusManager nodeStatusManager;
	private BLTSearchProvider searchProvider = null;
	
	// Register separate properties files for designer things and block things
	static {
		BundleUtil.get().addBundle(BLTProperties.BUNDLE_PREFIX,BLTSchematicDesignerHook.class,HOOK_BUNDLE_NAME);
	}
	
	public BLTSchematicDesignerHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		appRequestHandler = new SchematicRequestHandler();
		nodeStatusManager = new NodeStatusManager();
	}
	
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		ModuleScriptFunctions.setRequestHandler(appRequestHandler);
		mgr.addScriptModule(BLTProperties.SCHEMATIC_SCRIPT_PACKAGE,ModuleScriptFunctions.class);
		ScriptExtensionManager.getInstance().setToolkitRequestHandler(appRequestHandler);
	}
	
	// Insert a menu to allow control of database and tag provider.
    @Override
    public MenuBarMerge getModuleMenu() {
    	JMenuMerge controlMenu = new JMenuMerge(WellKnownMenuConstants.VIEW_MENU_NAME);
    	MenuBarMerge merge = new MenuBarMerge(BLTProperties.SCHEMATIC_MODULE_ID);  // as suggested in javadocs
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
		ResourceCreateManager.setup(ctx);
		ResourceDeleteManager.setup(ctx,nodeStatusManager);
		ResourceUpdateManager.setup(ctx,nodeStatusManager,appRequestHandler);
		ResourceSaveManager.setup(ctx,nodeStatusManager,appRequestHandler);
		context.addBeanInfoSearchPath("com.ils.blt.designer.component.beaninfos");
		
		
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

		// Setup the diagram workspace
		workspace = new SchematicDiagramWorkspace(context,appRequestHandler);
		rootNode = new SchematicTreeNode(context,workspace,appRequestHandler,nodeStatusManager);
		context.getProjectBrowserRoot().getProjectFolder().addChild(rootNode);
		context.registerResourceWorkspace(workspace);
		nodeStatusManager.createRootResourceStatus(rootNode);
		// Instantiate the notification handler so that we have notifications
		// ready when diagrams are displayed. The constructor is sufficient.
		NotificationHandler.getInstance();
		WorkspaceRepainter.setup(ctx,workspace);
		// Configure find/replace
		searchProvider = new BLTSearchProvider(context,rootNode.getName(),appRequestHandler,nodeStatusManager);
		context.registerSearchProvider(searchProvider);
	}
	
	public NodeStatusManager getNavTreeStatusManager() { return nodeStatusManager; }
	
	public SchematicDiagramWorkspace getWorkspace() { return workspace; }

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
		if( resource.getResourceType().equalsIgnoreCase(BLTProperties.SCHEMATIC_DIAGRAM_RESOURCE_TYPE) ) {
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
            SchematicSetupDialog setup = new SchematicSetupDialog(context,appRequestHandler);
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
            ValidationDialog validator = new ValidationDialog(context,appRequestHandler);
            validator.pack();
            validator.setVisible(true);
        }
    }
}
