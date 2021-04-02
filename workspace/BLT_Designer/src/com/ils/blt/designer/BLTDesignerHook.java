/**
 *   (c) 2013-2021 ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;


import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.io.IOException;
import java.util.Enumeration;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JRootPane;
import javax.swing.SwingUtilities;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.ApplicationScriptFunctions;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.search.BLTSearchProvider;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.ils.common.component.DiagramViewer;
import com.ils.common.component.recmap.RecommendationMap;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.factorypmi.designer.palette.model.DefaultPaletteItemGroup;
import com.inductiveautomation.ignition.client.util.action.StateChangeAction;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.AbstractDesignerModuleHook;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.SaveContext;
import com.inductiveautomation.ignition.designer.model.menu.JMenuMerge;
import com.inductiveautomation.ignition.designer.model.menu.MenuBarMerge;
import com.inductiveautomation.ignition.designer.model.menu.WellKnownMenuConstants;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;
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

	private GeneralPurposeTreeNode rootNode = null;
	private DesignerContext context = null;
	private final ILSLogger log;
	private DiagramWorkspace workspace = null;
	private ApplicationRequestHandler appRequestHandler = null;
	private NodeStatusManager nodeStatusManager = null;
	private BLTSearchProvider searchProvider = null;
	private boolean diagramsAttached = true;
	
	// Register separate properties files for designer things and block things
	static {
		BundleUtil.get().addBundle(BLTProperties.BUNDLE_PREFIX,BLTDesignerHook.class,HOOK_BUNDLE_NAME);
		BundleUtil.get().addBundle(BLTProperties.BLOCK_PREFIX,BLTDesignerHook.class,BLOCK_BUNDLE_NAME);
	}
	
	public BLTDesignerHook() {
		log = LogMaker.getLogger(this);
	}
	
	
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
    	if( !menuExists(context.getFrame(),INTERFACE_MENU_TITLE) ) {
    		toolsMenu.add(setupAction);
    	}

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
		this.context = ctx;
		appRequestHandler = new ApplicationRequestHandler();
		nodeStatusManager = new NodeStatusManager(context,appRequestHandler);
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
				log.infof("%s: Group not a DefaultPaletteItemGroup, is %s",CLSS,group.getClass().getName());
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
		rootNode = new GeneralPurposeTreeNode(context);
		context.getProjectBrowserRoot().getProjectFolder().addChild(rootNode);
		context.registerResourceWorkspace(workspace);
		nodeStatusManager.createRootResourceStatus(rootNode);
		// Instantiate the notification handler so that we have notifications
		// ready when diagrams are displayed. The constructor is sufficient.
		NotificationHandler.getInstance().setHook(this);
		// Query the gateway for latest notifications from all blocks
		appRequestHandler.triggerStatusNotifications();
	}
	
	public NodeStatusManager getNavTreeStatusManager() { return nodeStatusManager; }
	
	public DiagramWorkspace getWorkspace() { return workspace; }

	// Before the massive save, make sure that all dirty nodes have been
	// serialized into project resources.
	@Override
	public void notifyProjectSaveStart(SaveContext save) {
		log.infof("%s: NotifyProjectSaveStart",CLSS);
		
		// check if problems with save, just notify for now.  Can do save.abort() if it's serious
		String msg = rootNode.scanForNameConflicts(rootNode);
		if (msg != null && msg.length() > 1) {
			log.infof("%s: Workspace error, please correct before saving:  %s",CLSS, msg);
			ErrorUtil.showError(msg, "Save Workspace Error, save aborted");
			save.abort(new Throwable(msg));
		} 
		else {
			ResourceSaveManager saver = new ResourceSaveManager(getWorkspace(),rootNode);
			saver.saveSynchronously();
		}
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
	//
	//	TODO EREIAM JH - Why does this always return false?????????????
	//
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
    					if( menu.getName().equalsIgnoreCase(WellKnownMenuConstants.TOOLS_MENU_NAME)) {
    						int nitems = menu.getItemCount();
    						int jndex = 0;
    						log.tracef("%s: found VIEW menu",CLSS);
    						while(jndex<nitems ) {
    							JMenuItem item = menu.getItem(jndex);
    							if( item!=null ) {
    								String name = item.getText();
        							log.tracef("%s: found %s",CLSS,name);
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
	
	
	public GeneralPurposeTreeNode findApplicationForDiagram(ProcessDiagramView diagram) {
		NodeStatusManager mgr = getNavTreeStatusManager();
		GeneralPurposeTreeNode rtNode = (GeneralPurposeTreeNode)mgr.findNode(-1);
		AbstractResourceNavTreeNode ret = applicationForDiagram(null, rtNode, diagram);
		return (ret == null?null:(GeneralPurposeTreeNode)ret);

	}

	public AbstractResourceNavTreeNode applicationForDiagram(AbstractResourceNavTreeNode app, AbstractResourceNavTreeNode node, ProcessDiagramView diagram) {
		AbstractResourceNavTreeNode ret = null;

		if (node.getProjectResource() != null && node.getProjectResource().getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {
			if (diagram.getName().equals(node.getName())) {
				ProjectResource bob = node.getProjectResource();
				ret = app;
			}
		}
		else {
			if (node.getProjectResource() != null && node.getProjectResource().getResourceType().equals(BLTProperties.APPLICATION_RESOURCE_TYPE)) {
				app = node;
			}
			Enumeration<AbstractResourceNavTreeNode> enumer = node.children();
			while(enumer.hasMoreElements() && ret == null) {
				AbstractResourceNavTreeNode theNode = enumer.nextElement();
				ret = applicationForDiagram(app, theNode, diagram);
			}
		}
		return ret;
	}

	public String scanForDiagnosisNameConflicts(ProcessDiagramView diagram, String name) {
		String ret = "";
		// find application for the diagram.  This isn't particularly efficient as it traverses the whole tree
		GeneralPurposeTreeNode node = findApplicationForDiagram(diagram);
		
		ret = parseChildForDiagnosisName(node, name);
		
		return ret;
	}

	public String parseChildForDiagnosisName(AbstractResourceNavTreeNode theNode, String name) {
		String ret = "";
		if (theNode.getProjectResource().getResourceType().equals(BLTProperties.DIAGRAM_RESOURCE_TYPE)) {

			ProjectResource res = context.getProject().getResource(theNode.getProjectResource().getResourceId());	
			String json = new String(res.getData());
			SerializableDiagram sd = null;
			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			mapper.configure(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL,true);
			try {
				sd = mapper.readValue(json,SerializableDiagram.class);
			} 
			catch (JsonParseException jpe) {
//				logger.warnf("%s: open parse exception (%s)",CLSS,jpe.getLocalizedMessage());
			} 
			catch (JsonMappingException jme) {
//				logger.warnf("%s: open mapping exception (%s)",CLSS,jme.getLocalizedMessage());
			} 
			catch (IOException ioe) {
//				logger.warnf("%s: open io exception (%s)",CLSS,ioe.getLocalizedMessage());
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
	
	
	
}
