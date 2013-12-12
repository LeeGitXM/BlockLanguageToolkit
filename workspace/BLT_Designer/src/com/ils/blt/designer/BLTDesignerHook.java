/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;

import javax.swing.JFrame;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.component.DiagramAnalyzerComponent;
import com.ils.blt.designer.component.DiagramPreviewComponent;
import com.ils.blt.designer.graphics.PaletteBlocks;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.jgx.editor.JgxPalette;
import com.inductiveautomation.factorypmi.designer.palette.model.DefaultPaletteItemGroup;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.WorkspaceManager;
import com.inductiveautomation.ignition.designer.designable.AbstractDesignableWorkspace;
import com.inductiveautomation.ignition.designer.gui.IconUtil;
import com.inductiveautomation.ignition.designer.model.AbstractDesignerModuleHook;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspace;
import com.inductiveautomation.ignition.designer.model.SaveContext;
import com.inductiveautomation.vision.api.designer.VisionDesignerInterface;
import com.inductiveautomation.vision.api.designer.palette.JavaBeanPaletteItem;
import com.inductiveautomation.vision.api.designer.palette.Palette;
import com.inductiveautomation.vision.api.designer.palette.PaletteItemGroup;
import com.jidesoft.docking.DockingManager;
import com.jidesoft.docking.Workspace;
import com.mxgraph.swing.util.mxSwingConstants;
import com.mxgraph.util.mxConstants;

public class BLTDesignerHook extends AbstractDesignerModuleHook  {
	private static final String TAG = "BLTDesignerHook";
	public static String HOOK_BUNDLE_NAME   = "designer";// Properties file is designer.properties

	private DiagramTreeNode rootNode;
	private DesignerContext context = null;
	private JFrame paletteFrame = null;
	private final LoggerEx log;
	private DiagramWorkspace workspace = null;
	
	static {
		BundleUtil.get().addBundle(BLTProperties.BUNDLE_PREFIX,BLTDesignerHook.class,HOOK_BUNDLE_NAME);
	}
	
	public BLTDesignerHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(BLTProperties.PROPERTIES_SCRIPT_PACKAGE, BlockPropertiesScriptFunctions.class);
	}
	
	@Override
	public void startup(DesignerContext ctx, LicenseState activationState) throws Exception {
		this.context = ctx;
		
		workspace = new DiagramWorkspace(context);
		rootNode = new DiagramTreeNode(context);
		context.getProjectBrowserRoot().addChild(rootNode);
		context.registerResourceWorkspace(workspace);
		
		// Place icons for our custom widgets on the Vision palette
		VisionDesignerInterface vdi = 
					(VisionDesignerInterface) context.getModule(VisionDesignerInterface.VISION_MODULE_ID);

		if (vdi != null) {
				final Palette palette = vdi.getPalette();

				// Populate the palette
				PaletteItemGroup group = palette.addGroup(BundleUtil.get().getString(".Palette.Name"));
				if( group instanceof DefaultPaletteItemGroup ) {
					// The icon is located in vis-designer/images/incors
					((DefaultPaletteItemGroup) group).setIcon(IconUtil.getIcon("add_child"));
				}
				else {
					log.infof("%s: Group not a DefaultPaletteItemGroup, is %s",TAG,group.getClass().getName());
				}
					
				group.addPaletteItem(new JavaBeanPaletteItem(DiagramAnalyzerComponent.class));
				group.addPaletteItem(new JavaBeanPaletteItem(DiagramPreviewComponent.class));
		}
		// Register the listener for notifications
		GatewayConnectionManager.getInstance().addPushNotificationListener(new NotificationListener());
	}
	
	public DiagramWorkspace getWorkspace() { return workspace; }

	@Override
	public void notifyProjectSaveStart(SaveContext save) {
		workspace.saveOpenDiagrams();
	}
 
	/**
	 * Display the Diagnostics Tools Menu
	 */
	public void displayPalette()  {
		log.info(String.format("%s.displayPalette ...", TAG));

		mxSwingConstants.SHADOW_COLOR = Color.LIGHT_GRAY;
		mxConstants.W3C_SHADOWCOLOR = "#D3D3D3";
		if( paletteFrame ==null ) {
			JgxPalette palette = new JgxPalette();
			PaletteBlocks.populatePalette(palette.getPalette());
			paletteFrame = new JFrame("Block and Connector Palette");
			paletteFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
			palette.setPreferredSize(new Dimension(500,150));
			paletteFrame.getContentPane().add(palette,BorderLayout.CENTER);
			paletteFrame.pack();
			paletteFrame.setAlwaysOnTop(true);
		}
		paletteFrame.setVisible(true);
	}
	
	/**
	 * Iterate over all the dockable frames. Close any that are not useful.
	 */
	public void resetPanelsForDiagnostics() {
		DockingManager dockManager = context.getDockingManager();
		for(String name:dockManager.getAllFrameNames()) {
			if( name.equalsIgnoreCase("OPC Browser")            ||
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
		Workspace wksp = dockManager.getWorkspace();
		// There is only 1 child - the workspace mananger
		Component[]children = wksp.getComponents();
		for( Component child:children ) {
			if( child instanceof com.inductiveautomation.ignition.designer.WorkspaceManager) {
				WorkspaceManager workspaceManager = (WorkspaceManager)child;
				int count = workspaceManager.getWorkspaceCount();
				for(int index=0;index<count;index++) {
					ResourceWorkspace rw = workspaceManager.getWorkspace(index);
					log.info(TAG+"ResourceWorkspace="+rw.getClass().getSimpleName());
					if( rw instanceof com.inductiveautomation.ignition.designer.designable.AbstractDesignableWorkspace) {
						AbstractDesignableWorkspace adw = (AbstractDesignableWorkspace)rw;
					}
				}
			}
		}
		
		//Introspector.tree((Component)wksp,3);   // Logs component tree to a depth of three
	}

	@Override
	public String getResourceCategoryKey(Project project,ProjectResource resource) {
		// TODO Auto-generated method stub - return bundle key corresponding to category in export
		return super.getResourceCategoryKey(project, resource);
	}
	
	@Override
	public void shutdown() {	
	}
}
