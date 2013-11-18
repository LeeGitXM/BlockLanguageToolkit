/**
 *   (c) 2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JFrame;

import com.ils.blt.designer.graphics.PaletteBlocks;
import com.ils.blt.designer.navtree.DiagnosticsFolderNode;
import com.ils.blt.designer.workspace.DiagnosticsWorkspace;
import com.ils.diagnostics.common.DTProperties;
import com.ils.jgx.editor.JgxPalette;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayConnectionManager;
import com.inductiveautomation.ignition.common.expressions.ExpressionFunctionManager;
import com.inductiveautomation.ignition.common.licensing.LicenseState;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.script.ScriptManager;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.common.xmlserialization.deserialization.XMLDeserializer;
import com.inductiveautomation.ignition.common.xmlserialization.serialization.XMLSerializer;
import com.inductiveautomation.ignition.designer.WorkspaceManager;
import com.inductiveautomation.ignition.designer.designable.AbstractDesignableWorkspace;
import com.inductiveautomation.ignition.designer.model.AbstractDesignerModuleHook;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.model.DesignerModuleHook;
import com.inductiveautomation.ignition.designer.model.ResourceWorkspace;
import com.inductiveautomation.ignition.designer.model.SaveContext;
import com.inductiveautomation.ignition.designer.model.menu.JMenuMerge;
import com.inductiveautomation.ignition.designer.model.menu.MenuBarMerge;
import com.inductiveautomation.ignition.designer.model.menu.WellKnownMenuConstants;
import com.jidesoft.action.CommandBar;
import com.jidesoft.docking.DockableFrame;
import com.jidesoft.docking.DockingManager;
import com.jidesoft.docking.Workspace;
import com.mxgraph.swing.util.mxSwingConstants;
import com.mxgraph.util.mxConstants;

public class DTDesignerHook extends AbstractDesignerModuleHook implements DesignerModuleHook {
	private static final String TAG = "DTDesignerHook:";
	private DiagnosticsFolderNode rootNode;
	private DesignerContext context = null;
	private JFrame paletteFrame = null;
	private final LoggerEx log;
	
	public DTDesignerHook() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	
	
	@Override
	public MenuBarMerge getModuleMenu() {
		MenuBarMerge merge = new MenuBarMerge(DTProperties.MODULE_ID);  // s suggested in javadocs
		merge.addSeparator();

		Action panelResetAction = new AbstractAction("Reset Panels for Toolkit") {
			private static final long serialVersionUID = 5374557387733312464L;
			public void actionPerformed(ActionEvent ae) {
				resetPanelsForDiagnostics();
			}
		};

		JMenuMerge resetPanels = new JMenuMerge(WellKnownMenuConstants.VIEW_MENU_NAME);
		resetPanels.add(panelResetAction);
		merge.add(WellKnownMenuConstants.VIEW_MENU_LOCATION, resetPanels);
		
		Action paletteAction = new AbstractAction("Diagnostics Toolkit Palette") {
			private static final long serialVersionUID = 5374557387733312463L;
			public void actionPerformed(ActionEvent ae) {
				displayPalette();
			}
		};

		JMenuMerge palette = new JMenuMerge(WellKnownMenuConstants.VIEW_MENU_NAME);
		palette.add(paletteAction);
		merge.add(WellKnownMenuConstants.VIEW_MENU_LOCATION, palette);
		

		return merge;
	}
	
	@Override
	public void initializeScriptManager(ScriptManager mgr) {
		super.initializeScriptManager(mgr);
		mgr.addScriptModule(DTProperties.REPORTING_SCRIPT_PACKAGE, StatusReportingScriptFunctions.class);
		mgr.addScriptModule(DTProperties.PROPERTIES_SCRIPT_PACKAGE, PropertiesRequestScriptFunctions.class);
	}
	
	@Override
	public void startup(DesignerContext ctx, LicenseState activationState) throws Exception {
		this.context = ctx;
		rootNode = new DiagnosticsFolderNode(context);
		context.getProjectBrowserRoot().addChild(rootNode);
		context.registerResourceWorkspace(DiagnosticsWorkspace.getInstance());
		// Register the listener for notifications
		GatewayConnectionManager.getInstance().addPushNotificationListener(new GatewayDesignerDelegate());
	}

	@Override
	public void notifyProjectSaveStart(SaveContext save) {
		//workspace.saveResource();
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
				log.info(TAG+"Hiding frame="+name);
			}
			else {
				log.info(TAG+"Leaving frame="+name);
			}
		}
		log.info(TAG+"Workspace="+dockManager.getWorkspace());
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
	public void configureDeserializer(XMLDeserializer arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void configureFunctionFactory(ExpressionFunctionManager arg0) {
	}

	@Override
	public void configureSerializer(XMLSerializer arg0) {	
	}

	@Override
	public List<DockableFrame> getFrames() {
		return null;
	}

	@Override
	public List<CommandBar> getModuleToolbars() {
		return null;
	}

	@Override
	public String getResourceCategoryKey(Project arg0, ProjectResource arg1) {
		return null;
	}

	@Override
	public String getResourceDisplayName(Project arg0, ProjectResource arg1) {
		return null;
	}

	@Override
	public Icon getResourceIcon(Project arg0, ProjectResource arg1) {
		return null;
	}

	@Override
	public void notifyActivationStateChanged(LicenseState arg0) {
	}

	@Override
	public void notifyProjectSaveDone() {
	}

	@Override
	public void shutdown() {	
	}
}
