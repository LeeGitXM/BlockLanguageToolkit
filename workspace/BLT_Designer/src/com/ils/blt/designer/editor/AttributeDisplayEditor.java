/**
 *   (c) 2014-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.util.ArrayList;
import java.util.List;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.AttributeDisplayView;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.persistence.ToolkitProperties;
import com.ils.common.tag.TagUtility;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockDesignableContainer;
import com.inductiveautomation.ignition.designer.model.DesignerContext;


/**
 * This is the controller for the frame that contains various sliding panels
 * used for editing various attribute display attributes.    
 */
public class AttributeDisplayEditor extends AbstractPropertyEditor   {
	private static final long serialVersionUID = 8971626415423709616L;

	private final DesignerContext context;
	private final DiagramWorkspace workspace;
	private final ProcessDiagramView diagram;
	private final AttributeDisplayView display;
	private final ApplicationRequestHandler requestHandler;
	private static final List<String> coreAttributeNames;
	
	private MainPanel          mainPanel;       		// display the properties for a block
	private ConfigurationPanel configPanel;     		// configure a single block property
	
	static {
		// These are the attributes handled in the CorePropertyPanel
		coreAttributeNames = new ArrayList<String>();
		coreAttributeNames.add("class");
	}
	
	/**
	 * @param view the designer version of the block to edit. We 
	 */
	public AttributeDisplayEditor(DesignerContext ctx,DiagramWorkspace wksp,AttributeDisplayView view) {
		this.context = ctx;
		this.requestHandler = new ApplicationRequestHandler();
		this.workspace = wksp;
		this.diagram = wksp.getActiveDiagram();
		this.display = view;
        //this.mainPanel = new MainPanel(context,display, wksp);
        //this.configPanel = new ConfigurationPanel(this);
        init();    
	}

	public ApplicationRequestHandler getRequestHandler() { return this.requestHandler; }
	
	/** 
	 * Create the various panels. We keep one of each type.
	 */
	private void init() {
		mainPanel.initialize();
		add(mainPanel);                       // HOME_PANEL
		add(configPanel);                     // CONFIGURATION_PANEL
		setSelectedPane(BlockEditConstants.HOME_PANEL);
	}
	public AttributeDisplayView getDisplay() { return this.display; }
	public ProcessDiagramView getDiagram() { return this.diagram; }
	

	public DesignerContext getContext() { return this.context; }
	/**
	 * Changing the name is non-structural. If the diagram is not
	 * dirty for structural reasons, then we go ahead and save the
	 * project resource.
	 */
	public void saveDiagram() {
		if( !diagram.isDirty()) {
			BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(diagram.getResourceId());
			if( tab!=null )  workspace.saveDiagramResource(tab);
		}
	}
	/**
	 * Save a diagram that is not the current.
	 */
	public void saveDiagram(long resid) {
		BlockDesignableContainer tab = (BlockDesignableContainer)workspace.findDesignableContainer(resid);
		if( tab!=null )  workspace.saveDiagramResource(tab);
	}
	
	/**
	 * One of the edit panels has modified a block property. Update the
	 * running diagram directly. Do not mark the diagram as "dirty" since
	 * we've only changed a block property. Save the project resource.
	 */
	public void saveDiagramClean() {
		saveDiagram();
		diagram.setDirty(false);	
	}
	/**
	 * Modify a tag path to account for global production/isolation providers
	 * as well as the current state of the diagram.
	 * @param path
	 * @return the modified path.
	 */
	public String modifyPathForProvider(String path) {
		String tagPath = path;
		if( path!=null && !path.isEmpty() ) {
			if( diagram.getState().equals(DiagramState.ISOLATED)) {
				String provider = requestHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER);
				tagPath = TagUtility.replaceProviderInPath(provider,path);
			}
			else {
				String provider = requestHandler.getToolkitProperty(ToolkitProperties.TOOLKIT_PROPERTY_PROVIDER);
				tagPath = TagUtility.replaceProviderInPath(provider,path);
			}
		}
		return tagPath;
	}
	
	/**
	 * Un-subscribe to notifications to allow cleanup. These are all 
	 * done on the main panel.
	 */
	public void shutdown() {
		mainPanel.shutdown();
	}
	
	// Update the displayed value in the main panel
	public void updatePanelValue(String propertyName,Object val) {
		/*
		if (block.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE)) {
			//sourceMainPanel.updatePanelValue(propertyName,val);
		} 
		else {
			mainPanel.updatePanelValue(propertyName,val);
		}
		*/
	}
	
	public void updateCorePanel(int panelIndex,ProcessBlockView blk) {
		switch(panelIndex) {
		case BlockEditConstants.HOME_PANEL: 
			MainPanel mp = this.mainPanel;
			//if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE)) mp = sourceMainPanel;
			mp.updateCorePanel(blk);
			break;
		case BlockEditConstants.CONFIGURATION_PANEL:
		default:
			break;
		}
	}
	
	public void updatePanelForProperty(int panelIndex,BlockProperty prop) {
		switch(panelIndex) {
		case BlockEditConstants.CONFIGURATION_PANEL:
			configPanel.updateForProperty(prop);
			break;
		default:
			break;
		}
	}
}


