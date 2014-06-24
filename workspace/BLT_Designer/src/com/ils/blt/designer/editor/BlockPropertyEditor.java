/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComboBox;

import com.ils.blt.common.ApplicationRequestManager;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.LimitType;
import com.ils.blt.common.block.TransmissionScope;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.util.gui.SlidingPane;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;


/**
 * Display a panel to edit block properties.    
 */

public class BlockPropertyEditor extends SlidingPane   {
	private final static String TAG = "BlockPropertyEditor";
	private static final long serialVersionUID = 8971626415423709616L;

	private ProcessBlockView block;
	private final DesignerContext context;
	private final LoggerEx log;
	private final long projectId;
	private final long resourceId;
	private static final List<String> coreAttributeNames;
	
	private final MainPanel          mainPanel;       // display the properties for a block
	private final ConfigurationPanel configPanel;     // configure a single block property
	private final NameEditPanel      nameEditPanel;   // configure a block's name
	private final TagBrowserPanel    tagPanel;        // configure tag for a bound value
	private final ValueEditPanel     valueEditPanel;  // configure a single block property
	
	
	
	static {
		// These are the attributes handled in the CorePropertyPanel
		coreAttributeNames = new ArrayList<String>();
		coreAttributeNames.add("class");
	}
	
	/**
	 * @param view the designer version of the block to edit. We 
	 */
	public BlockPropertyEditor(DesignerContext ctx,long res,ProcessBlockView view) {
		this.context = ctx;
		this.projectId = ctx.getProject().getId();
		this.resourceId = res;
		this.block = view;
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        this.mainPanel = new MainPanel(this,block);
        this.configPanel = new ConfigurationPanel(this);
        this.nameEditPanel = new NameEditPanel(this);
        this.tagPanel = new TagBrowserPanel(this,context);
        this.valueEditPanel = new ValueEditPanel(this);
        init();    
	}

	
	/** 
	 * Create the various panels. We keep one of each type.
	 */
	private void init() {
		add(mainPanel);                       // HOME_PANEL
		add(configPanel);                     // CONFIGURATION_PANEL
		add(nameEditPanel);                   // NAME_EDIT_PANEL
		add(tagPanel);                        // TAG_BROWSER_PANEL
		add(valueEditPanel);                  // VALUE_EDIT_PANEL
		setSelectedPane(BlockEditConstants.HOME_PANEL);   
	}
	
	public void updatePanelForBlock(int panelIndex,ProcessBlockView block) {
		switch(panelIndex) {
		case BlockEditConstants.NAME_EDIT_PANEL:
			nameEditPanel.updateForBlock(block);
			break;
		case BlockEditConstants.CONFIGURATION_PANEL:
		case BlockEditConstants.HOME_PANEL: 
		default:
			break;
		}
	}
	public void updatePanelForProperty(int panelIndex,BlockProperty prop) {
		switch(panelIndex) {
		case BlockEditConstants.CONFIGURATION_PANEL:
			configPanel.updateForProperty(prop);
			break;
		case BlockEditConstants.HOME_PANEL: 
			mainPanel.updatePanelForProperty(prop);
			break;
		case BlockEditConstants.TAG_BROWSER_PANEL:
			tagPanel.updateForProperty(prop);
			break;
		case BlockEditConstants.VALUE_EDIT_PANEL:
			valueEditPanel.updateForProperty(prop);
			break;
		case BlockEditConstants.NAME_EDIT_PANEL:
		default:
			break;
		}
	}
	
	/** 
	 * Update a block property, both in the local view and back on the Gateway. If the property is read-only or
	 * has a binding of ENGINE, the value will not change in the engine.
	 */
	private void updateBlockProperty(final BlockProperty prop) {
        // Propagate the change to the Gateway:
        ApplicationRequestManager handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
        handler.setBlockProperty(block.getClassName(), projectId, resourceId, block.getId().toString(), prop.getName(), prop);
	}

	// =================================== Custom Panel Classes ===================================
	
	
	
	
	
	
	
	// A panel that displays a tag browser
	
	

	

	

	
	// =================================== Unused ===================================

	/**
	 * Create a combo box for true/false 
	 */
	private JComboBox<String> createBooleanCombo(final BlockProperty prop) {
		String[] entries = new String[2];
		entries[0]=Boolean.TRUE.toString();
		entries[1]=Boolean.FALSE.toString();
		
		final JComboBox<String> box = new JComboBox<String>(entries);
		box.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	        	prop.setValue(box.getSelectedItem().toString());
	        	updateBlockProperty(prop);
	        }
		});
		box.setSelectedItem(prop.getValue().toString());
		return box;
	}
	/**
	 * Create a combo box for limit type
	 */
	private JComboBox<String> createLimitTypeCombo(final BlockProperty prop) {
		String[] entries = new String[LimitType.values().length];
		int index=0;
		for(LimitType scope : LimitType.values()) {
			entries[index]=scope.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		box.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	        	LimitType type = LimitType.valueOf(LimitType.class, box.getSelectedItem().toString());
	        	log.debugf("%s: set limit type %s",TAG,box.getSelectedItem().toString());
	        	prop.setValue(box.getSelectedItem().toString());
	        	updateBlockProperty(prop);
	        }
		});
		box.setSelectedItem(prop.getValue().toString());
		return box;
	}
	
	/**
	 * Create a combo box for transmission scope
	 */
	private JComboBox<String> createTransmissionScopeCombo(final BlockProperty prop) {
		String[] entries = new String[TransmissionScope.values().length];
		int index=0;
		for(TransmissionScope scope : TransmissionScope.values()) {
			entries[index]=scope.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		box.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	        	TransmissionScope scope = TransmissionScope.valueOf(TransmissionScope.class, box.getSelectedItem().toString());
	        	log.debugf("%s: set transmission scope %s",TAG,box.getSelectedItem().toString());
	        	prop.setValue(scope.toString());
	        	updateBlockProperty(prop);
	        }
		});
		box.setSelectedItem(prop.getValue().toString());
		return box;
	}
	/*
	// Special for a LimitType block property
	private class LimitTypePanel extends JPanel {
		private static final long serialVersionUID = 6501004559543409511L;
		private static final String columnConstraints = "[para]0[][100lp,fill]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";

		public LimitTypePanel(BlockProperty prop) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			addSeparator(this,prop.getName());

			add(createLabel("Type"),"skip");
			add(createLimitTypeCombo(prop),"wrap");
		}
	}
		
	// Special for a TruthValue block property
	private class TruthStatePanel extends JPanel {
		private static final long serialVersionUID = 6501004559543409511L;
		private static final String columnConstraints = "[para]0[][100lp,fill]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";

		public TruthStatePanel(BlockProperty prop) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			addSeparator(this,prop.getName());

			add(createLabel("Type"),"skip");
			add(createBooleanCombo(prop),"wrap");
		}
	}


	// Special for whenever there is just a combo box
	private class ComboOnlyPanel extends JPanel {
		private static final long serialVersionUID = 6501004559543409511L;
		private static final String columnConstraints = "[para]0[][100lp,fill]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		
		public ComboOnlyPanel(BlockProperty prop) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			addSeparator(this,prop.getName());
			
			add(createLabel(prop.getName()),"skip");
			if(prop.getName().endsWith("?")) {
				add(createBooleanCombo(prop),"wrap");
			}	
		}
	}
	*/
	
}


