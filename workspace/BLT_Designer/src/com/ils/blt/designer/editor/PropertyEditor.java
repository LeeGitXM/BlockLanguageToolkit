package com.ils.blt.designer.editor;

import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import com.ils.block.common.BlockProperty;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.PropertiesRequestHandler;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;


/**
 * Display a dialog to edit cell attributes.
 *    BlockAttributeDialog bad = new BlockAttributeDialog("Attribute Editor");
 *    bad.pack();
 *    bad.setVisible(true);   // Terminates when dialog closed.
 *    result = bad.getModel();
 *    
 */

public class PropertyEditor extends JPanel {
	private final static String TAG = "PropertyEditor";
	private static final long serialVersionUID = 8971626415423709616L;
	private ProcessBlockView block;
	private final DesignerContext context;
	private final LoggerEx log;
	private final long projectId;
	private final long resourceId;
	private static final List<String> coreAttributeNames;
	
	// These are the attributes handled in the CorePropertyPanel
	static {
		coreAttributeNames = new ArrayList<String>();
		coreAttributeNames.add("class");
	}
	
	/**
	 * @param view the designer version of the block to edit. We 
	 */
	public PropertyEditor(DesignerContext ctx,long res,ProcessBlockView view) {
		this.context = ctx;
		this.projectId = ctx.getProject().getId();
		this.resourceId = res;
		this.block = view;
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        
        init();    
	}

	
	/** 
	 * Initialize the UI components. The "master" version of the block's
	 * properties resides in the gateway.
	 */
	private void init() {
		setLayout(new MigLayout("flowy,ins 2"));

		
		JPanel panel = new CorePropertyPanel(block);
		add(panel,"grow,push");
		
		// Get the block attributes from the gateway. If this is a newly
		// created block, the gateway will create it.
		PropertiesRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
		List<BlockProperty> properties = handler.getBlockProperties(projectId,resourceId,block.getId(),block.getClassName());
		if( properties!=null ) {
			for(BlockProperty property:properties) {
				panel = new PropertyPanel(property);
				if( !coreAttributeNames.contains(property.getName()) ) {
					add(panel,"grow,push");
				}
			}
		}
		 
    }
	
	/**
	 * Add a separator to a panel using Mig layout
	 */
	private void addSeparator(JPanel panel,String text) {
		JSeparator separator = new JSeparator();
        JLabel label = new JLabel(text);
        label.setFont(new Font("Tahoma", Font.PLAIN, 11));
        label.setForeground(Color.BLUE);
        panel.add(label, "split 2,span");
        panel.add(separator, "growx,wrap");
	}
	
	/**
	 * Create a new label
	 */
	private JLabel createLabel(String text) {
		return new JLabel(text);
	}
	
	/**
	 * Create a new label
	 */
	private JTextField createTextField(String text,boolean editable) {
		JTextField field = new JTextField(text);
		field.setEditable(editable);
		return field;
	}
	
	/**
	 * A property panel is an editor for a single property.
	 */
	@SuppressWarnings("serial")
	private class PropertyPanel extends JPanel {
		
		public PropertyPanel(BlockProperty prop) {
			setLayout(new MigLayout("wrap 3"));     // 3 cells across
			addSeparator(this,prop.getName());
		}
		
	}
	
	/**
	 * These properties are present in every block.
	 * class, label, state, statusText
	 */
	@SuppressWarnings("serial")
	private class CorePropertyPanel extends JPanel {
		private static final String columnConstraints = "[para]0[][100lp,fill][60lp][95lp,fill]";
		private static final String layoutConstraints = "ins 10";
		private static final String rowConstraints = "";
		
		public CorePropertyPanel(ProcessBlockView blk) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
			addSeparator(this,"Core");
			
			add(createLabel("Class"),"skip");
			add(createTextField(blk.getClassName(),false),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString(),false),"span,growx");
		}
		
	}
}


