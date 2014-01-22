package com.ils.blt.designer.editor;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import com.ils.block.common.BlockProperty;
import com.ils.block.common.BindingType;
import com.ils.block.common.PropertyType;
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
		
		// The Gateway knows the current state of a block and all its attributes. 
		// Always refresh the block attributes from the Gateway before display.
		
		PropertiesRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
		BlockProperty[] properties = handler.getBlockProperties(projectId,resourceId,block.getId(),block.getClassName());
		
		Collection<BlockProperty> bpList = new ArrayList<BlockProperty>();
		for(BlockProperty property:properties) {
			bpList.add(property);
			panel = new PropertyPanel(property);
			add(panel,"grow,push");
		}
		block.setProperties(bpList);
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
	 * Create a text field for read-only values
	 */
	private JTextField createTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setEditable(false);
		return field;
	}
	
	/**
	 * Create a text box for the link field
	 */
	private JTextField createLinkTextField(final BlockProperty prop) {
		String val = prop.getBinding();
		if(val==null) val = "";
		final JTextField field = new JTextField(val);
		field.setEditable(prop.isEditible());
		field.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	            prop.setBinding(field.getText());
	        }
		});
		return field;
	}
	/**
	 * Create a text box for the value field
	 */
	private JTextField createValueTextField(final BlockProperty prop) {	
		Object val = prop.getValue();
		if(val==null) val = "";
		final JTextField field = new JTextField(val.toString());
		field.setEditable(prop.isEditible());
		field.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	            prop.setValue(field.getText());
	        }
		});
		return field;
	}
	/**
	 * Create a text box for the maximum field
	 */
	private JTextField createMaxTextField(final BlockProperty prop) {
		
		double val = prop.getMaximum();
		String text = "";
		if(val!=Double.NaN && val<Double.MAX_VALUE ) {
			text = String.valueOf(val);
		}
		final JTextField field = new JTextField(text);
		field.setEditable(true);
		field.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	        	double val = Double.parseDouble(field.getText());
	            prop.setMaximum(val);
	        }
		});
		return field;
	}

	/**
	 * Create a text box for the maximum field
	 */
	private JTextField createMinTextField(final BlockProperty prop) {

		double val = prop.getMinimum();
		String text = "";
		if(val!=Double.NaN && val>Double.MIN_VALUE ) {
			text = String.valueOf(val);
		}
		final JTextField field = new JTextField(text);
		field.setEditable(true);
		field.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e){
				double val = Double.parseDouble(field.getText());
				prop.setMinimum(val);
			}
		});
		return field;
	}
	
	/**
	 * Create a combo box for data types
	 */
	private JComboBox createPropertyTypeCombo(final BlockProperty prop) {
		String[] entries = new String[PropertyType.values().length];
		int index=0;
		for(PropertyType type : PropertyType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox box = new JComboBox(entries);
		box.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	            prop.setValue(box.getSelectedItem().toString());
	        }
		});
		box.setSelectedItem(prop.getType());
		box.setEditable(false);
		box.setEnabled(false);
		return box;
	}
	/**
	 * Create a combo box for link types
	 */
	private JComboBox createLinkTypeCombo(final BlockProperty prop) {
		String[] entries = new String[BindingType.values().length];
		int index=0;
		for(BindingType type : BindingType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox box = new JComboBox(entries);
		box.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	            prop.setValue(box.getSelectedItem().toString());
	        }
		});
		box.setSelectedItem(prop.getBinding());
		return box;
	}
	
	
	/**
	 * A property panel is an editor for a single property.
	 */
	@SuppressWarnings("serial")
	private class PropertyPanel extends JPanel {
		private static final String columnConstraints = "[para]0[][100lp,fill][60lp][95lp,fill]";
		private static final String layoutConstraints = "ins 10";
		private static final String rowConstraints = "";
		public PropertyPanel(BlockProperty prop) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			addSeparator(this,prop.getName());
			
			add(createLabel("Value"),"skip");
			add(createValueTextField(prop),"");
			add(createPropertyTypeCombo(prop),"wrap");
			add(createLabel("Binding"),"skip");
			add(createLinkTextField(prop),"");
			add(createLinkTypeCombo(prop),"wrap");
			// For int or double, add min and max
			PropertyType type = prop.getType();
			if( type==PropertyType.DOUBLE || type==PropertyType.INTEGER) {
				add(createLabel("Min-Max"),"skip");
				add(createMinTextField(prop),"");
				add(createMaxTextField(prop),"wrap");
			}
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
			add(createTextField(blk.getClassName()),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
		}
		
	}
}


