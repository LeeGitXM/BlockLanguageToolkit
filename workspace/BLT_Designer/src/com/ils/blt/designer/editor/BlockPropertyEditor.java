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

import com.ils.block.common.BindingType;
import com.ils.block.common.BlockConstants;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.LimitType;
import com.ils.block.common.PropertyType;
import com.ils.block.common.TransmissionScope;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.BlockRequestHandler;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;


/**
 * Display a panel to edit block properties.    
 */

public class BlockPropertyEditor extends JPanel {
	private final static String TAG = "BlockPropertyEditor";
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
	public BlockPropertyEditor(DesignerContext ctx,long res,ProcessBlockView view) {
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
		
		// The Gateway knows the saved state of a block and its attributes. If the block has never been
		// initialized (edited), then get defaults from the Gateway, else retain it current.
		// Always refresh the block attributes from the Gateway before display.
		log.debugf("%s: init - editing %s (%s)",TAG,block.getId().toString(),block.getClassName());
		Collection<BlockProperty> propertyList = block.getProperties();
		if( propertyList==null || propertyList.isEmpty()) {
			propertyList = new ArrayList<BlockProperty>();
			BlockRequestHandler handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
			BlockProperty[] properties = handler.getBlockProperties(projectId,resourceId,block.getId(),block.getClassName());
			for(BlockProperty property:properties) {
				propertyList.add(property);
			}
			log.debugf("%s: init - initialize property list for %s (%d properties)",TAG,block.getId().toString(),propertyList.size());
			block.setProperties(propertyList);
		}
		
		// Now fill the editor 
		for(BlockProperty property:propertyList) {
			if( property.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_LIMIT_TYPE)) {
				panel = new LimitTypePanel(property);
			}
			else if( property.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SCOPE)) {
				panel = new ScopePanel(property);
			}
			else {
				panel = new PropertyPanel(property);
			}
			
			add(panel,"grow,push");
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
	 * Create a text field for read-only values
	 */
	private JTextField createTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setEditable(false);
		return field;
	}
	
	/**
	 * Create a text box for the binding field. 
	 * NOTE: An ENTER terminates text entry.
	 */
	private JTextField createBindingTextField(final BlockProperty prop) {
		String val = prop.getBinding();
		if(val==null) val = "";
		final JTextField field = new JTextField(val);
		field.setEditable(prop.isEditable());
		field.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	            prop.setBinding(field.getText());
	            log.debugf("%s: set binding %s",TAG,field.getText());
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
		field.setEditable(prop.isEditable());
		field.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	        	log.debugf("%s: set value %s",TAG,field.getText());
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
	 * Create a text box for the minimum field
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
	            prop.setValue(type.toString());
	        }
		});
		box.setSelectedItem(prop.getValue().toString());
		return box;
	}
	
	/**
	 * Create a combo box for data types
	 */
	private JComboBox<String> createPropertyTypeCombo(final BlockProperty prop) {
		String[] entries = new String[PropertyType.values().length];
		int index=0;
		for(PropertyType type : PropertyType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		box.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	        	PropertyType pt = PropertyType.valueOf(PropertyType.class, box.getSelectedItem().toString());
	        	log.debugf("%s: set property type %s",TAG,box.getSelectedItem().toString());
	            prop.setType(pt);
	        }
		});
		box.setSelectedItem(prop.getType().toString());
		box.setEditable(false);
		box.setEnabled(false);
		return box;
	}
	/**
	 * Create a combo box for link types
	 */
	private JComboBox<String> createBindingTypeCombo(final BlockProperty prop) {
		String[] entries = new String[BindingType.values().length];
		int index=0;
		for(BindingType type : BindingType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		box.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	        	BindingType bt = BindingType.valueOf(BindingType.class, box.getSelectedItem().toString());
	        	log.debugf("%s: set binding type %s",TAG,box.getSelectedItem().toString());
	            prop.setBindingType(bt);
	        }
		});
		box.setSelectedItem(prop.getBindingType().toString());
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
	        }
		});
		box.setSelectedItem(prop.getType().toString());
		return box;
	}
	
	/**
	 * A property panel is an editor for a single property.
	 */
	@SuppressWarnings("serial")
	private class PropertyPanel extends JPanel {
		private static final String columnConstraints = "[para]0[][100lp,fill][60lp][95lp,fill]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		public PropertyPanel(BlockProperty prop) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			addSeparator(this,prop.getName());
			
			if( prop.getName().matches("Interval"))
				add(createLabel("Time ~msecs"),"skip");
			else
				add(createLabel("Value"),"skip");
			add(createValueTextField(prop),"");
			add(createPropertyTypeCombo(prop),"wrap");
			add(createLabel("Binding"),"skip");
			add(createBindingTextField(prop),"");
			add(createBindingTypeCombo(prop),"wrap");
			// For int or double, add min and max
			PropertyType type = prop.getType();
			if( type==PropertyType.DOUBLE || type==PropertyType.INTEGER) {
				add(createLabel("Min-Max"),"skip");
				add(createMinTextField(prop),"");
				add(createMaxTextField(prop),"growx,wrap");
			}
		}
	}
	
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

	// Special for a transmit block
	private class ScopePanel extends JPanel {
		private static final long serialVersionUID = 6501004559543409511L;
		private static final String columnConstraints = "[para]0[][100lp,fill]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		
		public ScopePanel(BlockProperty prop) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			addSeparator(this,prop.getName());
			
			add(createLabel("Scope"),"skip");
			add(createTransmissionScopeCombo(prop),"wrap");
		}
	}
	
	/**
	 * These properties are present in every block.
	 * class, label, state, statusText
	 */
	@SuppressWarnings("serial")
	private class CorePropertyPanel extends JPanel {
		private static final String columnConstraints = "[para]0[][100lp,fill][60lp][95lp,fill]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		
		public CorePropertyPanel(ProcessBlockView blk) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
			addSeparator(this,"Block");
			
			add(createLabel("Label"),"skip");
			add(createTextField(blk.getLabel()),"span,growx");
			add(createLabel("Class"),"skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
		}
	}
}


