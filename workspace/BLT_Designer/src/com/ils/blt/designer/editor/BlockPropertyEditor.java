package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import net.miginfocom.swing.MigLayout;

import com.ils.block.common.BindingType;
import com.ils.block.common.BlockConstants;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.DistributionType;
import com.ils.block.common.LimitType;
import com.ils.block.common.PropertyType;
import com.ils.block.common.TransmissionScope;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ApplicationRequestManager;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.sqltags.tree.TagTreeNode;
import com.inductiveautomation.ignition.client.util.gui.SlidingPane;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.sqltags.dialog.TagBrowserPanel;


/**
 * Display a panel to edit block properties.    
 */

public class BlockPropertyEditor extends SlidingPane {
	private final static String TAG = "BlockPropertyEditor";
	private static final long serialVersionUID = 8971626415423709616L;
	private ProcessBlockView block;
	private final DesignerContext context;
	private final LoggerEx log;
	private final long projectId;
	private final long resourceId;
	private static final List<String> coreAttributeNames;
	
	private JPanel mainPanel = new JPanel();   // holds the property editor
	private JPanel tagBrowserPanel;
	private JTextField tagBindingTextField;  // text field corresponding to last tag combo selection
	private TreeSelectionModel tagTreeSelectionModel;
	
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
		add(mainPanel);
		add(createTagBrowserPanel());
		setSelectedPane(0);
		mainPanel.setLayout(new MigLayout("flowy,ins 2"));
		
		JPanel panel = new CorePropertyPanel(block);
		mainPanel.add(panel,"grow,push");
		
		// The Gateway knows the saved state of a block and its attributes. If the block has never been
		// initialized (edited), then get defaults from the Gateway, else retain it current.
		// Always refresh the block attributes from the Gateway before display.
		log.debugf("%s: init - editing %s (%s)",TAG,block.getId().toString(),block.getClassName());
		Collection<BlockProperty> propertyList = block.getProperties();
		if( propertyList==null || propertyList.isEmpty()) {
			propertyList = new ArrayList<BlockProperty>();
			ApplicationRequestManager handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
			BlockProperty[] properties = handler.getBlockProperties(block.getClassName(),projectId,resourceId,block.getId());
			for(BlockProperty property:properties) {
				propertyList.add(property);
			}
			log.debugf("%s: init - initialize property list for %s (%d properties)",TAG,block.getId().toString(),propertyList.size());
			block.setProperties(propertyList);
		}
		else {
			// Existing block: Update the transient values from tag subscriptions.
			ApplicationRequestManager handler = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getPropertiesRequestHandler();
			BlockProperty[] properties = handler.getBlockProperties(block.getClassName(),projectId,resourceId,block.getId());
			for(BlockProperty property:properties) {
				if( property.getBindingType().equals(BindingType.TAG) ) {
					// Search the property list for the actual property
					Iterator<BlockProperty> walker = propertyList.iterator();
					while(walker.hasNext()) {
						BlockProperty bp = walker.next();
						if( bp.getName().equalsIgnoreCase(property.getName())) {
							bp.setValue(property.getValue());
							break;
						}
					}
				}
			}
		}
		
		// Now fill the editor 
		for(BlockProperty property:propertyList) {
			if( property.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_LIMIT_TYPE)) {
				panel = new LimitTypePanel(property);
			}
			else if( property.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SCOPE) ||
					 property.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_DISTRIBUTION) ) {
				panel = new ComboOnlyPanel(property);
			}
			else {
				panel = new PropertyPanel(property);
			}
			
			mainPanel.add(panel,"grow,push");
		}
	}
	
	/** A utility method to drill down into the Swing component hierarchy and
	 *  get the component of the given class (or a subclass of it). 
	 *  Returns null if none found.
	 */
	private Component getComponent(Component c, Class<?> klass) {
		if(klass.isAssignableFrom(c.getClass())) {
			return c;
		}
		else if (c instanceof Container) {
			for(Component cc: ((Container)c).getComponents()) {
				Component child = getComponent(cc, klass);
				if(child != null) {
					return child;
				}
			}
		}
		else if(c instanceof JViewport) {
			Component child = getComponent(((JViewport)c).getView(), klass);
			if(child != null) {
				return child;
			}
		}
		return null;
	}
	
	private Component createTagBrowserPanel() {
		tagBrowserPanel = new JPanel();
		tagBrowserPanel.setLayout(new BorderLayout());
		TagBrowserPanel tagBrowser = context.getTagBrowser();
		
		// TODO: this is a total hack to get the tree's selection model.
		// I don't see any obvious methods on TagBrowserPanel to get the
		// selected tags. I've asked Carl what the right way is; until
		// I hear from him this will do.
		JTree tagTree = (JTree)getComponent(tagBrowser,JTree.class);
		tagTreeSelectionModel = tagTree.getSelectionModel();
		
		tagBrowserPanel.add(tagBrowser, BorderLayout.CENTER);
		JPanel buttonPanel = new JPanel();
		tagBrowserPanel.add(buttonPanel, BorderLayout.SOUTH);
		
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton);
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TreePath[] selectedPaths = tagTreeSelectionModel.getSelectionPaths();
				if(selectedPaths.length == 1) {
					TagTreeNode node = (TagTreeNode)(selectedPaths[0].getLastPathComponent());					
					tagBindingTextField.setText(node.getTagPath().toString());
					// invoke the action, as if a user had typed return:
					for(ActionListener listener: tagBindingTextField.getActionListeners()) {
						listener.actionPerformed(new ActionEvent("", 0, ""));
					}
					tagTreeSelectionModel.clearSelection();
					setSelectedPane(0);
				}
				else if(selectedPaths.length > 1) {
					JOptionPane.showMessageDialog(mainPanel, "More than one tag is selected--please select only one.");
				}
				else {
					JOptionPane.showMessageDialog(mainPanel, "No tag is selected.");					
				}
			}

		});

		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tagTreeSelectionModel.clearSelection();
				setSelectedPane(0);
			}			
		});

		return tagBrowserPanel;
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
		boolean canEdit = (prop.isEditable() && prop.getBindingType().equals(BindingType.NONE));
		field.setEditable(canEdit);
		field.setEnabled(canEdit);
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
	        	if(field.getText().length()>0) {
	        		double val = Double.parseDouble(field.getText());
		            prop.setMaximum(val);
	        	}
	        	else {
	        		prop.setMaximum(Double.MAX_VALUE);
	        	}
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
				if(field.getText().length()>0) {
	        		double val = Double.parseDouble(field.getText());
		            prop.setMinimum(val);
	        	}
	        	else {
	        		prop.setMinimum(Double.MIN_VALUE);
	        	}
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
	 * @param bindingTextField 
	 */
	private JComboBox<String> createBindingTypeCombo(final BlockProperty prop, final JTextField textField) {
		String[] entries = new String[BindingType.values().length];
		int index=0;
		for(BindingType type : BindingType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		// note: we select the item BEFORE defining the action listener to avoid invoking it
		// for initialization (as opposed to an actual user selection)
		box.setSelectedItem(prop.getBindingType().toString());
		box.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	        	BindingType bt = BindingType.valueOf(BindingType.class, box.getSelectedItem().toString());
	        	log.debugf("%s: set binding type %s",TAG,box.getSelectedItem().toString());
	            prop.setBindingType(bt);
	            if (bt == BindingType.TAG) {
	            	tagBindingTextField = textField;
	            	setSelectedPane(1);
	            }
	        }
		});
		return box;
	}
	/**
	 * Create a combo box for distribution type (i.e. statistical distribution)
	 */
	private JComboBox<String> createDistributionTypeCombo(final BlockProperty prop) {
		String[] entries = new String[DistributionType.values().length];
		int index=0;
		for(DistributionType type : DistributionType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		box.addActionListener(new ActionListener() {
	        public void actionPerformed(ActionEvent e){
	        	DistributionType type = DistributionType.valueOf(DistributionType.class, box.getSelectedItem().toString());
	        	log.debugf("%s: set distribution type %s",TAG,box.getSelectedItem().toString());
	            prop.setValue(type.toString());
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
	        }
		});
		box.setSelectedItem(prop.getValue().toString());
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
			JTextField bindingTextField = createBindingTextField(prop);
			add(bindingTextField,"");
			add(createBindingTypeCombo(prop,bindingTextField),"wrap");
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
			if(prop.getName().contains("Distribution")) {
				add(createDistributionTypeCombo(prop),"wrap");
			}
			else {
				add(createTransmissionScopeCombo(prop),"wrap");
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
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		
		public CorePropertyPanel(ProcessBlockView blk) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
			addSeparator(this,"Block");
			
			add(createLabel("Name"),"skip");
			add(createTextField(blk.getName()),"span,growx");
			add(createLabel("Class"),"skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
		}
	}
}


