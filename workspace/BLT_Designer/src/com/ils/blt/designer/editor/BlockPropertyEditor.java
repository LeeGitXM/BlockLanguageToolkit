package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import net.miginfocom.swing.MigLayout;

import com.ils.block.common.BindingType;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.LimitType;
import com.ils.block.common.PropertyType;
import com.ils.block.common.TransmissionScope;
import com.ils.blt.common.ApplicationRequestManager;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.client.sqltags.tree.TagRenderer;
import com.inductiveautomation.ignition.client.sqltags.tree.TagTreeNode;
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
	private static final Dimension BUTTON_SIZE = new Dimension(16,16);
	// These are the panels in the set of sliding screens
	private static int HOME_PANEL          = 0;
	private static int CONFIGURATION_PANEL = 1;
	private static int NAME_EDIT_PANEL     = 2;
	private static int TAG_BROWSER_PANEL   = 3;
	
	private ProcessBlockView block;
	private final DesignerContext context;
	private final LoggerEx log;
	private final long projectId;
	private final long resourceId;
	private static final List<String> coreAttributeNames;
	
	private final MainPanel          mainPanel;       // display the properties for a block
	private final ConfigurationPanel configPanel;     // configure a single block property
	private final NameEditPanel      nameEditPanel;   // configure a block's name
	private final TagBrowserPanel    tagPanel;        // configure a single block property
	
	
	
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
        this.mainPanel = new MainPanel(block);
        this.configPanel = new ConfigurationPanel();
        this.nameEditPanel = new NameEditPanel();
        this.tagPanel = new TagBrowserPanel();
        init();    
	}

	
	/** 
	 * Initialize the UI components. The "master" version of the block's
	 * properties resides in the gateway. 
	 */
	private void init() {
		add(mainPanel);                       // HOME_PANEL
		add(configPanel);                     // CONFIGURATION_PANEL
		add(nameEditPanel);                   // NAME_EDIT_PANEL
		add(tagPanel);                        // TAG_BROWSER_PANEL
		setSelectedPane(HOME_PANEL);
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
	            updateBlockProperty(prop);
	        }

		});
		field.addFocusListener( new FocusListener() {
			public void focusGained(FocusEvent e) {}

			public void focusLost(FocusEvent e) {
				prop.setValue(field.getText());
	            updateBlockProperty(prop);				
			}
		});
		return field;
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
	
	// A Configuration panel is designed to configuration of a single property.
	// Allow modification of the binding (subject to certain restrictions),
	// show the data type (read-only) and allow for attribute display.
	private class ConfigurationPanel extends JPanel {
		private static final long serialVersionUID = 1L;
		private BlockProperty property = null;
		private final JLabel headingLabel;
		private final JComboBox<String> bindingTypeCombo;
		private final JComboBox<String> propertyTypeCombo;
		private final JCheckBox annotationCheckBox;
		private final JTextField xfield;
		private final JTextField yfield;
		
		public ConfigurationPanel() {
			setLayout(new MigLayout("flowy,ins 2"));
			headingLabel = addHeading(this);
			//Create three panels - binding type, data type, display option.
			JPanel bindingPanel = new JPanel();
			bindingPanel.setLayout(new MigLayout("flowy,ins 2"));
			addSeparator(bindingPanel,"Binding");
			bindingTypeCombo = createBindingTypeCombo();
			bindingPanel.add(bindingTypeCombo,"skip,center,span");
			add(bindingPanel);

			JPanel typePanel = new JPanel();
			typePanel.setLayout(new MigLayout("flowy,ins 2"));
			addSeparator(bindingPanel,"Property Type");
			propertyTypeCombo = createPropertyTypeCombo();
			propertyTypeCombo.setEditable(false);
			typePanel.add(propertyTypeCombo,"center");
			add(typePanel);
			
			JPanel displayPanel = new JPanel();
			displayPanel.setLayout(new MigLayout("flowy,ins 2"));
			addSeparator(displayPanel,"Attribute Display");
			annotationCheckBox = new JCheckBox("Display attribute?");
			displayPanel.add(annotationCheckBox);
			displayPanel.add(createLabel("X offset"),"skip");
			xfield = createTextField("");
			displayPanel.add(xfield,"growx");
			displayPanel.add(createLabel("Y offset"),"skip");
			yfield = createTextField("");
			displayPanel.add(yfield,"growx");
			add(displayPanel);
			
			// The OK button copies data from the components and sets the property properties.
			// It then returns to the main tab
			JPanel buttonPanel = new JPanel();
			add(buttonPanel, BorderLayout.SOUTH);
			JButton okButton = new JButton("OK");
			buttonPanel.add(okButton);
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					if(property!=null) {
						property.setBindingType(BindingType.valueOf(bindingTypeCombo.getSelectedItem().toString()));
					}
					setSelectedPane(HOME_PANEL);
				}
			});
			JButton cancelButton = new JButton("Cancel");
			buttonPanel.add(cancelButton);
			cancelButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					setSelectedPane(HOME_PANEL);
				}			
			});
		}
		
		public void updateForProperty(BlockProperty prop) {
			this.property = prop;
			headingLabel.setText(prop.getName());
			bindingTypeCombo.setSelectedItem(prop.getBindingType().toString());
			propertyTypeCombo.setSelectedItem(prop.getType().toString());
			annotationCheckBox.setSelected(prop.isDisplayed());
			xfield.setText(String.valueOf(prop.getDisplayOffsetX()));
			yfield.setText(String.valueOf(prop.getDisplayOffsetX()));
		}
	}
	
	/**
	 * These properties are present in every block.
	 * class, label, state, statusText
	 */
	@SuppressWarnings("serial")
	private class CorePropertyPanel extends JPanel {
		private static final String columnConstraints = "[para]0[]0[]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "[para]0[]0[]";
		
		public CorePropertyPanel(ProcessBlockView blk) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
			addSeparator(this,"Core");
			add(createLabel("Name"),"skip");
			add(createTextField(blk.getName()),"growx,pushx");
			add(createNameEditButton(blk),"w :25:");
			add(createLabel("Class"),"newline,skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
		}
	}
	
	/**
	 * This is the main panel displayed in the edit frame.
	 * Note: block properties have been initialized in ProcessDiagramView.initBlockProperties()
	 */
	@SuppressWarnings("serial")
	private class MainPanel extends JPanel {
		public MainPanel(ProcessBlockView blk) {
			setLayout(new MigLayout("top,flowy,ins 2","",""));
			JPanel panel = new CorePropertyPanel(block);
			add(panel,"grow,push");
			 
			log.infof("%s.mainPanel: - editing %s (%s)",TAG,block.getId().toString(),block.getClassName());
			JPanel propertyPanel = null;
			// Now fill the editor. We use the same edit panel class for all properties
			for(BlockProperty property:block.getProperties()) {
				// We have gotten null from serialization problems ...
				if( property==null || property.getName()==null) continue;
				log.infof("%s.init: - creating editor for property %s",TAG,property.getName());
				propertyPanel = new PropertyPanel(property);
				add(propertyPanel,"gapbefore 0,gapafter 0,growx,push");
			}
		}
	}
	// A special Configuration panel is designed to configuration of a block name.
	// Instead of binding and data type, we just have an edit box. 
	// We do allow for attribute display.
	private class NameEditPanel extends JPanel {
			private static final long serialVersionUID = 1L;
			private static final String columnConstraints = "[para]0[]0[]";
			private static final String layoutConstraints = "flowy,ins 2";
			private static final String rowConstraints = "[para]0[]0[]";
			private ProcessBlockView block = null;
			private final JLabel headingLabel;
			private final JTextField nameField;
			private final JCheckBox annotationCheckBox;
			private final JTextField xfield;
			private final JTextField yfield;
			
			public NameEditPanel() {
				setLayout(new MigLayout("top,flowy,ins 2","",""));
				headingLabel = addHeading(this);
				//Create three panels - binding type, data type, display option.
				JPanel namePanel = new JPanel();
				setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
				addSeparator(namePanel,"Block Name");
				namePanel.add(createLabel("Name"),"skip");
				nameField = createTextField("");
				namePanel.add(nameField,"span,growx");
				add(namePanel);
				
				JPanel displayPanel = new JPanel();
				setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
				addSeparator(displayPanel,"Attribute Display");
				annotationCheckBox = new JCheckBox("Display attribute?");
				displayPanel.add(annotationCheckBox);
				displayPanel.add(createLabel("X offset"),"skip");
				xfield = createTextField("");
				displayPanel.add(xfield,"span,growx");
				displayPanel.add(createLabel("Y offset"),"skip");
				yfield = createTextField("");
				displayPanel.add(yfield,"span,growx");
				add(displayPanel);
				
				// The OK button copies data from the components and sets the property properties.
				// It then returns to the main tab
				JPanel buttonPanel = new JPanel();
				add(buttonPanel, BorderLayout.SOUTH);
				JButton okButton = new JButton("OK");
				buttonPanel.add(okButton);
				okButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						if( !nameField.getText().isEmpty()) block.setName(nameField.getText());
						try {
							block.setNameOffsetX(Integer.parseInt(xfield.getText()));
							block.setNameOffsetY(Integer.parseInt(yfield.getText()));
							block.setNameDisplayed(annotationCheckBox.isSelected());
						}
						catch(NumberFormatException nfe) {
							log.warnf("%s.nameEditPanel: Unable to convert x/y offset value to an integer (%s)",TAG,nfe.getLocalizedMessage());
						}
						setSelectedPane(HOME_PANEL);
					}
				});
				JButton cancelButton = new JButton("Cancel");
				buttonPanel.add(cancelButton);
				cancelButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						setSelectedPane(HOME_PANEL);
					}			
				});
			}
			
			public void updateForBlock(ProcessBlockView blk) {
				this.block = blk;
				headingLabel.setText(block.getName());
				annotationCheckBox.setSelected(blk.isNameDisplayed());
				xfield.setText(String.valueOf(blk.getNameOffsetX()));
				yfield.setText(String.valueOf(blk.getNameOffsetY()));
			}
		}
	/**
	 * A property panel is an editor for a single property. By default it contains:
	 *    First line --
	 *    	  title label
	 *    Next line --
	 *        valueBox     - text box with the current value
	 *        edit button  - go to editor screen to change value
	 *        binding btn  - go to separate screen to determine binding    
	 */
	@SuppressWarnings("serial")
	private class PropertyPanel extends JPanel {
		private static final String columnConstraints = "";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		
		public PropertyPanel(BlockProperty prop) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			if( prop.getName().matches(".*Interval.*")) {
				addSeparator(this,prop.getName()+" ~ secs");
			}
			else {
				addSeparator(this,prop.getName());
			}
			// Now the second line.
			add(createValueTextField(prop),"skip,growx,push");
			add(createEditButton(prop),"w :25:");
			add(createConfigurationButton(prop),"w :25:");
		}
	}
	
	// A panel that displays a tag browser
	private class TagBrowserPanel extends JPanel {
		private static final long serialVersionUID = 1L;
		private String selectedPath = "";
		private BlockProperty property = null;
		public TagBrowserPanel() {
			setLayout(new BorderLayout());
			JTree tagTree = new JTree();
			tagTree.setCellRenderer(new TagRenderer());
			tagTree.setModel(context.getTagBrowser().getSqlTagTreeModel());
			final TreeSelectionModel tagTreeSelectionModel = tagTree.getSelectionModel();
			tagTree.setBackground(getBackground());
			add(tagTree, BorderLayout.CENTER);
			JPanel buttonPanel = new JPanel();
			add(buttonPanel, BorderLayout.SOUTH);
			JButton okButton = new JButton("OK");
			buttonPanel.add(okButton);
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					TreePath[] selectedPaths = tagTreeSelectionModel.getSelectionPaths();
					if(selectedPaths.length == 1) {
						TagTreeNode node = (TagTreeNode)(selectedPaths[0].getLastPathComponent());					
						selectedPath = node.getTagPath().toString();
						//tagTreeSelectionModel.clearSelection();

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
		}
		public void updateForProperty(BlockProperty prop) {
			this.property = prop;

		}
	}
			
	// ========================== Component Creation Methods ========================
	/**
	 * Add a heading to an edit panel using Mig layout.
	 * @return the label so that it can be changed later
	 */
	private JLabel addHeading(JPanel panel) {
		JSeparator separator = new JSeparator();
		panel.add(separator, "growx,wrap");
        JLabel label = new JLabel();
        label.setFont(new Font("Tahoma", Font.BOLD, 14));
        label.setForeground(Color.BLUE);
        panel.add(label, "center,split 2,span");
        panel.add(separator, "growx,wrap");
        return label;
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
	 * Create a combo box for binding type
	 */
	private JComboBox<String> createBindingTypeCombo() {
		String[] entries = new String[BindingType.values().length];
		int index=0;
		for(BindingType type : BindingType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		return box;
	}
	
	

	/**
	 * Create a button that slides to a panel allowing choices of binding and
	 * other property options.
	 * The button has only an image.
	 * @param prop 
	 */
	private JButton createConfigurationButton(final BlockProperty prop) {
		JButton btn = null;
		final String ICON_PATH  = "Block/icons/editor/data_link.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn = new JButton(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BUTTON_SIZE);
			btn.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e){
					configPanel.updateForProperty(prop);
					setSelectedPane(CONFIGURATION_PANEL);
				}
			});
		}
		return btn;
	}
	/**
	 * Create a button that navigates to the proper editor.
	 */
	private JButton createEditButton(final BlockProperty prop) {
		JButton btn = null;
		final String ICON_PATH  = "Block/icons/editor/pencil.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn = new JButton(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BUTTON_SIZE);
			btn.addActionListener(new ActionListener() {
				// Determine the correct panel, depending on the property type
				public void actionPerformed(ActionEvent e){
					setSelectedPane(TAG_BROWSER_PANEL);
				}
			});
		}
		return btn;
	}
	/**
	 * Create a new label
	 */
	private JLabel createLabel(String text) {
		return new JLabel(text);
	}
	/**
	 * Create a button that navigates to the proper editor for
	 * a block's name.
	 */
	private JButton createNameEditButton(final ProcessBlockView blk) {
		JButton btn = null;
		final String ICON_PATH  = "Block/icons/editor/pencil.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn = new JButton(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BUTTON_SIZE);
			btn.addActionListener(new ActionListener() {
				// Determine the correct panel, depending on the property type
				public void actionPerformed(ActionEvent e){
					nameEditPanel.updateForBlock(blk);
					setSelectedPane(NAME_EDIT_PANEL);
				}
			});
		}
		return btn;
	}
	/**
	 * Create a combo box for property type
	 */
	private JComboBox<String> createPropertyTypeCombo() {
		String[] entries = new String[PropertyType.values().length];
		int index=0;
		for(PropertyType type : PropertyType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		return box;
	}
	/**
	 * Create a text field for read-only values
	 */
	private JTextField createTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setEditable(false);
		return field;
	}
	
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
	
	
}


