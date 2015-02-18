package com.ils.blt.designer.applicationConfiguration;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.border.BevelBorder;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;





import net.miginfocom.swing.MigLayout;


//import com.ils.sfc.client.step.AbstractIlsStepUI;
import com.inductiveautomation.ignition.common.config.BasicPropertySet;
import com.inductiveautomation.ignition.common.config.PropertyValue;

public class HomePane extends JPanel implements ApplicationConfigurationController.EditorPane {
	private ApplicationConfigurationController controller;
	private Application application;
	private static final long serialVersionUID = 2882399376824334427L;

	final JTextField nameField = new JTextField();
	//	private static Icon nextIcon = new ImageIcon(AbstractIlsStepUI.class.getResource("/images/add.png"));
//	final JButton nextButton = new JButton(nextIcon);
	final JButton nextButton = new JButton("Next");
	final JButton cancelButton = new JButton("Cancel");
	final JButton okButton = new JButton("OK");
	
/*	
	private PropertyEditor editor = new PropertyEditor();
	private ButtonPanel buttonPanel = new ButtonPanel(true, true, true, true, false,  RecipeEditorController.background);

	private Data recipeData;
*/	
	public HomePane(ApplicationConfigurationController controller, Application app) {
		super(new MigLayout(
				"",										// Layout Constraints
				"[50][50][50][50]",						// Column Constraints
				""										// Row Constraints
				));
		this.controller = controller;
		this.application=app;

		add(new JLabel("Application Configuration"), "cell 1 0 2 1");
		
		add(new JLabel("Name:"), "cell 0 3");
		nameField.setText(application.getName());
		add(nameField, "wrap");
		
		add(new JLabel("Description:"), "cell 0 4");
		add(new JTextField(application.getDescription()), "wrap");
		
		add(new JLabel("Console:"), "cell 0 5");
		
		add(new JLabel("Queue:"), "cell 0 6");
		
		add(new JLabel("Unit:"), "cell 0 7");
		
		add(cancelButton, "cell 1 9");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doCancel();}
		});

		add(okButton, "cell 2 9");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doOk();}
		});
		
		add(nextButton, "cell 3 10");
		nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doNext();}			
		});
		
		
/*		
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 10,gapy 3,gapx 5,fillx";
		final String rowConstraints = "para[][][][][][][][][]";
		
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		panel.add(createLabel("Application.Name"),"");
		nameField = createTextField("Application.Name.Desc",application.getName());
		panel.add(nameField,"span,wrap");
		
		panel.add(createLabel("Application.UUID"),"gaptop 2,aligny top");
		JTextField uuidField = createTextField("Application.UUID.Desc",application.getId().toString());
		uuidField.setEditable(false);
		panel.add(uuidField,"span,wrap");
		
		panel.add(createLabel("Application.Description"),"gaptop 2,aligny top");
		String description = (String)properties.get(PROPERTY_DESCRIPTION);
		if( description==null) description="";
		descriptionArea = createTextArea("Application.Description.Desc",description);
		panel.add(descriptionArea,"gaptop 2,aligny top,span,wrap");
		
		panel.add(createLabel("Application.Console"),"");
		String console = (String)properties.get(PROPERTY_CONSOLE);
		if( console==null) console="";
		consoleField = createTextField("Application.Console.Desc",console);
		panel.add(consoleField,"span,wrap");
		
		panel.add(createLabel("Application.Queue"),"");
		String queue = (String)properties.get(PROPERTY_MESSAGE_QUEUE);
		if( queue==null) queue="";
		queueField = createTextField("Application.Queue.Desc",queue);
		panel.add(queueField,"span,wrap");
		
		panel.add(createLabel("Application.Unit"),"");
		String unit = (String)properties.get(PROPERTY_UNIT);
		if( unit==null) unit="";
		unitField = createTextField("Application.Unit.Desc",unit);
		panel.add(unitField,"span,wrap");
		
		panel.add(createLabel("Application.Menu"),"");
		String include = (String)properties.get(PROPERTY_INCLUDE_IN_MENU);
		boolean shouldInclude = false;
		if( include!=null && include.equalsIgnoreCase("true")) shouldInclude = true;
		menuCheckBox = createCheckBox("Application.Menu.Desc",shouldInclude);
		panel.add(menuCheckBox,"");
		
		panel.add(createLabel("Application.Ramp"),"gapleft 10");
		String method = (String)properties.get(PROPERTY_RAMP_METHOD);
		if( method==null) method="";
		methodBox = createRampMethodCombo("Application.Ramp.Desc",method);
		panel.add(methodBox,"wrap");
		
		panel.add(createLabel("Application.Priority"),"");
		String priority = (String)properties.get(PROPERTY_HIGHEST_PRIORITY);
		if( priority==null) priority="";
		// Highest priority is read-only
		JTextField priorityField = createTextField("Application.Priority.Desc",priority);
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		priorityField.setEnabled(false);
		panel.add(priorityField,"");
		
		panel.add(createLabel("Application.State"),"gapleft 10");
		stateBox = createActiveStateCombo("Application.State",application.getState());
		panel.add(stateBox,"wrap 20");
*/
	}

	protected void doOk() {
		System.out.println("In doOK");

		// Set attributes from fields
		application.setName(nameField.getText());
		controller.doOK();
	}

	protected void doCancel() {
		System.out.println("In doOK");
		controller.doCancel();
	}

	protected void doNext() {
		controller.getSlidingPane().setSelectedPane(1);
	}

	@Override
	public void activate() {
		controller.slideTo(ApplicationConfigurationController.HOME);
	}
	
/*
	private void doAdd() {
		controller.getFieldCreator().setRecipeData((Structure)recipeData);
		controller.getFieldCreator().activate();
	}

	private void doEdit() {
		PropertyValue<?> selectedPropertyValue = getPropertyEditor().getSelectedPropertyValue();
		if(selectedPropertyValue == null) return;
		if(selectedPropertyValue.getProperty().equals(IlsProperty.TAG_PATH)) {
			controller.getTagBrowser().activate();
		}
		else if(selectedPropertyValue.getProperty().getType() == String.class) {
			controller.getTextEditor().setText((String)selectedPropertyValue.getValue());
			controller.getTextEditor().activate();
		}
	}

	private void doRemove() {
		PropertyValue<?> selectedPropertyValue = getPropertyEditor().getSelectedPropertyValue();
		if(selectedPropertyValue == null) return;
		Structure structureData = (Structure) recipeData;
		structureData.removeDynamicProperty(selectedPropertyValue.getProperty());
		getPropertyEditor().setPropertyValues(recipeData.getProperties(), false);
	}
*/	
	private void doOK() {
		System.out.println("In doOK");
/*
		recipeData.setProperties(editor.getPropertyValues());
		controller.getBrowser().activate();
*/
	}
/*
	public PropertyEditor getPropertyEditor() {
		return editor;
	}

	public void setRecipeData(Data recipeData) {
		this.recipeData = recipeData;
		boolean isStructure = recipeData instanceof Structure;
		buttonPanel.getAddButton().setVisible(isStructure);
		buttonPanel.getRemoveButton().setVisible(isStructure);
		validate();
		getPropertyEditor().setPropertyValues(recipeData.getProperties(), false);
	}
*/
}