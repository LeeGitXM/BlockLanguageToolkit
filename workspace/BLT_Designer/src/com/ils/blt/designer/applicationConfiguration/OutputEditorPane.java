package com.ils.blt.designer.applicationConfiguration;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.Format;
import java.text.NumberFormat;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.border.BevelBorder;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;

import net.miginfocom.swing.MigLayout;

import com.inductiveautomation.ignition.common.config.BasicPropertySet;
import com.inductiveautomation.ignition.common.config.PropertyValue;


public class OutputEditorPane extends JPanel implements ApplicationConfigurationController.EditorPane {
	private ApplicationConfigurationController controller;
	private static final Insets insets = new Insets(0,0,0,0);
	final JTextField nameField = new JTextField();
	final JTextField tagField = new JTextField();
	final JFormattedTextField mostNegativeIncrementField;
	final JFormattedTextField mostPositiveIncrementField;
//	final JFormattedTextField minimumIncrementField;
//	final JFormattedTextField highLimitField;
//	final JFormattedTextField lowLimitField;
	final JButton cancelButton = new JButton("Cancel");
	final JButton okButton = new JButton("OK");
/*	
	private PropertyEditor editor = new PropertyEditor();
	private ButtonPanel buttonPanel = new ButtonPanel(true, true, true, true, false,  RecipeEditorController.background);

	private Data recipeData;
*/	
	public OutputEditorPane(ApplicationConfigurationController controller) {
		super(new BorderLayout(20, 30));
		this.controller = controller;
		Format floatFormat = NumberFormat.getInstance();
		System.out.println("In Output Editor pane constructor");
		
//		JLabel label = new JLabel("Quant Output Editor");
//		label.setHorizontalAlignment(JLabel.CENTER);
//		add(label, BorderLayout.NORTH);
		
		// The labels & Fields go in center in a Box layout 
//		JPanel mainPanel = new JPanel(new GridBagLayout());
		JPanel mainPanel = new JPanel(new MigLayout("", "[right]"));
		
		
		mainPanel.add(new JLabel("Quant Output Editor"), "split, span, gaptop 10"); 
		mainPanel.add(new JSeparator(), "growx, wrap, gaptop 10");
		
		mainPanel.add(new JLabel("Name:"), "gap 10");
		mainPanel.add(nameField, "span, growx");

		mainPanel.add(new JLabel("Tag:"), "gap 10");
		mainPanel.add(tagField, "span, growx");
		
		mainPanel.add(new JLabel("Most Negative:"), "gap 10");
		mostNegativeIncrementField = new JFormattedTextField(floatFormat);
		mainPanel.add(mostNegativeIncrementField, "span, growx");

		mainPanel.add(new JLabel("Most Positive:"), "gap 10");
		mostPositiveIncrementField = new JFormattedTextField(floatFormat);
		mainPanel.add(mostPositiveIncrementField, "span, growx");
		
//		mainPanel.add(new JLabel("Minimum Incr:"), new GridBagConstraints(0,4,1,1,1.0,1.0,GridBagConstraints.CENTER, GridBagConstraints.BOTH, insets, 0, 0));
//		mainPanel.add(new JLabel("High Limit:"), new GridBagConstraints(0,5,1,1,1.0,1.0,GridBagConstraints.CENTER, GridBagConstraints.BOTH, insets, 0, 0));
//		mainPanel.add(new JLabel("Low Limit:"), new GridBagConstraints(0,6,1,1,1.0,1.0,GridBagConstraints.CENTER, GridBagConstraints.BOTH, insets, 0, 0));
//		mainPanel.add(new JLabel("Feedback Method:"), new GridBagConstraints(0,7,1,1,1.0,1.0,GridBagConstraints.CENTER, GridBagConstraints.BOTH, insets, 0, 0));
//		mainPanel.add(new JLabel("Incremental Output:"), new GridBagConstraints(0,8,1,1,1.0,1.0,GridBagConstraints.CENTER, GridBagConstraints.BOTH, insets, 0, 0));
		
/*		
		minimumIncrementField = new JFormattedTextField(floatFormat);
		mainPanel.add(minimumIncrementField, new GridBagConstraints(1,4,2,1,1.0,1.0,GridBagConstraints.CENTER, GridBagConstraints.BOTH, insets, 0, 0));
		highLimitField = new JFormattedTextField(floatFormat);
		mainPanel.add(highLimitField, new GridBagConstraints(1,5,2,1,1.0,1.0,GridBagConstraints.CENTER, GridBagConstraints.BOTH, insets, 0, 0));
		lowLimitField = new JFormattedTextField(floatFormat);
		mainPanel.add(lowLimitField, new GridBagConstraints(1,6,2,1,1.0,1.0,GridBagConstraints.CENTER, GridBagConstraints.BOTH, insets, 0, 0));
*/		
		//		nameField.setText(application.getName());
		
		// Now the buttons that go at the bottom
		JPanel bottomPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		add(bottomPanel, BorderLayout.SOUTH);
		bottomPanel.add(cancelButton);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doCancel();}
		});

		bottomPanel.add(okButton);
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doOk();}
		});
		
		add(mainPanel,BorderLayout.CENTER);
/*		
		add(editor, BorderLayout.CENTER);
		add(buttonPanel, BorderLayout.NORTH);
		buttonPanel.getAcceptButton().addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doOK();}			
		});
		buttonPanel.getAddButton().addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doAdd();}			
		});
		buttonPanel.getRemoveButton().addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doRemove();}			
		});
		buttonPanel.getEditButton().addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doEdit();}			
		});
*/
	}

	protected void doOk() {
		// TODO Need to add code here to scrape the screen
		controller.getSlidingPane().setSelectedPane(1);	
		
	}

	protected void doCancel() {
		controller.getSlidingPane().setSelectedPane(1);	
	}

	@Override
	public void activate() {
		controller.slideTo(ApplicationConfigurationController.EDITOR);
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