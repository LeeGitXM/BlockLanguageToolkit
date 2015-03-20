package com.ils.blt.designer.applicationConfiguration;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.Format;
import java.text.NumberFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
	private Application application;
	private Map<String,Object> outputMap;
	private static final Insets insets = new Insets(0,0,0,0);
	final JTextField nameField = new JTextField();
	final JTextField tagField = new JTextField();
	final JFormattedTextField mostNegativeIncrementField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField mostPositiveIncrementField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField minimumIncrementField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField setpointHighLimitField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField setpointLowLimitField = new JFormattedTextField(NumberFormat.getInstance());
	final JTextField feedbackMethodField = new JTextField();
	final JButton cancelButton = new JButton("Cancel");
	final JButton okButton = new JButton("OK");
	protected static final Dimension TEXT_FIELD_SIZE  = new Dimension(300,24);
	protected static final Dimension NUMERIC_FIELD_SIZE  = new Dimension(100,24);
	
/*	
	private PropertyEditor editor = new PropertyEditor();
	private ButtonPanel buttonPanel = new ButtonPanel(true, true, true, true, false,  RecipeEditorController.background);

	private Data recipeData;
*/	
	// The constructor
	public OutputEditorPane(ApplicationConfigurationController controller, Application app, SortedListModel model) {
		super(new BorderLayout(20, 30));
		this.controller = controller;
		this.application = app;
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
		
		mainPanel.add(new JLabel("Name:"), "gap 10,gaptop 10");
		nameField.setPreferredSize(TEXT_FIELD_SIZE);
		nameField.setToolTipText("The name of the Quant Output.");
		mainPanel.add(nameField, "span, growx");

		mainPanel.add(new JLabel("Tag:"), "gap 10");		
		tagField.setPreferredSize(TEXT_FIELD_SIZE);
		tagField.setToolTipText("The name of the OPC tag that corresponds to this quant output.");
		mainPanel.add(tagField, "span, growx");
		
		mainPanel.add(new JLabel("Most Negative:"), "gap 10");
		mostNegativeIncrementField.setPreferredSize(NUMERIC_FIELD_SIZE);
		mostNegativeIncrementField.setToolTipText("The largest negative change.");
		mainPanel.add(mostNegativeIncrementField, "span, growx");

		mainPanel.add(new JLabel("Most Positive:"), "gap 10");
		mostPositiveIncrementField.setPreferredSize(NUMERIC_FIELD_SIZE);
		mostPositiveIncrementField.setToolTipText("The largest positive change.");
		mainPanel.add(mostPositiveIncrementField, "span, growx");
		
		mainPanel.add(new JLabel("Min Change:"), "gap 10");
		minimumIncrementField.setPreferredSize(NUMERIC_FIELD_SIZE);
		minimumIncrementField.setToolTipText("The minimum absolute change.");
		mainPanel.add(minimumIncrementField, "span, growx");

		mainPanel.add(new JLabel("Low Limit:"), "gap 10");
		setpointLowLimitField.setPreferredSize(NUMERIC_FIELD_SIZE);
		setpointLowLimitField.setToolTipText("The absolute lower limit.");
		mainPanel.add(setpointLowLimitField, "span, growx");
		
		mainPanel.add(new JLabel("High Limit:"), "gap 10");
		setpointHighLimitField.setPreferredSize(NUMERIC_FIELD_SIZE);
		setpointHighLimitField.setToolTipText("The absolute upper limit.");
		mainPanel.add(setpointHighLimitField, "span, growx");
		
		// TODO this should be a combo box
		mainPanel.add(new JLabel("Feedback Method:"), "gap 10");
		feedbackMethodField.setPreferredSize(TEXT_FIELD_SIZE);
		feedbackMethodField.setToolTipText("The name of the Quant Output.");
		mainPanel.add(feedbackMethodField, "span, growx");
		
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
	}

	public void updateFields(Map<String,Object> map){
		System.out.println("In OutputEditorPane:updateFields() with " + map);
		outputMap=map;
		nameField.setText((String) outputMap.get("QuantOutput"));
		tagField.setText((String) outputMap.get("TagPath"));
		mostNegativeIncrementField.setValue(outputMap.get("MostNegativeIncrement"));
		mostPositiveIncrementField.setValue(outputMap.get("MostPositiveIncrement"));
		minimumIncrementField.setValue(outputMap.get("MinimumIncrement"));
		setpointLowLimitField.setValue(outputMap.get("SetpointLowLimit"));
		setpointHighLimitField.setValue(outputMap.get("SetpointHighLimit"));
		feedbackMethodField.setText((String) outputMap.get("FeedbackMethod"));
	}
	

	// The user pressed the OK button so save everything (I don't keep track of what, if anything, was 
	// changed so assume they change everything.
	protected void doOk() {
		// TODO Need to add code here to scrape the screen
		System.out.println("In OutputEditorPane:doOk()");
		
		// Update the outputMap with everything in the screen
		outputMap.put("QuantOutput", nameField.getText());
		outputMap.put("TagPath", tagField.getText());
		outputMap.put("MostNegativeIncrement", mostNegativeIncrementField.getValue());
		outputMap.put("MostPositiveIncrement", mostPositiveIncrementField.getValue());
		outputMap.put("MinimumIncrement", minimumIncrementField.getValue());
		outputMap.put("SetpointLowLimit", setpointLowLimitField.getValue());
		outputMap.put("SetpointHighLimit", setpointHighLimitField.getValue());
		outputMap.put("FeedbackMethod", feedbackMethodField.getText());

		System.out.println("Saving values: " + outputMap);
		
		controller.refreshOutputs();

		// Slide back to the Outputs pane
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