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

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SwingConstants;
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
	protected static final Dimension COMBO_SIZE  = new Dimension(300,24);
	final JTextField nameField = new JTextField();
	final JTextField tagField = new JTextField();
	final JFormattedTextField mostNegativeIncrementField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField mostPositiveIncrementField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField minimumIncrementField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField setpointHighLimitField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField setpointLowLimitField = new JFormattedTextField(NumberFormat.getInstance());
	final JCheckBox incrementalOutputCheckBox = new JCheckBox();
	final JComboBox<String> feedbackMethodComboBox = new JComboBox<String>();
	private static Icon previousIcon = new ImageIcon(Application.class.getResource("/images/arrow_left_green.png"));
	final JButton previousButton = new JButton(previousIcon);
	final JButton okButton = new JButton("Ok");
	private static Icon tagBrowserIcon = new ImageIcon(Application.class.getResource("/images/arrow_right_green.png"));
	final JButton nextButton = new JButton("Tags", tagBrowserIcon);
	
	protected static final Dimension TEXT_FIELD_SIZE  = new Dimension(300,24);
	protected static final Dimension NUMERIC_FIELD_SIZE  = new Dimension(100,24);
	
	// The constructor
	public OutputEditorPane(ApplicationConfigurationController controller, Application app, SortedListModel model) {
		super(new BorderLayout(20, 30));
		this.controller = controller;
		this.application = app;
		Format floatFormat = NumberFormat.getInstance();
		System.out.println("In Output Editor pane constructor");
				
		JPanel mainPanel = new JPanel(new MigLayout("", "[right]"));

		JLabel label = new JLabel("Quant Output Editor");
		label.setHorizontalAlignment(SwingConstants.CENTER);
		mainPanel.add(label, BorderLayout.NORTH);
		
		mainPanel.add(new JLabel("Name:"), "gap 10,gaptop 10");
		nameField.setPreferredSize(TEXT_FIELD_SIZE);
		nameField.setToolTipText("The name of the Quant Output.");
		mainPanel.add(nameField, "span, growx, wrap");

		mainPanel.add(new JLabel("Tag:"), "gap 10");		
		tagField.setPreferredSize(TEXT_FIELD_SIZE);
		tagField.setToolTipText("The name of the OPC tag that corresponds to this quant output.");
		mainPanel.add(tagField, "growx");
		
		mainPanel.add(nextButton,"right, wrap");
		nextButton.setHorizontalTextPosition(SwingConstants.LEFT);
		nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doTagSelector();}			
		});

		// Configure the Feedback method combo box
		mainPanel.add(new JLabel("Feedback Method:"), "gap 10");
		List<String> feedbackMethods = application.getFeedbackMethods();
		System.out.println("Loading feedback methods...");
		if( feedbackMethods!=null ) {
			for(Object feedbackMethod : application.getFeedbackMethods()) {
				System.out.println("Found a feedback method...");
				feedbackMethodComboBox.addItem((String) feedbackMethod);
			}
		}
		feedbackMethodComboBox.setToolTipText("The technique used to combine multiple recommendations for the this output!");
		feedbackMethodComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(feedbackMethodComboBox, "span, growx, wrap");
		
		mainPanel.add(new JLabel("Incremental Output:"), "gap 10");
		mainPanel.add(incrementalOutputCheckBox, "wrap, align left");
		
		JPanel incrementalContainer = new JPanel(new MigLayout("", "[right,110][]", ""));
		incrementalContainer.setBorder(BorderFactory.createTitledBorder("Incremental Changes"));
		
		incrementalContainer.add(new JLabel("Most Negative:"), "gap 10");
		mostNegativeIncrementField.setPreferredSize(NUMERIC_FIELD_SIZE);
		mostNegativeIncrementField.setToolTipText("The largest negative change.");
		incrementalContainer.add(mostNegativeIncrementField, "span, growx");

		incrementalContainer.add(new JLabel("Most Positive:"), "gap 10");
		mostPositiveIncrementField.setPreferredSize(NUMERIC_FIELD_SIZE);
		mostPositiveIncrementField.setToolTipText("The largest positive change.");
		incrementalContainer.add(mostPositiveIncrementField, "span, growx");
		
		incrementalContainer.add(new JLabel("Min Change:"), "gap 10");
		minimumIncrementField.setPreferredSize(NUMERIC_FIELD_SIZE);
		minimumIncrementField.setToolTipText("The minimum absolute change.");
		incrementalContainer.add(minimumIncrementField, "span, growx");

		mainPanel.add(incrementalContainer, "span, growx, wrap");
		
		JPanel absoluteContainer = new JPanel(new MigLayout("", "[right,110][]", ""));
		absoluteContainer.setBorder(BorderFactory.createTitledBorder("Absolute Changes"));
		
		absoluteContainer.add(new JLabel("Low Limit:"), "gap 10");
		setpointLowLimitField.setPreferredSize(NUMERIC_FIELD_SIZE);
		setpointLowLimitField.setToolTipText("The absolute lower limit.");
		absoluteContainer.add(setpointLowLimitField, "span, growx");
		
		absoluteContainer.add(new JLabel("High Limit:"), "gap 10");
		setpointHighLimitField.setPreferredSize(NUMERIC_FIELD_SIZE);
		setpointHighLimitField.setToolTipText("The absolute upper limit.");
		absoluteContainer.add(setpointHighLimitField, "span, growx");
		
		mainPanel.add(absoluteContainer, "span, growx, wrap");
		
		// Now the buttons that go at the bottom
		JPanel bottomPanel = new JPanel(new MigLayout("","[25%, left][50%, center][25%]",""));
		add(bottomPanel, BorderLayout.SOUTH);
		bottomPanel.add(previousButton);
		previousButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doPrevious();}
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
		incrementalOutputCheckBox.setSelected((boolean) outputMap.get("IncrementalOutput"));
		mostNegativeIncrementField.setValue(outputMap.get("MostNegativeIncrement"));
		mostPositiveIncrementField.setValue(outputMap.get("MostPositiveIncrement"));
		minimumIncrementField.setValue(outputMap.get("MinimumIncrement"));
		setpointLowLimitField.setValue(outputMap.get("SetpointLowLimit"));
		setpointHighLimitField.setValue(outputMap.get("SetpointHighLimit"));
		feedbackMethodComboBox.setSelectedItem((String) outputMap.get("FeedbackMethod"));
	}
	

	// The user pressed the OK button so save everything (I don't keep track of what, if anything, was 
	// changed so assume they change everything.
	protected void doOk() {
		System.out.println("In OutputEditorPane:doOk()");
		
		// Update the outputMap with everything in the screen
		outputMap.put("QuantOutput", nameField.getText());
		outputMap.put("TagPath", tagField.getText());
		outputMap.put("IncrementalOutput", incrementalOutputCheckBox.isSelected());
		outputMap.put("MostNegativeIncrement", mostNegativeIncrementField.getValue());
		outputMap.put("MostPositiveIncrement", mostPositiveIncrementField.getValue());
		outputMap.put("MinimumIncrement", minimumIncrementField.getValue());
		outputMap.put("SetpointLowLimit", setpointLowLimitField.getValue());
		outputMap.put("SetpointHighLimit", setpointHighLimitField.getValue());
		outputMap.put("FeedbackMethod", feedbackMethodComboBox.getSelectedItem());

		System.out.println("Saving values: " + outputMap);
		
		controller.refreshOutputs();

		// Slide back to the Outputs pane
		controller.getSlidingPane().setSelectedPane(ApplicationConfigurationController.OUTPUTS);	
	}

	protected void doPrevious() {
		controller.getSlidingPane().setSelectedPane(ApplicationConfigurationController.OUTPUTS);	
	}
	
	protected void doTagSelector() {
		controller.getSlidingPane().setSelectedPane(ApplicationConfigurationController.TAGSELECTOR);	
	}

	@Override
	public void activate() {
		controller.slideTo(ApplicationConfigurationController.EDITOR);
	}

}