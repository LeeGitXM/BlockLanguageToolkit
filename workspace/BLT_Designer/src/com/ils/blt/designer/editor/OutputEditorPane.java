package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import com.ils.blt.common.UtilityFunctions;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;

import net.miginfocom.swing.MigLayout;

/**
 * Handle editing Quant Outputs for an application. This panel handles details for 
 * a single named output.
 */
public class OutputEditorPane extends JPanel implements ActionListener  {
	private static final long serialVersionUID = -5387165467458025431L;
	private final static String CLSS = "OutputEditorPane";
	private static final Insets insets = new Insets(0,0,0,0);
	private final ApplicationPropertyEditor editor;
	private final JPanel mainPanel;
	private final GeneralPurposeDataContainer model;
	private Map<String,String> outputMap;  // Parameters for current output
	private final ILSLogger log;
	final JTextField nameField = new JTextField();
	final JTextField tagField = new JTextField();
	final JFormattedTextField mostNegativeIncrementField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField mostPositiveIncrementField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField minimumIncrementField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField setpointHighLimitField = new JFormattedTextField(NumberFormat.getInstance());
	final JFormattedTextField setpointLowLimitField = new JFormattedTextField(NumberFormat.getInstance());
	final JCheckBox incrementalOutputCheckBox = new JCheckBox();
	final JComboBox<String> feedbackMethodComboBox = new JComboBox<String>();
	private static Icon previousIcon = new ImageIcon(OutputEditorPane.class.getResource("/images/arrow_left_green.png"));
	final JButton homeButton = new JButton(previousIcon);
	//final JButton cancelButton = new JButton("Cancel");
	private static Icon tagBrowserIcon = new ImageIcon(OutputEditorPane.class.getResource("/images/arrow_right_green.png"));
	final JButton tagButton = new JButton("Tags", tagBrowserIcon);
	private final UtilityFunctions fcns = new UtilityFunctions();
	
	protected static final Dimension TEXT_FIELD_SIZE  = new Dimension(120,24);
	protected static final Dimension NUMERIC_FIELD_SIZE  = new Dimension(100,24);
	
	// The constructor
	public OutputEditorPane(ApplicationPropertyEditor editor) {
		super(new BorderLayout(20, 30));
		this.editor = editor;
		this.log = LogMaker.getLogger(this);
		this.model = editor.getModel();
				
		//mainPanel = new JPanel(new MigLayout("", "[right]"));
		mainPanel = new JPanel(new MigLayout());
		add(mainPanel,BorderLayout.CENTER);

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
		
		mainPanel.add(tagButton,"right, wrap");
		tagButton.setHorizontalTextPosition(SwingConstants.LEFT);
		tagButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doTagSelector();}			
		});

		// Configure the Feedback method combo box
		mainPanel.add(new JLabel("Feedback Method:"), "gap 10");
		List<String> feedbackMethods = model.getLists().get("FeedbackMethods");
		if( feedbackMethods!=null ) {
			for(String feedbackMethod : feedbackMethods) {
				feedbackMethodComboBox.addItem(feedbackMethod);
			}
		}
		feedbackMethodComboBox.setToolTipText("The technique used to combine multiple recommendations for the this output!");
		feedbackMethodComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		mainPanel.add(feedbackMethodComboBox, "span, growx, wrap");
		
		mainPanel.add(new JLabel("Incremental Output:"), "gap 10");
		mainPanel.add(incrementalOutputCheckBox, "wrap, align left");
		
		JPanel incrementalContainer = new JPanel(new MigLayout("", "[right,110][]", ""));
		incrementalContainer.setBorder(BorderFactory.createTitledBorder("Incremental Changes"));
		
		incrementalContainer.add(new JLabel("Most Negative:"), "gap 10");
		mostNegativeIncrementField.setPreferredSize(NUMERIC_FIELD_SIZE);
		mostNegativeIncrementField.setToolTipText("The largest negative change.");
		mostNegativeIncrementField.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT);
		incrementalContainer.add(mostNegativeIncrementField, "span, growx");

		incrementalContainer.add(new JLabel("Most Positive:"), "gap 10");
		mostPositiveIncrementField.setPreferredSize(NUMERIC_FIELD_SIZE);
		mostPositiveIncrementField.setToolTipText("The largest positive change.");
		mostPositiveIncrementField.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT);
		incrementalContainer.add(mostPositiveIncrementField, "span, growx");
		
		incrementalContainer.add(new JLabel("Min Change:"), "gap 10");
		minimumIncrementField.setPreferredSize(NUMERIC_FIELD_SIZE);
		minimumIncrementField.setToolTipText("The minimum absolute change.");
		minimumIncrementField.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT);
		incrementalContainer.add(minimumIncrementField, "span, growx");

		mainPanel.add(incrementalContainer, "span, growx, wrap");
		
		JPanel absoluteContainer = new JPanel(new MigLayout("", "[right,110][]", ""));
		absoluteContainer.setBorder(BorderFactory.createTitledBorder("Absolute Changes"));
		
		absoluteContainer.add(new JLabel("Low Limit:"), "gap 10");
		setpointLowLimitField.setPreferredSize(NUMERIC_FIELD_SIZE);
		setpointLowLimitField.setToolTipText("The absolute lower limit.");
		setpointLowLimitField.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT);
		absoluteContainer.add(setpointLowLimitField, "span, growx");
		
		absoluteContainer.add(new JLabel("High Limit:"), "gap 10");
		setpointHighLimitField.setPreferredSize(NUMERIC_FIELD_SIZE);
		setpointHighLimitField.setToolTipText("The absolute upper limit.");
		setpointHighLimitField.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT);
		absoluteContainer.add(setpointHighLimitField, "span, growx");
		
		mainPanel.add(absoluteContainer, "span, growx, wrap");
		
		// Now the arrow button
		// Perform a save of all the fields before we go to the outputs
		JPanel bottomPanel = new JPanel(new MigLayout("","[25%, left][50%, center][25%]",""));
		mainPanel.add(bottomPanel, "span");
		homeButton.setPreferredSize(ApplicationPropertyEditor.BUTTON_SIZE);
		homeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				save();
				editor.saveResource();
				editor.setSelectedPane(ApplicationPropertyEditor.OUTPUTS);
			}
		});
		bottomPanel.add(homeButton);
		homeButton.setPreferredSize(ApplicationPropertyEditor.BUTTON_SIZE);
		homeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editor.setSelectedPane(ApplicationPropertyEditor.OUTPUTS);
			}
		});
		//bottomPanel.add(cancelButton);
		
	}

	/**
	 * Called from the OutputsPane to configure the particular output to edit.
	 * @param map
	 */
	public void updateFields(Map<String,String> map){
		outputMap=map;
		nameField.setText((String) outputMap.get("QuantOutput"));
		tagField.setText((String) outputMap.get("TagPath"));
		incrementalOutputCheckBox.setSelected(fcns.coerceToBoolean(outputMap.get("IncrementalOutput")));
		double dbl = fcns.coerceToDouble(outputMap.get("MostNegativeIncrement"));
		mostNegativeIncrementField.setValue(dbl);
		dbl = fcns.coerceToDouble(outputMap.get("MostPositiveIncrement"));
		mostPositiveIncrementField.setValue(dbl);
		dbl = fcns.coerceToDouble(outputMap.get("MinimumIncrement"));
		minimumIncrementField.setValue(dbl);
		dbl = fcns.coerceToDouble(outputMap.get("SetpointLowLimit"));
		setpointLowLimitField.setValue(dbl);
		dbl = fcns.coerceToDouble(outputMap.get("SetpointHighLimit"));
		setpointHighLimitField.setValue(dbl);
		feedbackMethodComboBox.setSelectedItem((String) outputMap.get("FeedbackMethod"));
	}
	
	public JTextField getTagField() { return this.tagField; }

	// The user exited a field, so save everything (I don't keep track of what, if anything, was 
	// changed.
	protected void save() {
		// Update the outputMap with everything in the screen
		String name = nameField.getText();
		outputMap.put("QuantOutput", name);
		outputMap.put("TagPath", tagField.getText());
		outputMap.put("IncrementalOutput", (incrementalOutputCheckBox.isSelected()?"1":"0"));
		outputMap.put("MostNegativeIncrement", mostNegativeIncrementField.getText());
		outputMap.put("MostPositiveIncrement", mostPositiveIncrementField.getText());
		outputMap.put("MinimumIncrement", minimumIncrementField.getText());
		outputMap.put("SetpointLowLimit", setpointLowLimitField.getText());
		outputMap.put("SetpointHighLimit", setpointHighLimitField.getText());
		if(feedbackMethodComboBox.getSelectedItem()!=null) {
			outputMap.put("FeedbackMethod", feedbackMethodComboBox.getSelectedItem().toString());
		}

		log.infof("%s.save: Saving %s\n ",CLSS,outputMap.toString());
		// Substitute the current output map into the list of quant outputs
		// We do a linear search ... remove the existing map and add our new one at the end
		List<Map<String,String>> outputList=model.getMapLists().get("QuantOutputs");
		if( outputList==null ) {
			outputList = new ArrayList<>();
			model.getMapLists().put("QuantOutputs",outputList);
		}
		for(Map<String,String> map : outputList) {
			String str = (String) map.get("QuantOutput");
			if(str!=null && str.equals(name)) {
				outputList.remove(map);
				break;
			}
		}
		outputList.add(outputMap);
		editor.refreshOutputs();	
	}
	
	protected void doTagSelector() {
		editor.setSelectedPane(ApplicationPropertyEditor.TAGSELECTOR);	
	}

	// ============================================== Action listener ==========================================
	@Override
	public void actionPerformed(ActionEvent event) {
		save();
		editor.saveResource();
	}	
}