package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
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

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.script.Script;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
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
	private final ApplicationPropertyEditor editor;
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
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
	final JButton previousButton = new JButton(previousIcon);
	private static Icon tagBrowserIcon = new ImageIcon(OutputEditorPane.class.getResource("/images/arrow_right_green.png"));
	final JButton tagButton = new JButton("Tags", tagBrowserIcon);
	private final UtilityFunctions fcns = new UtilityFunctions();
	
	protected static final Dimension TEXT_FIELD_SIZE  = new Dimension(120,24);
	protected static final Dimension MIN_FIELD_SIZE  = new Dimension(150,20);
	protected static final Dimension NUMERIC_FIELD_SIZE  = new Dimension(100,24);
	
	// The constructor
	public OutputEditorPane(ApplicationPropertyEditor editor) {
		super(new BorderLayout(20, 30));
		this.editor = editor;
		this.log = LogMaker.getLogger(this);
		this.model = editor.getModel();
		this.setPreferredSize(editor.PANEL_SIZE);
		
		// This is the starting point, it looks great except it doeesn't grow if the panel grows
		//mainPanel = new JPanel(new MigLayout("", "[right]"));
		
		// From the white paper on MIG layouts  - how does this thing know to have 3 columns??
		mainPanel = new JPanel(new MigLayout("fillx", "[right]rel[grow, fill]"));
		
		add(mainPanel,BorderLayout.CENTER);

		JLabel label = new JLabel("Quant Output Editor");
		label.setHorizontalAlignment(SwingConstants.CENTER);
		mainPanel.add(label, BorderLayout.NORTH);
		
		mainPanel.add(new JLabel("Name:"), "gap 10,gaptop 10");
		nameField.setPreferredSize(TEXT_FIELD_SIZE);
		nameField.setToolTipText("The name of the Quant Output.");
		mainPanel.add(nameField, "span, growx, wrap");

		mainPanel.add(new JLabel("Tag:"), "gap 10");
		
		// I want the field wide and the button small but my MIG layout somehow wants 3 columns 
		// BorderLayout mostly works, the height of the field is a little bigger than I'd like, but it is pretty close.
		JPanel tagPanel = new JPanel(new BorderLayout());
		tagField.setMinimumSize(MIN_FIELD_SIZE);

		tagField.setToolTipText("The name of the OPC tag that corresponds to this quant output.");
		tagPanel.add(tagField, BorderLayout.CENTER);
		
		tagPanel.add(tagButton, BorderLayout.EAST);
		tagButton.setHorizontalTextPosition(SwingConstants.LEFT);
		tagButton.setToolTipText("Select a tag from the tag tree.");
		tagButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doTagSelector();}			
		});
		//mainPanel.add(tagPanel, "span");  This one was centered...
		//mainPanel.add(tagPanel, "span, growx");  This was also centered...
		// The tagField is taller than I'd like, I think because it is in a BorderLayout panel, but I did that to get the tag button correctly sized. PH 07/01/2021
		mainPanel.add(tagPanel, "span, growx, wrap, gapy 2");

		// Configure the Feedback method combo box
		mainPanel.add(new JLabel("Feedback Method:"), "gap 10");
		List<String> feedbackMethods = model.getLists().get("FeedbackMethods");
		if( feedbackMethods!=null ) {
			for(String feedbackMethod : feedbackMethods) {
				feedbackMethodComboBox.addItem(feedbackMethod);
			}
		}
		ApplicationRequestHandler requestHandler = new ApplicationRequestHandler();
		String db = requestHandler.getProductionDatabase();
		String tag = requestHandler.getProductionTagProvider();
		SerializableApplication app = ((ApplicationPropertyEditor)editor).getApplication();
		if( app.getState().equals(DiagramState.ISOLATED)) {
			db = requestHandler.getIsolationDatabase();
			tag = requestHandler.getIsolationTagProvider();
		}
		feedbackMethodComboBox.setToolTipText("The technique used to combine multiple recommendations for the this output!");
		feedbackMethodComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		List<String> items = new ArrayList<>();
		Script script = extensionManager.createExtensionScript(ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.GET_LIST_OPERATION, tag);
		extensionManager.runScript(editor.context.getScriptManager(),script,ScriptConstants.LIST_KEY_FEEDBCK_METHOD,items,db);
		for(String item:items) {
			feedbackMethodComboBox.addItem(item);
		}
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
		
		// Add the Previous / Back button - it should be all the way at the bottom, anchored to the left side.
		// Perform a save of all the fields before we go to the outputs
		JPanel bottomPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		add(bottomPanel,BorderLayout.SOUTH);
		bottomPanel.add(previousButton);
		
		// This is saving the contents of the pane all the way to the database.  That isn't really correct.  It should save it to a data structure in the 
		// Designer that only gets written to the DB when they press File -> Save.  We should mark the application / output as Dirty.
		previousButton.setPreferredSize(ApplicationPropertyEditor.NAV_BUTTON_SIZE);
		previousButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				save();
				editor.saveResource();
				editor.setSelectedPane(ApplicationPropertyEditor.OUTPUTS);
			}
		});

		// There were two action listeners here - I think you should only have one, so maybe this was an attempt to cache the change until they select File-> Save PH 06/29/3021
//		previousButton.setPreferredSize(ApplicationPropertyEditor.NAV_BUTTON_SIZE);
//		previousButton.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent e) {
//				editor.setSelectedPane(ApplicationPropertyEditor.OUTPUTS);
//			}
//		});	
		
		previousButton.setHorizontalAlignment(SwingConstants.LEFT);
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
		if( feedbackMethodComboBox.getItemCount()>0) {
			feedbackMethodComboBox.setSelectedItem((String) outputMap.get("FeedbackMethod"));
		}
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