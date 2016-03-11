package com.ils.blt.designer.applicationConfiguration;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;

import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.SortedListModel;


public class OutputsPane extends JPanel implements ApplicationConfigurationController.EditorPane {
	private final ApplicationConfigurationController controller;
	private final GeneralPurposeDataContainer model;
	private SortedListModel<String> outputListModel;
	private Integer newOutputId = -1;
	private static final long serialVersionUID = 2882399376824334428L;
	private static Icon addIcon = new ImageIcon(OutputsPane.class.getResource("/images/add.png"));
	private static Icon deleteIcon = new ImageIcon(OutputsPane.class.getResource("/images/delete.png"));
	private static Icon previousIcon = new ImageIcon(OutputsPane.class.getResource("/images/arrow_left_green.png"));
	final JButton previousButton = new JButton(previousIcon);
	final JButton addButton = new JButton(addIcon);
	final JButton deleteButton = new JButton(deleteIcon);
	final JButton editButton = new JButton("Edit");
	final JPanel buttonPanel;
	private final JList<String> jlist;
	private final OutputEditorPane outputEditor;
	final JScrollPane outputsScrollPane = new JScrollPane();
/*	
	private PropertyEditor editor = new PropertyEditor();
	private ButtonPanel buttonPanel = new ButtonPanel(true, true, true, true, false,  RecipeEditorController.background);

	private Data recipeData;
*/	
	public OutputsPane(ApplicationConfigurationController controller,OutputEditorPane editor) {
		super(new BorderLayout(20, 30));
		System.out.println("In Outputs pane constructor");
		this.controller = controller;
		this.model = controller.getModel();
		this.outputEditor = editor;
		this.outputListModel = controller.getOutputListModel();
		
		JLabel label = new JLabel("Outputs");
		label.setHorizontalAlignment(SwingConstants.CENTER);
		add(label, BorderLayout.NORTH);
		
		jlist = new JList<String>(outputListModel);
		JScrollPane scrollPane = new JScrollPane(jlist);
		scrollPane.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(10,10,10,10),
								BorderFactory.createEtchedBorder(EtchedBorder.RAISED)));
		add(scrollPane, BorderLayout.CENTER);
		
		// The three button along the right are in their own panel
		buttonPanel = new JPanel();
		buttonPanel.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		BoxLayout layout = new BoxLayout(buttonPanel, BoxLayout.Y_AXIS);
		buttonPanel.setLayout(layout);
		add(buttonPanel,BorderLayout.EAST);
		
		addButton.setAlignmentX(RIGHT_ALIGNMENT);
		addButton.setPreferredSize(ApplicationConfigurationConstants.EDIT_BUTTON_SIZE);
		buttonPanel.add(addButton);
		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doAdd();}
		});
		
		deleteButton.setAlignmentX(RIGHT_ALIGNMENT);
		deleteButton.setPreferredSize(ApplicationConfigurationConstants.EDIT_BUTTON_SIZE);
		buttonPanel.add(deleteButton);
		deleteButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doDelete();}
		});
		
		editButton.setAlignmentX(RIGHT_ALIGNMENT);
		editButton.setPreferredSize(ApplicationConfigurationConstants.EDIT_BUTTON_SIZE);
		buttonPanel.add(editButton);
		editButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doEdit();}
		});
		
		// The previous button should be all the way at the bottom, hugging the left side.
		JPanel bottomPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		add(bottomPanel,BorderLayout.SOUTH);
		bottomPanel.add(previousButton);
		previousButton.setPreferredSize(ApplicationConfigurationConstants.BUTTON_SIZE);
		previousButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doPrevious();}
		});
	}
	
	public void refresh(){
		System.out.println("Refreshing...");
		outputListModel.clear();
	}

	protected void doEdit() {
		// Get the name of the output that is selected, if nothing is selected then return
		String outputName= (String) jlist.getSelectedValue();
		if( outputName==null ) {	
			JOptionPane.showMessageDialog(OutputsPane.this, "Please selecte an output to edit.");					
			return;
		}
		
		System.out.println("Output: " + outputName);

		// Get the Map that corresponds to the name that is selected.
		// We do a linear search ...
		List<Map<String,String>> outputList=model.getMapLists().get("QuantOutputs");
		Map<String,String> outputMap = null;
		if( outputList!=null ) {
			for(Map<String,String> map : outputList) {
				String str = (String) map.get("QuantOutput");
				if(str.equals(outputName)){
					outputMap = map;
					break;
				}
			}
		}
		
		
		if (outputMap != null){
			System.out.println("Looking at an Output" + outputMap);
			// Get the output editor and call method that puts the output into the fields
			outputEditor.updateFields(outputMap);
			controller.slideTo(ApplicationConfigurationConstants.EDITOR);
		}
	}

	protected void doDelete() {
		// Get the name of the output that is selected, if nothing is selected then return
		String outputName= (String) jlist.getSelectedValue();
		if( outputName==null ) {
			System.out.println("Output is NULL!!!");
			return;
		}
		System.out.println("Deleting Output: " + outputName + "...");
		// Again we do a linear search ...
		List<Map<String,String>> outputList=model.getMapLists().get("QuantOutputs");
		if( outputList!=null ) {
			for(Map<String,String> map : outputList) {
				String str = (String) map.get("QuantOutput");
				if(str.equals(outputName)){
					outputList.remove(map);
					break;
				}
			}
		}
		controller.refreshOutputs();
	}

	protected void doAdd() {	
		// Get the Map that corresponds to the name that is selected
		Map<String,String> outputMap=newOutput();
		if (outputMap != null){
			System.out.println("Looking at an Output" + outputMap);
			// Get the output editor and call method that puts the output into the fields
			outputEditor.updateFields(outputMap);
			controller.slideTo(ApplicationConfigurationConstants.EDITOR);
		}
	}

	protected void doPrevious() {
		controller.slideTo(ApplicationConfigurationConstants.HOME);		
	}

	@Override
	public void activate() {
		controller.slideTo(ApplicationConfigurationConstants.EDITOR);
	}

	// Create a new outputMap, which corresponds to a QuantOutput, with default values
	public Map<String,String> newOutput() {
		System.out.println("Creating a new output... ");

		Map<String,String> outputMap = new HashMap<String,String>();
		outputMap.put("QuantOutputId",String.valueOf(newOutputId));
		newOutputId = newOutputId - 1;
		outputMap.put("QuantOutput", "");
		outputMap.put("TagPath", "");
		outputMap.put("MostNegativeIncrement",String.valueOf(-10.0));
		outputMap.put("MostPositiveIncrement", String.valueOf(10.0));
		outputMap.put("MinimumIncrement", String.valueOf(0.01));
		outputMap.put("SetpointLowLimit", String.valueOf(0.0));
		outputMap.put("SetpointHighLimit", String.valueOf(100.0));
		outputMap.put("FeedbackMethod", "SIMPLE-SUM");
		outputMap.put("IncrementalOutput", String.valueOf(true));

		List<Map<String,String>> outputList=model.getMapLists().get("QuantOutputs");
		if( outputList==null ) {
			outputList = new ArrayList<>();
			model.getMapLists().put("QuantOutputs", outputList);
		}
		outputList.add(outputMap);
		return outputMap;
	}
}
