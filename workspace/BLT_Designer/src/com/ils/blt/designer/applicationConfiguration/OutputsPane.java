package com.ils.blt.designer.applicationConfiguration;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
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


public class OutputsPane extends JPanel implements ApplicationConfigurationController.EditorPane {
	private ApplicationConfigurationController controller;
	private Application application;
	private SortedListModel outputListModel;
	private Map<String,Object> outputMap;
	private ArrayList< Map<String,Object> > outputList;
	private static final long serialVersionUID = 2882399376824334428L;
	private static Icon addIcon = new ImageIcon(Application.class.getResource("/images/add.png"));
	private static Icon deleteIcon = new ImageIcon(Application.class.getResource("/images/delete.png"));
	private static Icon previousIcon = new ImageIcon(Application.class.getResource("/images/arrow_left_green.png"));
	final JButton previousButton = new JButton(previousIcon);
	final JButton addButton = new JButton(addIcon);
	final JButton deleteButton = new JButton(deleteIcon);
	final JButton editButton = new JButton("Edit");
	final JPanel buttonPanel;
	final JList jlist;
	final JScrollPane outputsScrollPane = new JScrollPane();
/*	
	private PropertyEditor editor = new PropertyEditor();
	private ButtonPanel buttonPanel = new ButtonPanel(true, true, true, true, false,  RecipeEditorController.background);

	private Data recipeData;
*/	
	public OutputsPane(ApplicationConfigurationController controller, Application app, SortedListModel model) {
		super(new BorderLayout(20, 30));
		System.out.println("In Outputs pane constructor");
		this.controller = controller;
		this.outputListModel = model;
		this.application = app;
		
		JLabel label = new JLabel("Outputs");
		label.setHorizontalAlignment(JLabel.CENTER);
		add(label, BorderLayout.NORTH);
		
		JList lst = new JList(model);
		JScrollPane scrollPane = new JScrollPane(lst);
		this.jlist=lst;
		add(scrollPane, BorderLayout.CENTER);
		
		// The three button along the right are in their own panel
		buttonPanel = new JPanel();
		BoxLayout layout = new BoxLayout(buttonPanel, BoxLayout.Y_AXIS);
		buttonPanel.setLayout(layout);
		add(buttonPanel,BorderLayout.EAST);
		
		addButton.setAlignmentX(RIGHT_ALIGNMENT);
		buttonPanel.add(addButton);
		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doAdd();}
		});
		
		deleteButton.setAlignmentX(RIGHT_ALIGNMENT);
		buttonPanel.add(deleteButton);
		deleteButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doDelete();}
		});
		
		editButton.setAlignmentX(RIGHT_ALIGNMENT);
		buttonPanel.add(editButton);
		editButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doEdit();}
		});
		
		// The previous button should be all the way at the bottom, hugging the left side.
		JPanel bottomPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		add(bottomPanel,BorderLayout.SOUTH);
		bottomPanel.add(previousButton);
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
			System.out.println("Output is NULL!!!");
			return;
		}
		
		System.out.println("Output: " + outputName);

		// Get the Map that corresponds to the name that is selected
		Map<String,Object> outputMap=application.getOutput(outputName);
		if (outputMap != null){
			System.out.println("Looking at an Output" + outputMap);
			// Get the output editor and call method that puts the output into the fields
			OutputEditorPane outputEditor=controller.getOutputEditor();
			outputEditor.updateFields(outputMap);
			controller.getSlidingPane().setSelectedPane(2);
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
		application.deleteQuantOutput(outputName);
		controller.refreshOutputs();
	}

	protected void doAdd() {
		System.out.println("In doAdd()");
		
		// Get the Map that corresponds to the name that is selected
		Map<String,Object> outputMap=application.newOutput();
		if (outputMap != null){
			System.out.println("Looking at an Output" + outputMap);
			// Get the output editor and call method that puts the output into the fields
			OutputEditorPane outputEditor=controller.getOutputEditor();
			outputEditor.updateFields(outputMap);
			controller.getSlidingPane().setSelectedPane(2);
		}
	}

	protected void doPrevious() {
		controller.getSlidingPane().setSelectedPane(0);		
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
