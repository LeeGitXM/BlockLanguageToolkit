package com.ils.blt.designer.applicationConfiguration;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

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
	private SortedListModel outputListModel;
	private static final long serialVersionUID = 2882399376824334428L;
	final JButton previousButton = new JButton("Previous");
	final JButton addButton = new JButton("Add");
	final JButton deleteButton = new JButton("Delete");
	final JButton editButton = new JButton("Edit");
	final JList jlist;
	final JScrollPane outputsScrollPane = new JScrollPane();
/*	
	private PropertyEditor editor = new PropertyEditor();
	private ButtonPanel buttonPanel = new ButtonPanel(true, true, true, true, false,  RecipeEditorController.background);

	private Data recipeData;
*/	
	public OutputsPane(ApplicationConfigurationController controller, SortedListModel model) {
		super(new MigLayout(
				"",										// Layout Constraints
				"[20][300][20]",						// Column Constraints
				"[10][10][10][10][200][10]"	// Row Constraints
				));
		System.out.println("In  constructor");
		this.controller = controller;
		this.outputListModel = model;
		
		JLabel label = new JLabel("Outputs");
		add(label, "cell 1 0 1 1");
		
		JList jlist = new JList(model);
		JScrollPane scrollPane = new JScrollPane(jlist);
		this.jlist=jlist;
		add(scrollPane, "cell 0 1 2 4");
		
		add(previousButton, "cell 0 5");
		previousButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doPrevious();}
		});

		add(addButton, "cell 2 1");
		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doAdd();}
		});
		
		add(deleteButton, "cell 2 2");
		deleteButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doDelete();}
		});
		
		add(editButton, "cell 2 3");
		editButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doEdit();}
		});
		

		
		//TODO Need to add a JList and a Jscrollpane here
/*	
		add(new JScrollPane(sourceList),
				new GridBagConstraints(0,1,1,5,.5,1,GridBagConstraints.CENTER, 
						GridBagConstraints.BOTH, EMPTY_INSETS,0,0));
*/

	}

	protected void doEdit() {
		controller.getSlidingPane().setSelectedPane(2);
	}

	protected void doDelete() {
		// TODO Auto-generated method stub
	}

	protected void doAdd() {
		controller.getSlidingPane().setSelectedPane(2);
	}

	protected void doPrevious() {
		controller.getSlidingPane().setSelectedPane(0);		
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
