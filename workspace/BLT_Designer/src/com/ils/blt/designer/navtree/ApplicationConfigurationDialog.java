/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.block.RampMethod;
import com.ils.blt.common.serializable.SerializableApplication;
/**
 * Display a dialog to configure an Application node
 */

public class ApplicationConfigurationDialog extends ConfigurationDialog { 
	private final static String TAG = "ApplicationConfigurationDialog";
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 380;
	private final int DIALOG_WIDTH = 440;
	private final SerializableApplication application;
	private JTextField consoleField;
	private JCheckBox menuCheckBox;
	private JComboBox<String> methodBox;
	private JTextField queueField;
	private JTextField unitField;
	
	public ApplicationConfigurationDialog(Frame frame,SerializableApplication app) {
		super(frame);
		this.application = app;
		this.setTitle(PREFIX+".Application.Title");
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
	}

	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {
		
		add(createLabel(PREFIX+".Application.Name"),"");
		nameField = createTextField(PREFIX+".Application.Name",application.getName());
		add(nameField,"span,wrap");
		
		add(createLabel(PREFIX+".Application.Description"),"gaptop 2,aligny top");
		descriptionArea = createTextArea(PREFIX+".Application.Description",application.getDescription());
		add(descriptionArea,"gaptop 2,aligny top,span,wrap");
		
		add(createLabel(PREFIX+".Application.Console"),"");
		consoleField = createTextField(PREFIX+".Application.Console",application.getConsole());
		add(consoleField,"span,wrap");
		
		add(createLabel(PREFIX+".Application.Queue"),"");
		queueField = createTextField(PREFIX+".Application.Queue",application.getMessageQueue());
		add(queueField,"span,wrap");
		
		add(createLabel(PREFIX+".Application.Unit"),"");
		unitField = createTextField(PREFIX+".Application.Unit",application.getUnit());
		add(unitField,"span,wrap");
		
		add(createLabel(PREFIX+".Application.Menu"),"");
		menuCheckBox = createCheckBox(PREFIX+".Application.Menu",application.isIncludeInMenus());
		add(menuCheckBox,"");
		add(createLabel(PREFIX+".Application.Ramp"),"gapleft 10");
		methodBox = createRampMethodCombo(PREFIX+".Application.Ramp",application.getRampMethod());
		add(methodBox,"wrap");
		
		add(createLabel(PREFIX+".Application.Priority"),"");
		// Highest priority is read-only
		JTextField priorityField = createTextField(PREFIX+".Application.Priority",String.valueOf(application.getHighestPriorityProblem()));
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		priorityField.setEnabled(false);
		add(priorityField,"");
		add(createLabel(PREFIX+".Application.State"),"gapleft 10");
		stateBox = createActiveStateCombo(PREFIX+".Application.State",application.getState());
		add(stateBox,"wrap");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, "dock south,gaptop 15");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Set attributes from fields
				application.setName(nameField.getText());
				application.setDescription(descriptionArea.getText());
				application.setConsole(consoleField.getText());
				application.setMessageQueue(queueField.getText());
				application.setUnit(unitField.getText());
				application.setIncludeInMenus(menuCheckBox.isSelected());
				String method = (String)methodBox.getSelectedItem();
				application.setRampMethod(RampMethod.valueOf(method));
				String activeState = (String)stateBox.getSelectedItem();
				application.setState(ActiveState.valueOf(activeState));
				dispose();
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton,"");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cancelled = true;
				dispose();
			}			
		});
	}
	/**
	 * @return the application that we are configuring.
	 */
	public SerializableApplication getApplication() { return application; }
	
}
