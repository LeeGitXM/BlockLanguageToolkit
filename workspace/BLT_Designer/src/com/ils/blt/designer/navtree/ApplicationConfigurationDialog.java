/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.serializable.SerializableApplication;
/**
 * Display a dialog to export a diagram.
 *    ExportDialog ed = new ExportDialog("Attribute Editor");
 *    bad.pack();
 *    bad.setVisible(true);   // Terminates when dialog closed.
 *    result = bad.getModel();
 */

public class ApplicationConfigurationDialog extends ConfigurationDialog { 
	private final static String TAG = "ApplicationConfigurationDialog";
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 400;
	private final int DIALOG_WIDTH = 400;
	private final SerializableApplication application;
	
	
	public ApplicationConfigurationDialog(SerializableApplication app) {
		super();
		this.application = app;
		this.setTitle(PREFIX+".Application.Title");
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout.
	 */
	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {
		
		JPanel namePanel = new JPanel(new MigLayout("fillx","para[:80:]0[]",""));
		namePanel.add(createLabel(PREFIX+".Application.Name"),"");
		nameField = createTextField(PREFIX+".Application.Name","");
		namePanel.add(nameField,"");
		add(namePanel,"wrap");
		
		JPanel descriptionPanel = new JPanel(new MigLayout("fillx","para[:80:]0[]","[:100:]"));
		descriptionPanel.add(createLabel(PREFIX+".Application.Description"),"gaptop 2,aligny top");
		descriptionArea = createTextArea(PREFIX+".Application.Description","");
		descriptionPanel.add(descriptionArea,"gaptop 2,aligny top,span");
		add(descriptionPanel,"wrap");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, "dock south");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(application!=null) {
					// Set attributes from fields
				}
				dispose();
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton,"");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}			
		});
	}
	/**
	 * @return the application that we are configuring.
	 */
	public SerializableApplication getApplication() { return application; }
	
}
