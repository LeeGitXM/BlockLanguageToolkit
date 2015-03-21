/**
 *   (c) 2015  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.config;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;

import java.util.HashMap;
import java.util.Iterator;
import java.util.ResourceBundle;

/**
 * Display a dialog to configure the outputs available for a Final Diagnosis.
 */
public class FinalDiagnosisQOConfiguration extends JDialog {
	private final int DIALOG_HEIGHT = 200;
	private final int DIALOG_WIDTH = 400;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final ResourceBundle rb;
	private final ApplicationRequestHandler requestHandler;
	
	public FinalDiagnosisQOConfiguration(Frame frame,ProcessDiagramView diag,ProcessBlockView view) {
		super(frame);
		this.diagram = diag;
		this.block = view;
		this.setTitle("Configure Outputs for Final Diagnosis");
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.block");  // block.properties
		setAlwaysOnTop(true);
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		this.requestHandler = new ApplicationRequestHandler();
        initialize();
	}
	
	private void initialize() {

		DualListBox dual = new DualListBox();
		dual.addSourceElements(new String[] {"One", "Two", "Three"});
		
		add(dual, BorderLayout.CENTER);
		setSize(400,300);
		setVisible(true);
		// The OK button reads the values from the widgets and propagates to output
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton(rb.getString("Force.ForceButton"));
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Loop over the values and place in the target list
				dispose();
			}
		});
		JButton cancelButton = new JButton(rb.getString("Force.CancelButton"));
		buttonPanel.add(cancelButton, "");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
	}
}
