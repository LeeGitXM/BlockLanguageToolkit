/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableApplication;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
/**
 * Display a dialog to export a diagram.
 *    ExportDialog ed = new ExportDialog("Attribute Editor");
 *    bad.pack();
 *    bad.setVisible(true);   // Terminates when dialog closed.
 *    result = bad.getModel();
 */

public class ApplicationConfigurationDialog extends JDialog { 
	private final static String TAG = "ApplicationConfigurationDialog";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 300;
	private final int DIALOG_WIDTH = 400;
	public static final Dimension ENTRY_BOX_SIZE  = new Dimension(160,24);
	private final LoggerEx log;
	private final SerializableApplication application;
	private JTextField nameField;
	
	
	public ApplicationConfigurationDialog(SerializableApplication app) {
		super();
		this.application = app;
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        setSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout.
	 */
	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {
		final String columnConstraints = "[para]0[]0[]0[]0[]";
		final String layoutConstraints = "filly,ins 10";
		final String rowConstraints = "";
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		
		JPanel namePanel = new JPanel();
		namePanel.add(createLabel("Name"),"skip");
		nameField = createTextField("");
		namePanel.add(nameField,"span 2,growx,wrap");
		add(namePanel,"");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel(new MigLayout("", "60[center]5[center]",""));
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
	
	/**
	 * Create a new label. The text is the bundle key.
	 */
	private JLabel createLabel(String text) {
		return new JLabel(text);
	}
	/**
	 * Create a text field for editing the name
	 */
	private JTextField createTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(ENTRY_BOX_SIZE);
		field.setEditable(true);
		return field;
	}
}
