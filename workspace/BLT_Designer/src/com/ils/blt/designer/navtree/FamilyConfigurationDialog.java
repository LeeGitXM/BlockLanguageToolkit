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
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.designer.editor.BlockEditConstants;
import com.ils.blt.designer.editor.ValueEditPanel;
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

public class FamilyConfigurationDialog extends JDialog  { 
	private final static String TAG = "FamilyConfigurationDialog";
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 300;
	private final int DIALOG_WIDTH = 400;
	private final LoggerEx log;
	private final SerializableFamily family;
	
	
	public FamilyConfigurationDialog(SerializableFamily fam) {
		super();
		this.family = fam;
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        setSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {
		final String columnConstraints = "";
		final String layoutConstraints = "filly,ins 10";
		final String rowConstraints = "";
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel(new MigLayout("", "60[center]5[center]",""));
		add(buttonPanel, "dock south");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(family!=null) {
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
	 * @return the family that we are editing
	 */
	public SerializableFamily getFamily() { return family; }

}
