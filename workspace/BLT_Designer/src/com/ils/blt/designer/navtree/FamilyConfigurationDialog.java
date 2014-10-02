/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.serializable.SerializableFamily;
/**
 * Display a dialog to export a diagram.
 *    ExportDialog ed = new ExportDialog("Attribute Editor");
 *    bad.pack();
 *    bad.setVisible(true);   // Terminates when dialog closed.
 *    result = bad.getModel();
 */

public class FamilyConfigurationDialog extends ConfigurationDialog  { 
	private final static String TAG = "FamilyConfigurationDialog";
	private static final long serialVersionUID = 2882399376824334427L;
	private final SerializableFamily family;
	
	
	public FamilyConfigurationDialog(SerializableFamily fam) {
		super();
		this.family = fam;
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {


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
