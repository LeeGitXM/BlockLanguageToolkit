/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer.extensions;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * This is a special-purpose editor for a FinalDiagnosis block.
 * Even though the block itself is implemented in Python, we
 * code the pop-up editor in Java Swing. We make use of the 
 * block properties SetAuxiliaryDataHook, GetAuxiliaryDataHook 
 * that define Python scripts that set and retrieve arbitrary
 * non-serialized data belonging to the block.
 */

public class FinalDiagnosisEditor extends JDialog {
	private static String TAG = "FinalDiagnosisEditor";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 2112388376824433327L;
	private final int DIALOG_HEIGHT = 320;
	private final int DIALOG_WIDTH = 500;
	private static final Dimension PANEL_SIZE  = new Dimension(480,120);
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;

	
	public FinalDiagnosisEditor(DesignerContext context,ProcessDiagramView diag,ProcessBlockView view) {
		super(context.getFrame());
		this.diagram = diag;
		this.block = view;
		this.setTitle(BundleUtil.get().getString(PREFIX+".NoteTextEdit.Title"));
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
	}
	
	/**
	 * Create the UI elements
	 */
	private void initialize() {
		
		// The internal panel has two panes - one for the JTextPane, the other for the JTextArea.
		setLayout(new BorderLayout());
		JPanel internalPanel = new JPanel();
	
		internalPanel.setLayout(new MigLayout("ins 2","",""));
		addSeparator(internalPanel,"This isn't really what we want to do ...");
		
		
		
		add(internalPanel,BorderLayout.CENTER);
		

		// The OK button simply closes the dialog
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Save any changes made during the edit
				block.setDirty(true);
				SwingUtilities.invokeLater(new WorkspaceRepainter());
				dispose();
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton, "");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
	}
	

	
	
	/**
	 * Add a separator to a panel using Mig layout
	 */
	private JLabel addSeparator(JPanel panel,String text) {
		JSeparator separator = new JSeparator();
		JLabel label = new JLabel(text);
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		panel.add(label, "split 2,span");
		panel.add(separator, "growx,wrap");
		return label;
	}

}
