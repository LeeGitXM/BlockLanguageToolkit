/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer.workspace;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * This is a read-only viewer for blocks for blocks that return internal state
 * (theoretically all of them). 
 */

public class NoteTextEditor extends JDialog {
	private static String TAG = "NoteTextEditor";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 2002388376824434427L;
	private final int DIALOG_HEIGHT = 320;
	private final int DIALOG_WIDTH = 500;
	private static final Dimension TABLE_SIZE  = new Dimension(480,120);
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	
	public NoteTextEditor(ProcessDiagramView dia,ProcessBlockView view) {
		super();
		this.diagram = dia;
		this.block = view;
		this.setTitle(BundleUtil.get().getString(PREFIX+".NoteTextEdit.Title"));
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		queryBlock();
        initialize();
	}
	
	private void initialize() {
		
		// The internal panel has two panes - one for the table, the other for the list
		setLayout(new BorderLayout());
		JPanel internalPanel = new JPanel();
		
		/*
		//Create the internal panel - it has two panes
		internalPanel.setLayout(new MigLayout("ins 2","",""));
		addSeparator(internalPanel,"Properties");
		internalPanel.add(createPropertiesPanel(),"wrap");
		
		if( !buffer.isEmpty() ) {
			addSeparator(internalPanel,"List");
			internalPanel.add(createListPanel(),"wrap");
		}
		add(internalPanel,BorderLayout.CENTER);
		*/

		// The OK button simply closes the dialog
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton("Dismiss");
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
	}

	/**
	 * Obtain the current text of the block
	 */
	private void queryBlock() {
		
	}
	
}
	