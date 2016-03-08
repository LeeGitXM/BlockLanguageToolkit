/**
 *   (c) 2016  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.config;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;

/**
 * This is a read-only viewer for blocks that return an
 * explanation for their TRUE or FALSE state.
 */

public class BlockExplanationViewer extends JDialog {
	private static String TAG = "BlockExplanationViewer";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 4114388376825535527L;
	private final int DIALOG_HEIGHT = 400;
	private final int DIALOG_WIDTH = 500;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private String explanation = "";
	JPanel internalPanel = null;
	
	public BlockExplanationViewer(Frame frame,ProcessDiagramView dia,ProcessBlockView view) {
		super(frame);
		this.diagram = dia;
		this.block = view;
		this.setTitle(String.format(BundleUtil.get().getString(PREFIX+".Explanation.Title",view.getName())));
		setAlwaysOnTop(true);
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		initialize();
		queryBlock();
		updateInformation();
	}
	
	private void initialize() {
		
		// The internal panel has three panes - one for properties, one for an activity history
		// and the other for any internal buffer
		setLayout(new BorderLayout());
		internalPanel = new JPanel();
		internalPanel.setLayout(new MigLayout("ins 2","",""));
		//Create the internal panel - it has a single pane
		add(internalPanel,BorderLayout.CENTER);
		

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
		// The Refresh button acquires more data
		JButton refreshButton = new JButton("Refresh");
		buttonPanel.add(refreshButton, "");
		refreshButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				refresh();
			}
		});
	}

	private void queryBlock() {
		ApplicationRequestHandler handler = new ApplicationRequestHandler();
		explanation = handler.getExplanation(diagram.getId().toString(), block.getId().toString());
	}
	/**
	 * Update the UI per most current information from the block
	 */
	private void updateInformation() {
		internalPanel.removeAll();
		internalPanel.add(createExplanationPanel(),"wrap");
		internalPanel.revalidate();
		internalPanel.repaint();
	}
	
	private void refresh() {
		queryBlock();
		updateInformation();
	}
	
	/**
	 * The panel contains a single text pane with the explanation
	 */
	private JPanel createExplanationPanel()  {
		JPanel outerPanel = new JPanel();
		JTextPane pane = new JTextPane();		
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","",""));

        pane.setText(explanation);

        JScrollPane tablePane = new JScrollPane(pane);
        outerPanel.add(tablePane, "wrap");
		return outerPanel;
	}

}
	