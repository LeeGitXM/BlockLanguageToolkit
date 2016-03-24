/**
 *   (c) 2016  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.config;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.BundleUtil;

import net.miginfocom.swing.MigLayout;

/**
 * This is a read-only viewer for blocks that return an
 * explanation for their TRUE or FALSE state.
 */

public class BlockExplanationViewer extends JDialog {
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 4114388376825535527L;
	private static final Dimension PANEL_SIZE  = new Dimension(480,120);
	private final ApplicationRequestHandler handler;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private String explanation = "";
	private JTextArea textArea;
	
	public BlockExplanationViewer(Frame frame,ProcessDiagramView dia,ProcessBlockView view) {
		super(frame);
		this.diagram = dia;
		this.block = view;
		this.handler = new ApplicationRequestHandler();
		String bstate = handler.getBlockState(diagram.getId().toString(), view.getName());
		this.setTitle(String.format(BundleUtil.get().getString(PREFIX+".Explanation.Title",bstate,view.getName())));
		setAlwaysOnTop(true);
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setPreferredSize(PANEL_SIZE);
		initialize();
		refresh();
	}
	
	private void initialize() {
		
		// The internal panel has three panes - one for properties, one for an activity history
		// and the other for any internal buffer
		setLayout(new BorderLayout());
		JPanel internalPanel = new JPanel();
		internalPanel.setLayout(new MigLayout("ins 5","",""));
		textArea = createExplanationPanel(internalPanel);
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


	
	private void refresh() {
		explanation = handler.getExplanation(diagram.getId().toString(), block.getId().toString());
		textArea.setText(explanation);
	}
	
	/**
	 * The panel contains a single text pane with the explanation
	 */
	private JTextArea createExplanationPanel(JPanel outerPanel)  {
		JTextArea area = new JTextArea();
		area.setEditable(true);
		area.setLineWrap(true);
		area.setWrapStyleWord(true);
		
		JScrollPane areaScrollPane = new JScrollPane(area);
		areaScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		areaScrollPane.setPreferredSize(PANEL_SIZE);
        outerPanel.add(areaScrollPane, "wrap");
		return area;
	}

}
	