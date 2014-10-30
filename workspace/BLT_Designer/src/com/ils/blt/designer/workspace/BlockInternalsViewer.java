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
import java.util.ArrayList;
import java.util.HashMap;
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

public class BlockInternalsViewer extends JDialog {
	private static String TAG = "BlockInternalsViewer";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 2002388376824434427L;
	private final int DIALOG_HEIGHT = 320;
	private final int DIALOG_WIDTH = 500;
	private static final Dimension TABLE_SIZE  = new Dimension(480,120);
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private Map<String,String> attributes = null;
	private List<Map<String,String>> buffer = null;
	private JTable table;
	
	public BlockInternalsViewer(ProcessDiagramView dia,ProcessBlockView view) {
		super();
		this.diagram = dia;
		this.block = view;
		this.setTitle(BundleUtil.get().getString(PREFIX+".ViewInternals.Title"));
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
		
		//Create the internal panel - it has two panes
		internalPanel.setLayout(new MigLayout("ins 2","",""));
		addSeparator(internalPanel,"Properties");
		internalPanel.add(createPropertiesPanel(),"wrap");
		
		if( !buffer.isEmpty() ) {
			addSeparator(internalPanel,"List");
			internalPanel.add(createListPanel(),"wrap");
		}
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
	}

	private void queryBlock() {
		ApplicationRequestHandler handler = new ApplicationRequestHandler();
		SerializableBlockStateDescriptor descriptor = handler.getInternalState(diagram.getId().toString(), block.getId().toString());
		if( descriptor!=null ) {
			attributes = descriptor.getAttributes();
			buffer = descriptor.getBuffer();
			log.infof("%s.queryBlock: %d properties, %d history entries",TAG, attributes.size(),buffer.size());
		}
	}
	
	
	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createPropertiesPanel()  {
		JPanel outerPanel = new JPanel();
		table = new JTable();		
		//outerPanel.setLayout(new MigLayout("ins 2,filly","para[:480:]","[120]"));
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","",""));
		String PRE = PREFIX+".ViewInternals.Col.";
		String[] columnNames = { BundleUtil.get().getString(PRE+"Name"),
				                 BundleUtil.get().getString(PRE+"Value") };
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0); 
		String [] row = new String[2];
		for( String key:attributes.keySet()) {
			row[0] = key;
			row[1] = attributes.get(key);
			dataModel.addRow(row);
		}
        table = new JTable(dataModel);
        table.setPreferredSize(TABLE_SIZE);
        table.setRowSelectionAllowed(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);

        
        JScrollPane tablePane = new JScrollPane(table);
        table.setFillsViewportHeight(true);
        outerPanel.add(tablePane, "wrap");
		return outerPanel;
	}
	
	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createListPanel()  {
		JPanel outerPanel = new JPanel();
		table = new JTable();
		Map<String,String> prototype = buffer.get(0);
		String[] columnNames = prototype.keySet().toArray(new String[prototype.keySet().size()]);
		int nColumns = columnNames.length;
		//outerPanel.setLayout(new MigLayout("ins 2,filly","para[:480:]","[120]"));
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","",""));
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0); 
		for( Map<String,String> entity:buffer) {
			String[] row = entity.values().toArray(new String[nColumns]);
			dataModel.addRow(row);
		}
        table = new JTable(dataModel);
        table.setPreferredSize(TABLE_SIZE);
        table.setRowSelectionAllowed(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);

        
        JScrollPane tablePane = new JScrollPane(table);
        table.setFillsViewportHeight(true);
        outerPanel.add(tablePane, "wrap");
		return outerPanel;
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
	