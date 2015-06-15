/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
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
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ModuleRequestHandler;
import com.ils.blt.common.BLTProperties;
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
 * This is a read-only viewer for blocks for blocks that return internal state
 * (theoretically all of them). 
 */

public class BlockInternalsViewer extends JDialog {
	private static String TAG = "BlockInternalsViewer";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 4004388376825535527L;
	private final int DIALOG_HEIGHT = 320;
	private final int DIALOG_WIDTH = 500;
	private static SimpleDateFormat dateFormatter = new SimpleDateFormat(BlockConstants.TIMESTAMP_FORMAT);
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private Map<String,String> attributes = null;
	private List<Map<String,String>> buffer = null;
	private JTable table = null;
	JPanel internalPanel = null;
	
	public BlockInternalsViewer(Frame frame,ProcessDiagramView dia,ProcessBlockView view) {
		super(frame);
		this.diagram = dia;
		this.block = view;
		this.setTitle(String.format(BundleUtil.get().getString(PREFIX+".ViewInternals.Title",view.getName())));
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
		
		// The internal panel has two panes - one for the table, the other for the list
		setLayout(new BorderLayout());
		internalPanel = new JPanel();
		internalPanel.setLayout(new MigLayout("ins 2","",""));
		//Create the internal panel - it has two panes
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
		ModuleRequestHandler handler = new ModuleRequestHandler();
		SerializableBlockStateDescriptor descriptor = handler.getInternalState(diagram.getId().toString(), block.getId().toString());
		if( descriptor!=null ) {
			attributes = descriptor.getAttributes();
			buffer = descriptor.getBuffer();
			log.infof("%s.queryBlock %s: %d properties, %d history entries",TAG, block.getName(),attributes.size(),buffer.size());
		}
		else {
			log.infof("%s.queryBlock %s: no information returned",TAG,block.getName());
		}
	}
	/**
	 * Update the UI per most current information from the block
	 */
	private void updateInformation() {
		internalPanel.removeAll();
		addSeparator(internalPanel,"Properties");
		internalPanel.add(createPropertiesPanel(),"wrap");
		
		if( !buffer.isEmpty() ) {
			addSeparator(internalPanel,"List");
			internalPanel.add(createListPanel(),"wrap");
		}
		internalPanel.revalidate();
		internalPanel.repaint();
	}
	
	private void refresh() {
		queryBlock();
		updateInformation();
	}
	
	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createPropertiesPanel()  {
		JPanel outerPanel = new JPanel();
		table = new JTable();		
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","",""));
		String PRE = PREFIX+".ViewInternals.Col.";
		String[] columnNames = { BundleUtil.get().getString(PRE+"Name"),
				                 BundleUtil.get().getString(PRE+"Value") };
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0);
		// There should always be attributes
		if( attributes!=null )  {
			String [] row = new String[2];
			for( String key:attributes.keySet()) {
				row[0] = key;
				String attribute = attributes.get(key);
				row[1] = (attribute==null?" ":attribute);
				dataModel.addRow(row);
			}
		}
		
		for( ProcessAnchorDescriptor pad: block.getAnchors() ) {
			if(pad.getType().equals(AnchorType.Origin)) {
				QualifiedValue qv = pad.getLastValue();
				String [] row = new String[2];
				row[0] = "port: "+ pad.getDisplay();
				String text = "";
				if( qv!=null ) text = String.format("%s  %s  %s", 
						(qv.getValue()==null?"null":qv.getValue().toString()),
						(qv.getQuality() ==null?"null":qv.getQuality().toString()),
						                   dateFormatter.format(qv.getTimestamp()));
				row[1] = (qv==null?"":qv.getValue().toString());
				dataModel.addRow(row);
			}
		}
		
        table = new JTable(dataModel);
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
	