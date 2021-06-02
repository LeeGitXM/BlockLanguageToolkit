/**
 *   (c) 2014-2021  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 *   
 *   This class implements a swing dialog that provides a table of all of the properties of the originating block.
 *   The first column is a check box whose state is set based on whether or not there is already a corresponding readout for the property;
 *   i.e., was one previously created.  Checking or unchecking the checkbox automatically creates or deletes the readout.
 */
package com.ils.blt.designer.config;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.AttributeDisplayView;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.BundleUtil;

import net.miginfocom.swing.MigLayout;

/**
 * This is a properties list viewer to allow users to select what block
 * properties to display on the workspace
 */

public class AttributeDisplaySelector extends JDialog implements TableModelListener {
	private static String CLSS = "AttributeDisplaySelector";
	private final ILSLogger log;
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX; // Required for text strings
	private static final long serialVersionUID = 4004388376825535527L;
	private final int DIALOG_HEIGHT = 400;
	private final int DIALOG_WIDTH = 600;
	private final int TABLE_HEIGHT = 200;
	private final int TABLE_WIDTH = 800;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private JTable table = null;
	JPanel internalPanel = null;
	private final DiagramWorkspace workspace;

	public AttributeDisplaySelector(Frame frame, ProcessDiagramView dia, ProcessBlockView view, DiagramWorkspace wkspc) {
		super(frame);
		this.workspace = wkspc;
		this.diagram = dia;
		this.block = view;
		this.setTitle(String.format(BundleUtil.get().getString(PREFIX + ".ViewInternals.Title", view.getName())));
		setAlwaysOnTop(true);
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.log = LogMaker.getLogger(this);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH, DIALOG_HEIGHT));
		initialize();
		setUI();
	}

	private void initialize() {

		setLayout(new BorderLayout());
		internalPanel = new JPanel();
		internalPanel.setLayout(new MigLayout("ins 2,fillx", "", ""));
		// Create the internal panel - it has two panes
		add(internalPanel, BorderLayout.CENTER);

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
	 * Update the UI per most current information from the block
	 */
	private void setUI() {
		internalPanel.removeAll();
		addSeparator(internalPanel, "Properties");
		internalPanel.add(createPropertiesPanel(), "wrap");

		internalPanel.revalidate();
		internalPanel.repaint();
	}

	/*
	 * This is called then they check or uncheck a row in the table.
	 * As a result of that we create or delete a readout.
	 */
	public void tableChanged(TableModelEvent e) {
		if (e.getType() == TableModelEvent.UPDATE) {
			int row = e.getFirstRow();
			int column = e.getColumn();

			if (column == 0) {
				TableModel model = (TableModel) e.getSource();
				String propName = (String) model.getValueAt(row, 1);
				BlockProperty property = block.getProperty(propName);
				boolean newValue = ((Boolean) model.getValueAt(row, column)).booleanValue();
				
				AttributeDisplayView pad = findDisplay(diagram,block,property);
				// CASE I - checked box, display does not exist. Create it.
				if ( newValue && (pad==null) ) {
					pad = new AttributeDisplayView(block,property.getName());
					// TODO: Add display to workspace
					diagram.setDirty(true);
				}
				// CASE II - checked box, but display already exists. Do nothing, just use it.
				else if(newValue && (pad!=null) ) {
					diagram.setDirty(false);
				}
				// CASE III - unchecked box and there is display. Delete it.
				else if ( !newValue && (pad!=null)) {
				}
				// CASE IV - unchecked box and there is no display. Do nothing.
				else  {
				}
			}
		}
	}

	/**
	 * A list add panel is a panel appending a string element in the list. It
	 * contains:- Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createPropertiesPanel() {
		JPanel outerPanel = new JPanel();
		table = new JTable();
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly", "", ""));
		String PRE = PREFIX + ".ViewInternals.Col.";
		//String[] columnNames = { "Shown", BundleUtil.get().getString(PRE + "Name"), BundleUtil.get().getString(PRE + "Value") };
		String[] columnNames = { "Shown", BundleUtil.get().getString(PRE + "Name") };
		DefaultTableModel dataModel = new DefaultTableModel(columnNames, 0) {
			private static final long serialVersionUID = 1L;

			public Class<?> getColumnClass(int colIndex) {
				return getValueAt(0, colIndex).getClass();
			}
		};
		dataModel.addTableModelListener(this);

		// For each block property, see if there is a display stored in the diagram.
		for (BlockProperty prop : block.getProperties()) {
			AttributeDisplayView display = findDisplay(diagram,block,prop);
			Object[] row = new Object[2];
			row[0] = new Boolean(display!=null);
			row[1] = prop.getName();
			dataModel.addRow(row);
		}

		table = new JTable(dataModel);
		table.setRowSelectionAllowed(true);
		table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
		table.setPreferredScrollableViewportSize(new Dimension(TABLE_WIDTH, TABLE_HEIGHT));

		JScrollPane tablePane = new JScrollPane(table);
		table.setFillsViewportHeight(true);
		outerPanel.add(tablePane, "pushx,wrap");
		return outerPanel;
	}

	/**
	 * Add a separator to a panel using Mig layout
	 */
	private JLabel addSeparator(JPanel panel, String text) {
		JSeparator separator = new JSeparator();
		JLabel label = new JLabel(text);
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		panel.add(label, "split 2,span");
		panel.add(separator, "growx,wrap");
		return label;
	}

	/**
	 * 
	 * @param blockId
	 * @param name
	 * @return the attribute display for the given block and property
	 */
	private AttributeDisplayView findDisplay(ProcessDiagramView dia, ProcessBlockView blk,BlockProperty prop) {
		AttributeDisplayView display = null;
		for(AttributeDisplayView pad:dia.getAttributeDisplays()) {
			if( pad.getBlock().getId().equals(blk.getId()) &&
				pad.getPropertyName().equals(prop.getName()) ) {
				display = pad;
				break;
			}
		}
		return display;
	}
}
