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
import java.awt.Point;
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
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.AttributeDisplayDescriptor;
import com.ils.blt.designer.workspace.BlockAttributeView;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;

import net.miginfocom.swing.MigLayout;

/**
 * This is a properties list viewer to allow users to select what block
 * properties to display on the workspace. The attribute "Name" is not
 * strictly a property of the block but is treated as one.
 */

public class AttributeDisplaySelector extends JDialog implements TableModelListener {
	private static String CLSS = "AttributeDisplaySelector";
	private final LoggerEx log;
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
		this.log = LogUtil.getLogger(getClass().getPackageName());
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
				boolean newValue = ((Boolean) model.getValueAt(row, column)).booleanValue();
				
				BlockAttributeView pad = findDisplay(diagram,block,propName);
				// CASE I - checked box, display does not exist. Create it.
				// Add to diagram
				if ( newValue && (pad==null) ) {
					pad = new BlockAttributeView(new AttributeDisplayDescriptor());
					pad.setDiagram(diagram);
					pad.setReferenceBlock(block);
					pad.setPropertyName(propName);
					pad.setLocation(new Point(100,40));
					diagram.addBlock(pad);
					diagram.setDirty(true);
				}
				// CASE II - checked box, but display already exists. Do nothing, just use it.
				else if(newValue && (pad!=null) ) {
					diagram.setDirty(false);
				}
				// CASE III - unchecked box and there is display. Delete it.
				else if ( !newValue && (pad!=null)) {
					diagram.deleteBlock(pad);
				}
				// CASE IV - unchecked box and there is no display. Do nothing.
				else  {
				}
				SwingUtilities.invokeLater(new WorkspaceRepainter());
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
		
		// First add "name". It is always present as a member of the block, not property
		ProcessBlockView display = findDisplay(diagram,block,"Name");
		Object[] row = new Object[2];
		row[0] = (display!=null);
		row[1] = "Name";
		dataModel.addRow(row);

		// For each block property, see if there is a display stored in the diagram.
		for (BlockProperty prop : block.getProperties()) {
			display = findDisplay(diagram,block,prop.getName());
			row = new Object[2];
			row[0] = (display!=null);
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
	 * The "display" is a BlockAttributeView block with the indicated reference block and property.
	 * @return the attribute display for the given block and property
	 */
	private BlockAttributeView findDisplay(ProcessDiagramView dia, ProcessBlockView refBlock,String propName) {
		BlockAttributeView display = null;
		for(Block block:dia.getBlocks()) {
			if( block instanceof BlockAttributeView ) {
				BlockAttributeView bav = (BlockAttributeView)block;
				BlockProperty viewProp = bav.getProperty(BlockConstants.BLOCK_PROPERTY_PROPERTY);
				if( viewProp!=null && propName.equalsIgnoreCase(viewProp.getValue().toString()) ) {
					BlockProperty idProp =  bav.getProperty(BlockConstants.ATTRIBUTE_DISPLAY_BLOCK_ID);
					if( idProp!=null && bav.getId().equals(refBlock.getId())) {
						display = bav;
					}
				}
			}
		}
		return display;
	}
}
