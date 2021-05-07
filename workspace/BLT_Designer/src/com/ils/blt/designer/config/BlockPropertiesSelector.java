/**
 *   (c) 2014  ILS Automation. All rights reserved.
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
import java.util.Collection;
import java.util.UUID;

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

import com.ils.block.AbstractProcessBlock;
import com.ils.block.BlockPropertyDisplay;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;

import net.miginfocom.swing.MigLayout;

/**
 * This is a properties list viewer to allow users to select what block
 * properties to display on the workspace
 */

public class BlockPropertiesSelector extends JDialog implements TableModelListener {
	private static String TAG = "BlockPropertiesSelector";
	private final LoggerEx log;
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX; // Required for text strings
	private static final long serialVersionUID = 4004388376825535527L;
	private final int DIALOG_HEIGHT = 400;
	private final int DIALOG_WIDTH = 600;
	private final int TABLE_HEIGHT = 200;
	private final int TABLE_WIDTH = 800;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
//	private Map<String, BlockProperty> blockProperties = null;
	private Collection<BlockProperty> blockProperties = null;
	private JTable table = null;
	JPanel internalPanel = null;
	private final DiagramWorkspace workspace;

	public BlockPropertiesSelector(Frame frame, ProcessDiagramView dia, ProcessBlockView view, DiagramWorkspace wkspc) {
		super(frame);
		this.workspace = wkspc;
		this.diagram = dia;
		this.block = view;
		this.setTitle(String.format(BundleUtil.get().getString(PREFIX + ".ViewInternals.Title", view.getName())));
		setAlwaysOnTop(true);
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setPreferredSize(new Dimension(DIALOG_WIDTH, DIALOG_HEIGHT));
		initialize();
		queryBlock();
		updateInformation();
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

	private void queryBlock() {

		blockProperties = block.getProperties();
//		ApplicationRequestHandler handler = new ApplicationRequestHandler();
//		SerializableBlockStateDescriptor descriptor = handler.getInternalState(diagram.getId().toString(), block.getId().toString());
//		if( descriptor!=null ) {
//			blockProperties = descriptor.getProperties();
//			log.infof("%s.queryBlock %s: %d properties",TAG, block.getName(),blockProperties.size());
//		}
//		else {
//			log.infof("%s.queryBlock %s: no information returned",TAG,block.getName());
//		}
	}

	/**
	 * Update the UI per most current information from the block
	 */
	private void updateInformation() {
		internalPanel.removeAll();
		addSeparator(internalPanel, "Properties");
		internalPanel.add(createPropertiesPanel(), "wrap");

		internalPanel.revalidate();
		internalPanel.repaint();
	}

	/*
	 * This is called then they check or uncheck a row in the table.
	 */
	public void tableChanged(TableModelEvent e) {
		if (e.getType() == TableModelEvent.UPDATE) {
			int row = e.getFirstRow();
			int column = e.getColumn();

			if (column == 0) {
				TableModel model = (TableModel) e.getSource();
				boolean newValue = ((Boolean) model.getValueAt(row, column)).booleanValue();

				String propName = (String) model.getValueAt(row, 1);
//	    		for( String pad: blockProperties.keySet() ) {  // property names
//	    			BlockProperty prop = blockProperties.get(pad);
				for (BlockProperty prop : blockProperties) { // properties
					if (prop.getName().equalsIgnoreCase(propName)) { // found name of changed property
						if (newValue != prop.isShowProperty()) { // make sure it's a real change
							if (newValue == true) {

								AbstractProcessBlock apBlock = new BlockPropertyDisplay();
								ProcessBlockView newBlock = new ProcessBlockView(
										apBlock.getBlockPrototype().getBlockDescriptor());
								Point parentLoc = block.getLocation();
								Point loc = new Point((int) parentLoc.getX() + 5, (int) parentLoc.getY() + 55);

//    							newBlock..addSignalConnection(listener);

								newBlock.setLocation(loc);
								diagram.addBlock(newBlock);
//    							Collection<BlockProperty> blocks = newBlock.getProperties();
								for (BlockProperty bp : newBlock.getProperties()) {
									if (bp.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TEXT)) {
										bp.setValue(prop.getValue().toString());
									}
									if (bp.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_PREFIX)) {
										bp.setValue(prop.getName());
									}
								}

//	    						connect it to the current block
								prop.setShowProperty(true);
								prop.setDisplayedBlockUUID(newBlock.getId().toString());
								// diagram.

//   								ApplicationRequestHandler handler = new ApplicationRequestHandler();
//   								handler.setBlockProperties(diagram.getId(), block.getId(), blockProperties.values());

								// It's not getting to the gateway

								newBlock.setDirty(true); // These didn't help
								block.setDirty(true);

							} else {
								// The designer UN-checked a box so there must be a readout so delete it
								String linkedIdStr = prop.getDisplayedBlockUUID();
								UUID linkedId = UUID.fromString(linkedIdStr);
								prop.setShowProperty(false);
								prop.setDisplayedBlockUUID("");

//								now delete the linked block
								Block foundBlock = diagram.getBlock(linkedId);
								diagram.deleteBlock(foundBlock);

							}
							diagram.setDirty(true);
						}
					}
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

//		for( String pad: blockProperties.keySet() ) {
		for (BlockProperty prop : blockProperties) {
			if (prop.isEditable()) {
				Object[] row = new Object[3];
				row[0] = new Boolean(prop.isShowProperty());
				row[1] = prop.getName();
				//String text = "" + prop.getValue();
				//row[2] = text;
				dataModel.addRow(row);
			}
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

}
