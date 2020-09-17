/**
 *   (c) 2020  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collections;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableModel;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;

import net.miginfocom.swing.MigLayout;

/**
 * We call it a "Source" panel because its purpose is to
 * configure a SourceConnection block. In fact it lists
 * SinkConnections which are available to be connected.
 * The list text consists of comma-separated strings. 
 */

public class SourceEditPanel extends BasicEditPanel {
	// A panel is designed to edit properties that are lists of strings.
	private final static String TAG = "SourceEditPanel";
	private static final long serialVersionUID = 1L;
	//private final JLabel headingLabel;
	private final List<SerializableBlockStateDescriptor> sinks;
	private JTable table;

	public SourceEditPanel(final BlockPropertyEditor editor) {
		super(editor);
		sinks = editor.getRequestHandler().listBlocksOfClass(BlockConstants.BLOCK_CLASS_SINK);
		Collections.sort(sinks);
		setLayout(new BorderLayout());
		//Create the edit panel - it has two panes
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new MigLayout("ins 2","",""));
		//headingLabel = addHeading(editPanel);
		addSeparator(editPanel,"Sink Names");
		editPanel.add(createTablePanel(),"wrap");
		add(editPanel,BorderLayout.CENTER);

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel();
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Coerce to the correct data type
				ApplicationRequestHandler handler = editor.getRequestHandler();
				int selectedRow = table.getSelectedRow();
				if( selectedRow>=0 ) {
					SerializableBlockStateDescriptor sinkDescriptor = sinks.get(selectedRow);
					ProcessBlockView block = editor.getBlock();
					BlockProperty tagProperty = block.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
					SerializableResourceDescriptor diag = handler.getDiagramForBlock(sinkDescriptor.getIdString());
					Object val = handler.getPropertyValue(diag.getId(), sinkDescriptor.getIdString(), BlockConstants.BLOCK_PROPERTY_TAG_PATH);
					Object binding = handler.getPropertyBinding(diag.getId(), sinkDescriptor.getIdString(), BlockConstants.BLOCK_PROPERTY_TAG_PATH);
					tagProperty.setValue(val);
					tagProperty.setBinding(binding.toString());
					block.setName(sinkDescriptor.getName());
					editor.updateCorePanel(BlockEditConstants.HOME_PANEL,block); // Core attributes
					editor.updatePanelForProperty(BlockEditConstants.HOME_PANEL, tagProperty);
					editor.updatePanelValue(SourceMainPanel.PROP_NAME, sinkDescriptor.getName());
					editor.saveDiagramClean() ;   // Immediately update the running diagram
				}
				else {
					log.warnf("%s.OK action: property is NULL, no action taken",TAG);
				}
				setSelectedPane(BlockEditConstants.HOME_PANEL);
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton,"");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setSelectedPane(BlockEditConstants.HOME_PANEL);
			}			
		});
		add(buttonPanel,BorderLayout.SOUTH);
	}

	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createTablePanel()  {
		JPanel outerPanel = new JPanel();
		table = new JTable();	
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","para[:240:]","[:160:]5[:30:]"));
		String[] columnNames = { "Sinks" };
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0);  // No null rows
		for( SerializableBlockStateDescriptor block:sinks ) {
			String [] row = new String[1];
			row[0] = block.getName();
			dataModel.addRow(row);
		}
		table = new JTable(dataModel);
        table.setPreferredSize(BlockEditConstants.LIST_SIZE);
        table.setRowSelectionAllowed(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        JScrollPane tablePane = new JScrollPane(table);
        table.setFillsViewportHeight(true);
        outerPanel.add(tablePane, "push,wrap");
		return outerPanel;
	}

}
	