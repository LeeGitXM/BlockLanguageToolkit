/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer.editor;

import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.block.BlockProperty;
import com.inductiveautomation.ignition.client.images.ImageLoader;

/**
 * Display a panel to edit lists of strings using a list box.
 * This is one of the sliding panels in the block editor.
 * The first character in the string is the delimiter. We use
 * it to separate the other items in the list.
 * 
 *  There is no annotation for a list.
 */

public class ListEditPanel extends BasicEditPanel {
	// A panel is designed to edit properties that are lists of strings.
	private final static String TAG = "ListEditPanel";
	private static final long serialVersionUID = 1L;
	private static final String DEFAULT_DELIMITER = ",";
	private BlockProperty property = null;
	private final JLabel headingLabel;
	private JButton addButton;      // Click to add a row
	private JButton deleteButton;   // Click to delete a row

	private JTextField delimiterField;
	private JTable table;

	public ListEditPanel(final BlockPropertyEditor editor) {
		super(editor);
		setLayout(new MigLayout("top,flowy,ins 2","",""));
		headingLabel = addHeading(this);
		//Create the edit panel - it has two panes
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new MigLayout("ins 2","",""));
		addSeparator(editPanel,"List");
		editPanel.add(createDelimiterPanel(DEFAULT_DELIMITER),"wrap");
		editPanel.add(createTablePanel(),"wrap");
		add(editPanel,"");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, "dock south");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(property!=null) {
					// Coerce to the correct data type
					String delimiter = delimiterField.getText();
					if( delimiter.length()<1 ) delimiter = DEFAULT_DELIMITER;
					if( delimiter.length()>1 ) delimiter = delimiter.substring(0, 1);
					DefaultTableModel dtm = (DefaultTableModel)table.getModel();
					List<String> model = new ArrayList<>();
					int rows = dtm.getRowCount();
					int row = 0;
					while( row<rows ) {
						model.add((String) dtm.getValueAt(row, 0));
						row++;
					}
					String list = BlockProperty.assembleList(model,delimiter);
					property.setValue(list);
				}
				editor.notifyOfChange();   // Handle "dirtiness" and repaint diagram
				updatePanelForProperty(BlockEditConstants.HOME_PANEL,property);
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
	}

	public void updateForProperty(BlockProperty prop) {
		String val = prop.getValue().toString();
		if( val.length()<2) return;
		log.infof("%s.updateForProperty: %s (%s)",TAG,prop.getName(),val);
		// Delimiter is the first character 
		String delimiter = val.substring(0, 1);
		delimiterField.setText(delimiter);
		List<String> model = BlockProperty.disassembleList(val);
		DefaultTableModel dtm = (DefaultTableModel)table.getModel();
		dtm.setRowCount(0);
		for( String entry:model) {
			String[] row = new String[1];
			row[0] = entry;
			dtm.addRow(row) ;
		}
		this.property = prop;
		headingLabel.setText(prop.getName());	 
	}

	/**
	 * Create a text field for data entry
	 */
	protected JTextField createTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(ENTRY_BOX_SIZE);
		field.setEditable(true);
		return field;
	}
	
	/**
	 * Create a panel for entry/display of the delimiter
	 */
	protected JPanel createDelimiterPanel(String delim) {
		final JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 2, fillx","[]10[20]",""));     // 2 cells across
		panel.add(createLabel("Delimiter"),"");
		delimiterField = createTextField(delim);
		panel.add(delimiterField,"wrap");
		return panel;
	}
	
	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createTablePanel()  {
		JPanel outerPanel = new JPanel();
		table = new JTable();		
		outerPanel.setLayout(new MigLayout("ins 2,filly","para[:300:]","[120]5[]"));
		String[] columnNames = { "Values" };
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0);  // No null rows
        table = new JTable(dataModel);
        table.setPreferredSize(TABLE_SIZE);
        table.setRowSelectionAllowed(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        JScrollPane tablePane = new JScrollPane(table);
        table.setFillsViewportHeight(true);
        outerPanel.add(tablePane, "wrap");
        JPanel buttonPanel = new JPanel(new MigLayout("ins 2,fillx","[:25:]2[:25:]","[:30:]"));
        addButton = createAddButton(table);
        buttonPanel.add(addButton,"");
        deleteButton = createDeleteButton(table);
        buttonPanel.add(deleteButton,"");
        outerPanel.add(buttonPanel,"wrap");
        ListSelectionModel lsm = table.getSelectionModel();
        lsm.addListSelectionListener(new SelectionHandler(table,deleteButton));
		return outerPanel;
	}
	
	/**
	 * Create a button that deletes the nth entry
	 */
	private JButton createAddButton(final JTable tbl) {
		JButton btn = new JButton();
		final String ICON_PATH  = "Block/icons/editor/add.png";
		try {
			Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BUTTON_SIZE);
			if( img !=null) {
				Icon icon = new ImageIcon(img);
				btn.setIcon(icon);
				btn.setMargin(new Insets(0,0,0,0));
				btn.setOpaque(false);
				btn.setBorderPainted(false);
				btn.setBackground(getBackground());
				btn.setBorder(null);
				btn.setPreferredSize(BUTTON_SIZE);
				btn.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e){
						DefaultTableModel dtm = (DefaultTableModel)tbl.getModel();
						String[] row = new String[1];
						row[0] = "";   // Add an empty row
						dtm.addRow(row);
					}
				});
			}
			else {
				log.warnf("%s.createAddButton icon not found(%s)",TAG,ICON_PATH);
			}
		}
		catch(Exception ex) {
			log.warnf("%s.createDeleteButton icon not found(%s) (%s)",TAG,ICON_PATH, ex.getMessage());
		}
		return btn;
	}
	
	/**
	 * Create a button that deletes the nth entry
	 */
	private JButton createDeleteButton(final JTable tbl) {
		JButton btn = new JButton();
		final String ICON_PATH  = "Block/icons/editor/delete.png";
		try {
			Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BUTTON_SIZE);
			if( img !=null) {
				Icon icon = new ImageIcon(img);
				btn.setIcon(icon);
				btn.setMargin(new Insets(0,0,0,0));
				btn.setOpaque(false);
				btn.setBorderPainted(false);
				btn.setBackground(getBackground());
				btn.setBorder(null);
				btn.setPreferredSize(BUTTON_SIZE);
				btn.addActionListener(new ActionListener() {
					// We are guaranteed that the selection interval is contiguous
					public void actionPerformed(ActionEvent e){
						int[] selected = tbl.getSelectedRows();
						if( selected.length < 1 ) return;
						int minIndex = tbl.getRowCount()+1;
						int maxIndex = -1;
						for( int i:selected ) {
							selected[i] = tbl.convertRowIndexToModel(i);
							if( selected[i] > maxIndex ) maxIndex = selected[i];
							if( selected[i] < minIndex ) minIndex = selected[i];
						}
						int row = maxIndex;
						DefaultTableModel dtm = (DefaultTableModel)tbl.getModel();
						while(row>=minIndex ) {
							dtm.removeRow(row);
							row--;
						}
					}
				});
			}
			else {
				log.warnf("%s.createDeleteButton icon not found(%s)",TAG,ICON_PATH);
			}
		}
		catch(Exception ex) {
			log.warnf("%s.createDeleteButton icon not found(%s) (%s)",TAG,ICON_PATH, ex.getMessage());
		}
		return btn;
	}
	
	/**
	 * Create a selection listener for both the list and the table.
	 * We allow only one row to be selected at a time. Enable/disable
	 * the add/delete buttons.
	 */
	private class SelectionHandler implements ListSelectionListener {
		private final JTable table;
		private final JButton delBtn;

		SelectionHandler(JTable tbl,JButton rowDeleter ) {
			this.table = tbl;
			this.delBtn = rowDeleter;
		}
		
		public void valueChanged(ListSelectionEvent e) {
			if (!e.getValueIsAdjusting()) {
				if (e.getSource() == table.getSelectionModel()) {
					ListSelectionModel lsm = table.getSelectionModel();
					delBtn.setEnabled(!lsm.isSelectionEmpty());
				} 
			}
		}
	}
}
	