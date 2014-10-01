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

import javax.swing.DefaultListModel;
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
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

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
	private final static String TAG = "EnumEditPanel";
	private static final long serialVersionUID = 1L;
	private static final String columnConstraints = "[para]0[]0[]0[]";
	private static final String layoutConstraints = "ins 2";
	private static final String rowConstraints = "";
	private BlockProperty property = null;
	private final JLabel headingLabel;

	private String delimiter = ",";
	private List<String> model;

	public ListEditPanel(final BlockPropertyEditor editor) {
		super(editor);
		model = new ArrayList<>();
		model.add("");   // Put at least one line in the model
		setLayout(new MigLayout("top,flowy,ins 2","",""));
		headingLabel = addHeading(this);
		//Create two panels - value edit display option.
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		addSeparator(editPanel,"List");
		editPanel.add(createDelimiterPanel(delimiter),"skip,wrap");
		editPanel.add(createListTablePanel(model),"skip");
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
		model = BlockProperty.disassembleList(prop.getValue().toString());
		log.infof("%s.updateForProperty: %s (%s)",TAG,prop.getName(),prop.getValue().toString());
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
		String columnConstraints = "[para]0[][20]";
		String layoutConstraints = "ins 2";
		String rowConstraints = "";
		final JPanel panel = new JPanel();
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 2 cells across
		panel.add(createLabel("Delimiter"),"skip");
		panel.add(createTextField(delim),"wrap");
		return panel;
	}
	
	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        valueBox       - text box with the current value (editable)
	 *        add button     - to append this entry 
	 */
	private JPanel createListTablePanel(List<String> model)  {
		String columnConstraints = "para[200]2[25]2[25]";
		String layoutConstraints = "ins 2";
		String rowConstraints = "";
		JPanel tablePanel = new JPanel();
		JTable table = new JTable();
		tablePanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		tablePanel.add(table,"pushx");
		tablePanel.add(createAddButton(),"w :25:,align top");
		tablePanel.add(createDeleteButton(),"w :25:,align top,wrap");
		
		String[] columnNames = { "Value" };
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,1);  // One row
		for( String val:model) {
			String[] row = new String[1];
			row[0] = val;
			dataModel.addRow(row) ;
		}
        table = new JTable(dataModel);
        ListSelectionModel lsm = table.getSelectionModel();
        lsm.addListSelectionListener(new SharedListSelectionHandler());
        JScrollPane tablePane = new JScrollPane(table);
        tablePanel.add(tablePane, "");
		return tablePanel;
	}
	
	/**
	 * Create a button that deletes the nth entry
	 */
	private JButton createAddButton() {
		JButton btn = new JButton();
		final String ICON_PATH  = "Block/icons/editor/add.png";
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
				// Determine the correct panel, depending on the property type
				public void actionPerformed(ActionEvent e){
				}
			});
		}
		else {
			log.warnf("%s.createAddButton icon not found(%s)",TAG,ICON_PATH);
		}
		return btn;
	}
	
	/**
	 * Create a button that deletes the nth entry
	 */
	private JButton createDeleteButton() {
		JButton btn = new JButton();
		final String ICON_PATH  = "Block/icons/editor/delete.png";
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
				// Determine the correct panel, depending on the property type
				public void actionPerformed(ActionEvent e){
				}
			});
		}
		else {
			log.warnf("%s.createDeleteButton icon not found(%s)",TAG,ICON_PATH);
		}
		return btn;
	}
	
	/**
	 * Create a selection listener for both the list and the table.
	 * We allow only one row to be selected at a time. Enable/disable
	 * the add/delete buttons.
	 */
	private class SharedListSelectionHandler implements ListSelectionListener {
        public void valueChanged(ListSelectionEvent e) {
            ListSelectionModel lsm = (ListSelectionModel)e.getSource();
 
            int firstIndex = e.getFirstIndex();
            int lastIndex = e.getLastIndex();
            boolean isAdjusting = e.getValueIsAdjusting();
        }
    }
}
	