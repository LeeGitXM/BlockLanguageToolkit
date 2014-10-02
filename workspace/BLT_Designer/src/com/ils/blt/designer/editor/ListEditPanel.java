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
	private final static String TAG = "EnumEditPanel";
	private static final long serialVersionUID = 1L;
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
		//Create the edit panel - it has two panes
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new MigLayout("ins 2","",""));
		addSeparator(editPanel,"List");
		editPanel.add(createDelimiterPanel(delimiter),"wrap");
		editPanel.add(createListTablePanel(model),"wrap");
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
		final JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 2, fillx","[]10[20]",""));     // 2 cells across
		panel.add(createLabel("Delimiter"),"");
		panel.add(createTextField(delim),"wrap");
		return panel;
	}
	
	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createListTablePanel(List<String> model)  {
		JPanel outerPanel = new JPanel();
		JTable table = new JTable();
		//outerPanel.setLayout(new MigLayout("ins 2,filly","para[:240:]","[80]10[40]"));		
		outerPanel.setLayout(new MigLayout("ins 2,filly","para[:240:]",""));
		String[] columnNames = { "Values" };
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,1);  // One row
		for( String val:model) {
			String[] row = new String[1];
			row[0] = val;
			dataModel.addRow(row) ;
		}
        table = new JTable(dataModel);
        table.setPreferredSize(TABLE_SIZE);
        ListSelectionModel lsm = table.getSelectionModel();
        lsm.addListSelectionListener(new SharedListSelectionHandler());
        JScrollPane tablePane = new JScrollPane(table);
        table.setFillsViewportHeight(true);
        outerPanel.add(tablePane, "wrap");
        JPanel buttonPanel = new JPanel(new MigLayout("ins 2,fillx","20[:25:]10[:25:]","[:50:]"));
        buttonPanel.add(createAddButton(),"");
        buttonPanel.add(createDeleteButton(),"");
        outerPanel.add(buttonPanel,"wrap");
		return outerPanel;
	}
	
	/**
	 * Create a button that deletes the nth entry
	 */
	private JButton createAddButton() {
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
					// Determine the correct panel, depending on the property type
					public void actionPerformed(ActionEvent e){
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
	private JButton createDeleteButton() {
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
					// Determine the correct panel, depending on the property type
					public void actionPerformed(ActionEvent e){
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
	private class SharedListSelectionHandler implements ListSelectionListener {
        public void valueChanged(ListSelectionEvent e) {
            ListSelectionModel lsm = (ListSelectionModel)e.getSource();
 
            int firstIndex = e.getFirstIndex();
            int lastIndex = e.getLastIndex();
            boolean isAdjusting = e.getValueIsAdjusting();
        }
    }
}
	