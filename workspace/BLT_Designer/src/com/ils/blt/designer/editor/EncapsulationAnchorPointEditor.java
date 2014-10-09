/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer.editor;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * This is an editor for encapsulation blocks. We allow the user to define anchor points. 
 */

public class EncapsulationAnchorPointEditor extends JDialog {
	private static String TAG = "EncapsulationAnchorPointEditor";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 2772399376824434427L;
	private static final Dimension BUTTON_SIZE = new Dimension(16,16);
	private final int DIALOG_HEIGHT = 380;
	private final int DIALOG_WIDTH = 440;
	private static final Dimension TABLE_SIZE  = new Dimension(300,120);
	private final ProcessBlockView block;
	private JButton addButton;      // Click to add a row
	private JButton deleteButton;   // Click to delete a row

	private JTable table;

	public EncapsulationAnchorPointEditor(ProcessBlockView view) {
		super();
		this.block = view;
		this.setTitle(BundleUtil.get().getString(PREFIX+".Encapsulation.Title"));
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
	}
	
	private void initialize() {
		setLayout(new MigLayout("top,flowy,ins 2","",""));
		
		// The main panel has two panes - the editor and the buttons
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new MigLayout("ins 2","",""));
		//addSeparator(editPanel,"List");
		editPanel.add(createTablePanel(),"wrap");
		add(editPanel,"");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, "dock south");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {

				DefaultTableModel dtm = (DefaultTableModel) table.getModel();
				List<String> model = new ArrayList<>();
				int rowCount = dtm.getRowCount();
				log.infof("%s.OK action: row count = %d", TAG, rowCount);
				int row = 0;
				while (row < rowCount) {
					String rowValue = (String) dtm.getValueAt(row, 0);
					log.infof("%s.OK action: added %s", TAG, rowValue);
					if (rowValue.length() > 0) {
						model.add(rowValue);
					}
					row++;
				}
				dispose();
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton,"");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}			
		});
	}

	
	
	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createTablePanel()  {
		JPanel outerPanel = new JPanel();
		table = new JTable();		
		outerPanel.setLayout(new MigLayout("ins 2,filly","para[:300:]","[120]5[]"));
		String[] columnNames = { "PortName,Direction,Side,Datatype" };
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
						int index = 0;
						for( int i:selected ) {
							selected[index] = tbl.convertRowIndexToModel(i);
							log.infof("%s.createDeleteButton: Selected row %d converted to %d",TAG,i,selected[index]);
							if( selected[index] > maxIndex ) maxIndex = selected[index];
							if( selected[index] < minIndex ) minIndex = selected[index];
							index++;
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
					log.infof("%s.SelectionHandler.valueChanged: Delete %s", TAG,(lsm.isSelectionEmpty()?"DISABLE":"ENABLE"));
					delBtn.setEnabled(!lsm.isSelectionEmpty());
				} 
			}
		}
	}
	
	private class EncapsulationEditTableModel extends AbstractTableModel {
        private String[] columnNames = {"First Name",
                                        "Last Name",
                                        "Sport",
                                        "# of Years",
                                        "Vegetarian"};
        private Object[][] data = {
        {"Kathy", "Smith",
         "Snowboarding", new Integer(5), new Boolean(false)},
        {"John", "Doe",
         "Rowing", new Integer(3), new Boolean(true)},
        {"Sue", "Black",
         "Knitting", new Integer(2), new Boolean(false)},
        {"Jane", "White",
         "Speed reading", new Integer(20), new Boolean(true)},
        {"Joe", "Brown",
         "Pool", new Integer(10), new Boolean(false)}
        };
 
        public int getColumnCount() {
            return columnNames.length;
        }
 
        public int getRowCount() {
            return data.length;
        }
 
        public String getColumnName(int col) {
            return columnNames[col];
        }
 
        public Object getValueAt(int row, int col) {
            return data[row][col];
        }
 
        /*
         * JTable uses this method to determine the default renderer/
         * editor for each cell.  If we didn't implement this method,
         * then the last column would contain text ("true"/"false"),
         * rather than a check box.
         */
        public Class getColumnClass(int c) {
            return getValueAt(0, c).getClass();
        }
 
        /*
         * Don't need to implement this method unless your table's
         * editable.
         */
        public boolean isCellEditable(int row, int col) {
            //Note that the data/cell address is constant,
            //no matter where the cell appears onscreen.
            if (col < 2) {
                return false;
            } else {
                return true;
            }
        }
 
        /*
         * Don't need to implement this method unless your table's
         * data can change.
         */
        public void setValueAt(Object value, int row, int col) {
 
            data[row][col] = value;
            fireTableCellUpdated(row, col);
 

        }
 
        private void printDebugData() {
            int numRows = getRowCount();
            int numCols = getColumnCount();
 
            for (int i=0; i < numRows; i++) {
                System.out.print("    row " + i + ":");
                for (int j=0; j < numCols; j++) {
                    System.out.print("  " + data[i][j]);
                }
                System.out.println();
            }
            System.out.println("--------------------------");
        }
    }
}
	