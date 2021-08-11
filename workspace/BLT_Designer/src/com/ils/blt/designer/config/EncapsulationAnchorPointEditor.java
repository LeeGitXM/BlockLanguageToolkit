/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer.config;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultCellEditor;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.ui.AnchorSide;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * This is an editor for encapsulation blocks. We allow the user to define anchor points. 
 */

public class EncapsulationAnchorPointEditor extends JDialog {
	private static String TAG = "EncapsulationAnchorPointEditor";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 2772399376824434427L;
	private static final Dimension BUTTON_SIZE = new Dimension(16,16);
	private final int DIALOG_HEIGHT = 320;
	private final int DIALOG_WIDTH = 420;
	private static final Dimension TABLE_SIZE  = new Dimension(380,120);
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private JButton addButton;      // Click to add a row
	private JButton deleteButton;   // Click to delete a row

	private JTable table;
	public EncapsulationAnchorPointEditor(DesignerContext ctx,ProcessDiagramView diag,ProcessBlockView view) {
		super();
		this.diagram = diag;
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
		editPanel.add(createTablePanel(),"wrap");
		add(editPanel,"gaptop 20");

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
		String PRE = PREFIX+".Encapsulation.Col.";
		String[] columnNames = { BundleUtil.get().getString(PRE+"PortName"),
				                 BundleUtil.get().getString(PRE+"Direction"),
				                 BundleUtil.get().getString(PRE+"Side"),
				                 BundleUtil.get().getString(PRE+"CxnType") };
		DefaultTableModel dataModel = new EncapsulationEditTableModel(columnNames,0);  // No null rows
        table = new JTable(dataModel);
        table.setPreferredSize(TABLE_SIZE);
        table.setRowSelectionAllowed(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        initColumnSizes(table);    // Setup column sizes
        // Set combo boxes for all except the name field.
        setUpNameColumn(table, table.getColumnModel().getColumn(0));
        setUpDirectionColumn(table, table.getColumnModel().getColumn(1));
        setUpSideColumn(table, table.getColumnModel().getColumn(2));
        setUpConnectionTypeColumn(table, table.getColumnModel().getColumn(3));
        
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
						Object[] row = { "in",AnchorDirection.INCOMING,AnchorSide.LEFT,ConnectionType.DATA};
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
	//============================= Column Renderers ====================================
    /*
     * This method uses column widths from the model.
     */
    private void initColumnSizes(JTable tbl) {
    	EncapsulationEditTableModel model = (EncapsulationEditTableModel)tbl.getModel();
        TableColumn column = null;
        int cellWidth = 0;
        int[] widths = model.cellWidths;
 
        for (int i = 0; i < model.getColumnCount(); i++) {
            cellWidth = widths[i];
            column = tbl.getColumnModel().getColumn(i);
            column.setPreferredWidth(cellWidth);
        }
    }
    private void setUpNameColumn(JTable table,TableColumn nameColumn) {
    	DefaultTableCellRenderer renderer = new DefaultTableCellRenderer();
		renderer.setToolTipText(BundleUtil.get().getString(PREFIX+".Encapsulation.Col.PortName.Desc"));
		nameColumn.setCellRenderer(renderer);
	}
	private void setUpDirectionColumn(JTable table,TableColumn directionColumn) {
		//Set up the editor for the direction cells.
		JComboBox<String> box = new JComboBox<String>();
		for(AnchorDirection dir : AnchorDirection.values()) {
			box.addItem(dir.name());
		}
		directionColumn.setCellEditor(new DefaultCellEditor(box));

		DefaultTableCellRenderer renderer =
				new DefaultTableCellRenderer();
		renderer.setToolTipText(BundleUtil.get().getString(PREFIX+".Encapsulation.Col.Direction.Desc"));
		directionColumn.setCellRenderer(renderer);
	}
	private void setUpSideColumn(JTable table,TableColumn directionColumn) {
		//Set up the editor for the side cells.
		JComboBox<String> box = new JComboBox<String>();
		for(AnchorSide side : AnchorSide.values()) {
			box.addItem(side.name());
		}
		directionColumn.setCellEditor(new DefaultCellEditor(box));

		DefaultTableCellRenderer renderer =
				new DefaultTableCellRenderer();
		renderer.setToolTipText(BundleUtil.get().getString(PREFIX+".Encapsulation.Col.Side.Desc"));
		directionColumn.setCellRenderer(renderer);
	}
	private void setUpConnectionTypeColumn(JTable table,TableColumn typeColumn) {
		//Set up the editor for the datatype cells.
		JComboBox<String> box = new JComboBox<String>();
		for(ConnectionType type : ConnectionType.values()) {
			box.addItem(type.name());
		}
		typeColumn.setCellEditor(new DefaultCellEditor(box));

		//Set up tool tips for the sport cells.
		DefaultTableCellRenderer renderer =
				new DefaultTableCellRenderer();
		renderer.setToolTipText(BundleUtil.get().getString(PREFIX+".Encapsulation.Col.CxnType.Desc"));
		typeColumn.setCellRenderer(renderer);
	}
	
	/**
	 * Create a selection listener for both the list and the table.
	 * We allow only one row to be selected at a time. Enable/disable
	 * the add/delete buttons.
	 */
	private class SelectionHandler implements ListSelectionListener {
		private final JTable selectionTable;
		private final JButton delBtn;

		SelectionHandler(JTable tbl,JButton rowDeleter ) {
			this.selectionTable = tbl;
			this.delBtn = rowDeleter;
		}
		
		public void valueChanged(ListSelectionEvent e) {
			if (!e.getValueIsAdjusting()) {
				if (e.getSource() == selectionTable.getSelectionModel()) {
					ListSelectionModel lsm = selectionTable.getSelectionModel();
					log.infof("%s.SelectionHandler.valueChanged: Delete %s", TAG,(lsm.isSelectionEmpty()?"DISABLE":"ENABLE"));
					delBtn.setEnabled(!lsm.isSelectionEmpty());
				} 
			}
		}
	}
	
	private class EncapsulationEditTableModel extends DefaultTableModel {
		private static final long serialVersionUID = 4731223117636121300L;
		public EncapsulationEditTableModel(String[] columnNames,int rowCount) {
			super(columnNames,rowCount);
		}
        
        /**
         * JTable uses this method to determine the default renderer/
         * editor for each cell.
         * @see javax.swing.table.DefaultTableModel#getColumnClass(int)
         */
		@Override
        public Class<?> getColumnClass(int c) {
			switch(c) {
				case 1: return AnchorDirection.class;
				case 2: return AnchorSide.class;
				case 3: return ConnectionType.class;
				default: return String.class;
			}
            
        }
		/**
		 * All cells are editable.
		 */
		@Override
		public boolean isCellEditable(int row, int col) {
            return true;
        }
		/**
		 * @return an array of column widths. Sum should be close to 400
		 *         
		 */
		public final int[] cellWidths = {100,100,80,100};
		/**
		 * When setting the value, cast into the correct class depending on the column.
		 * @see javax.swing.table.DefaultTableModel#setValueAt(java.lang.Object, int, int)
		 * @pqrqm col 1-based column number
		 */
		public void setValueAt(Object value, int row, int col) {
			if( value==null ) return;    // Do nothing
			switch(col) {
			    case 2:  super.setValueAt(AnchorDirection.valueOf(value.toString().toUpperCase()), row, col);
			    case 3:  super.setValueAt(AnchorSide.valueOf(value.toString().toUpperCase()), row, col);
			    case 4:  super.setValueAt(ConnectionType.valueOf(value.toString().toUpperCase()), row, col);
				default: super.setValueAt(value.toString(), row, col);
			}
            fireTableCellUpdated(row, col);
        }
    }
}
	