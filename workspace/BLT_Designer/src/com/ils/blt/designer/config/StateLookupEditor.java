/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer.config;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

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
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.designer.editor.BlockEditConstants;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import net.miginfocom.swing.MigLayout;

/**
 * This is a read-only viewer for blocks that return internal state
 * (theoretically all of them). 
 */

public class StateLookupEditor extends JDialog {
	private static String CLSS = "StateLookupEditor";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 4004388376825535527L;
	private final int DIALOG_HEIGHT = 400;
	private final int DIALOG_WIDTH = 600;
	private final int TABLE_HEIGHT = 250;
	private final int TABLE_WIDTH = 550;
	private final ProcessBlockView block;
	private final ProcessDiagramView diagram;
	private final Map<String,TruthValue> lookupMap;
	private final JTable table;
	JPanel internalPanel = null;

	public StateLookupEditor(DiagramWorkspace wksp,ProcessDiagramView dia,ProcessBlockView view) {
		super(wksp.getContext().getFrame());
		this.block = view;
		this.diagram = dia;
		this.lookupMap = new HashMap<>();
		this.table = new JTable();
		this.setTitle(String.format(BundleUtil.get().getString(PREFIX+".StateLookupEdit.Title",view.getName())));
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
		
		// The internal panel has a pane with the state list, plus a pane at the bottom 
		// with add and delete buttons.
		setLayout(new BorderLayout());
		JPanel mainPanel = new JPanel();
		internalPanel = new JPanel();
		internalPanel.setLayout(new MigLayout("ins 2,fillx","",""));
		mainPanel.add(internalPanel,BorderLayout.CENTER);
		//Create the internal panel - it has a single pane with buttons at the botton
		JPanel internalButtonPanel = new JPanel();
		internalButtonPanel.setLayout(new FlowLayout());
		mainPanel.add(internalButtonPanel,BorderLayout.SOUTH);
		
		add(mainPanel,BorderLayout.CENTER);
		
		// The delete button removes an existing row
		JButton deleteButton = createDeleteButton(table);
		internalButtonPanel.add(deleteButton, FlowLayout.LEFT);
		// The add button creates a new row
		JPanel buttonPanel = new JPanel();
		JButton addButton = createAddButton(table);
		internalButtonPanel.add(addButton,FlowLayout.LEFT);
		
		// The Save button saves the new values
		JButton saveButton = new JButton("Save");
		buttonPanel.add(saveButton, "");
		saveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				StringBuffer nameValues = new StringBuffer();
				TableModel model = table.getModel();
				int count = model.getRowCount();
				int row=0;
				while( row<count ) {
					if( row>0 )nameValues.append(",");
					nameValues.append(model.getValueAt(row, 0));
					nameValues.append(":");
					nameValues.append(model.getValueAt(row, 1).toString());
					row++;
				}
				for(BlockProperty prop:block.getProperties()) {
					if( prop.getName().equals(BlockConstants.BLOCK_PROPERTY_NAME_VALUES)) {
						prop.setValue(nameValues.toString());
						diagram.fireStateChanged();
						break;
					}
				}
				log.warnf("%s.savebutton: set %s=%s",CLSS,BlockConstants.BLOCK_PROPERTY_NAME_VALUES,nameValues.toString());
				dispose();
			}
		});
		// The Dismiss button simply closes the dialog
		JButton cancelButton = new JButton("Dismiss");
		buttonPanel.add(cancelButton, "");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
		add(buttonPanel, BorderLayout.SOUTH);
	}

	private void queryBlock() {
		Collection<BlockProperty> properties = block.getProperties();
		for(BlockProperty prop:properties) {
			if( prop.getName().equals(BlockConstants.BLOCK_PROPERTY_NAME_VALUES) ) {
				// Convert string into map
				String statevalues = prop.getValue().toString();
				lookupMap.clear();
				String[] keyvalues = statevalues.split(",");
				for(String key:keyvalues ) {
					String[] nv = key.split(":");
					String name = nv[0].trim().toUpperCase();
					TruthValue tv = TruthValue.UNKNOWN;
					if( nv.length>1) {
						String text = nv[1].trim().toUpperCase();
						try {
							tv = TruthValue.valueOf(text);
						}
						catch(IllegalStateException ise) {
							log.warnf("%s.queryBlock: Illegal state %s",CLSS,text);
						}
					}
					log.infof("%s.queryBlock: Received %s=%s",CLSS,name,tv.name());
					lookupMap.put(name,tv);
				}
				break;
			}
		}
	}
	/**
	 * Update the UI per most current information from the block
	 */
	private void updateInformation() {

		if( lookupMap.size()>0 ) {
			internalPanel.add(createStatesPanel(),"wrap");
		}
		internalPanel.revalidate();
		internalPanel.repaint();
	}

	
	/**
	 * Create a panel for displaying the name:values.
	 * The column names are: State. Value (a truth=value pulldown.
	 */
	private JPanel createStatesPanel()  {
		JPanel outerPanel = new JPanel();

		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","",""));
		String[] columnNames = {"Name", "Value"};
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0);
		table.setModel(dataModel);
		for( String key:lookupMap.keySet()) {
			String[] row = new String[2];
			row[0] = key;
			row[1] = lookupMap.get(key).name();
			dataModel.addRow(row);
			log.infof("%s.createStatesPanel: row %s : %s",CLSS,row[0],row[1]);
		}
        table.setRowSelectionAllowed(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
        table.setPreferredScrollableViewportSize(new Dimension(TABLE_WIDTH,TABLE_HEIGHT));
        
        TableColumn tvColumn = table.getColumnModel().getColumn(1);
        JComboBox<String> comboBox = new JComboBox<>();
        comboBox.addItem("TRUE");
        comboBox.addItem("FALSE");
        comboBox.addItem("UNKNOWN");
        tvColumn.setCellEditor(new DefaultCellEditor(comboBox));
        
        JScrollPane tablePane = new JScrollPane(table);
        table.setFillsViewportHeight(true);
        outerPanel.add(tablePane, "pushx,wrap");
		return outerPanel;
	}
	/**
	 * Create a button that adds a new entry
	 */
	private JButton createAddButton(final JTable tbl) {
		JButton btn = new JButton();
		final String ICON_PATH  = "Block/icons/editor/add.png";
		try {
			Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BlockEditConstants.BUTTON_SIZE);
			if( img !=null) {
				Icon icon = new ImageIcon(img);
				btn.setIcon(icon);
				btn.setMargin(new Insets(0,0,0,0));
				btn.setOpaque(false);
				btn.setBorderPainted(false);
				btn.setBackground(getBackground());
				btn.setBorder(null);
				btn.setPreferredSize(BlockEditConstants.BUTTON_SIZE);
				btn.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e){
						DefaultTableModel dtm = (DefaultTableModel)tbl.getModel();
						String[] row = new String[2];
						row[0] = "";   // Add an empty row
						row[1] = "UNKNOWN";
						dtm.addRow(row);
					}
				});
			}
			else {
				log.warnf("%s.createAddButton icon not found(%s)",CLSS,ICON_PATH);
			}
		}
		catch(Exception ex) {
			log.warnf("%s.createDeleteButton icon not found(%s) (%s)",CLSS,ICON_PATH, ex.getMessage());
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
			Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BlockEditConstants.BUTTON_SIZE);
			if( img !=null) {
				Icon icon = new ImageIcon(img);
				btn.setIcon(icon);
				btn.setMargin(new Insets(0,0,0,0));
				btn.setOpaque(false);
				btn.setBorderPainted(false);
				btn.setBackground(getBackground());
				btn.setBorder(null);
				btn.setPreferredSize(BlockEditConstants.BUTTON_SIZE);
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
							log.debugf("%s.createDeleteButton: Selected row %d converted to %d",CLSS,i,selected[index]);
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
				log.warnf("%s.createDeleteButton icon not found(%s)",CLSS,ICON_PATH);
			}
		}
		catch(Exception ex) {
			log.warnf("%s.createDeleteButton icon not found(%s) (%s)",CLSS,ICON_PATH, ex.getMessage());
		}
		return btn;
	}
}
	