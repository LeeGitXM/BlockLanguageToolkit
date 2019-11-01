/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
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
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JCheckBox;
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

import com.ils.block.AbstractProcessBlock;
import com.ils.block.BlockPropertyDisplay;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import net.miginfocom.swing.MigLayout;

/**
 * This is a properties list viewer to allow users to select what block properties to display on the workspace 
 */

public class BlockPropertiesSelector extends JDialog implements TableModelListener {
	private static String TAG = "BlockPropertiesSelector";
	private final LoggerEx log;
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 4004388376825535527L;
	private final int DIALOG_HEIGHT = 400;
	private final int DIALOG_WIDTH = 600;
	private final int TABLE_HEIGHT = 200;
	private final int TABLE_WIDTH = 800;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private Map<String, BlockProperty> blockProperties = null;
	private JTable table = null;
	JPanel internalPanel = null;
	
	public BlockPropertiesSelector(Frame frame,ProcessDiagramView dia,ProcessBlockView view) {
		super(frame);
		this.diagram = dia;
		this.block = view;
		this.setTitle(String.format(BundleUtil.get().getString(PREFIX+".ViewInternals.Title",view.getName())));
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
		
		// The internal panel has three panes - one for properties, one for an activity history
		// and the other for any internal buffer
		setLayout(new BorderLayout());
		internalPanel = new JPanel();
		internalPanel.setLayout(new MigLayout("ins 2,fillx","",""));
		//Create the internal panel - it has two panes
		add(internalPanel,BorderLayout.CENTER);
		

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
		// The Refresh button acquires more data
		JButton refreshButton = new JButton("Refresh");
		buttonPanel.add(refreshButton, "");
		refreshButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				refresh();
			}
		});
	}

	private void queryBlock() {
		
		
		ApplicationRequestHandler handler = new ApplicationRequestHandler();
		SerializableBlockStateDescriptor descriptor = handler.getInternalState(diagram.getId().toString(), block.getId().toString());
		if( descriptor!=null ) {
			blockProperties = descriptor.getProperties();
			log.infof("%s.queryBlock %s: %d properties",TAG, block.getName(),blockProperties.size());
		}
		else {
			log.infof("%s.queryBlock %s: no information returned",TAG,block.getName());
		}
	}
	/**
	 * Update the UI per most current information from the block
	 */
	private void updateInformation() {
		internalPanel.removeAll();
		addSeparator(internalPanel,"Properties");
		internalPanel.add(createPropertiesPanel(),"wrap");
		
		internalPanel.revalidate();
		internalPanel.repaint();
	}
	
	private void refresh() {
		queryBlock();
		updateInformation();
	}
	
	public void tableChanged(TableModelEvent e)
	{
	    if (e.getType() == TableModelEvent.UPDATE)
	    {
	        int row = e.getFirstRow();
	        int column = e.getColumn();

	        if (column == 0)
	        {
	            TableModel model = (TableModel)e.getSource();
	            boolean newValue = ((Boolean)model.getValueAt(row, column)).booleanValue();

	            String propName = (String) model.getValueAt(row, 1);
	    		for( String pad: blockProperties.keySet() ) {
	    			BlockProperty prop = blockProperties.get(pad);
	    			if (prop.getName().equalsIgnoreCase(propName)) {
	    				if (newValue != prop.isDisplayed()) {  // make sure it's a real change
	    					if (newValue == true) {
	    						log.trace("Adding a dispay block");
	    						
    							AbstractProcessBlock apBlock = new BlockPropertyDisplay();
    							ProcessBlockView newBlock = new ProcessBlockView (apBlock.getBlockPrototype().getBlockDescriptor());
    							Point parentLoc = block.getLocation();
    							Point loc = new Point((int)parentLoc.getX() + 50, (int)parentLoc.getY() + 50);
    							newBlock.setLocation(loc);
    						
								newBlock.setLocation(loc);
   								diagram.addBlock(newBlock);

//   								gateway.block.linkDisplay(newBlock)
	    								
   								log.infof("%s.Drop: dropped",TAG);

//	    						add a display block and connect it to the current block
	    					} else {
	    						log.warn("EREIAM JH - REMOVE A DISpLAY!");
//	    						This needs a call to the gateway
//								block.unLinkDisplay(prop.getName());
//	    						delete a display block and disconnect from current block
	    					}
	    				}
	    			}
	            }
	    		refresh();
	        }
	    }
	}
	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createPropertiesPanel()  {
		JPanel outerPanel = new JPanel();
		table = new JTable();		
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","",""));
		String PRE = PREFIX+".ViewInternals.Col.";
		String[] columnNames = { "Shown", BundleUtil.get().getString(PRE+"Name"),
				                 BundleUtil.get().getString(PRE+"Value") };
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0)                
		{
	        public Class<?> getColumnClass(int colIndex) {
                return getValueAt(0, colIndex).getClass();
            }
		};
		dataModel.addTableModelListener(this);
		
		// Add anchors to the list of attributes
		for( String pad: blockProperties.keySet() ) {
			JCheckBox box = new JCheckBox(""+pad);
			Object[] row = new Object[3];
			row[0] = new Boolean(blockProperties.get(pad).isDisplayed());
				row[1] = pad;
				String text = "" + blockProperties.get(pad).getValue();
				row[2] = text;
				dataModel.addRow(row);
		}
		
        table = new JTable(dataModel);
        table.setRowSelectionAllowed(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
        table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
        table.setPreferredScrollableViewportSize(new Dimension(TABLE_WIDTH,TABLE_HEIGHT));

        
        JScrollPane tablePane = new JScrollPane(table);
        table.setFillsViewportHeight(true);
        outerPanel.add(tablePane, "pushx,wrap");
		return outerPanel;
	}
	
	
	/**
	 * Add a separator to a panel using Mig layout
	 */
	private JLabel addSeparator(JPanel panel,String text) {
		JSeparator separator = new JSeparator();
		JLabel label = new JLabel(text);
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		panel.add(label, "split 2,span");
		panel.add(separator, "growx,wrap");
		return label;
	}

}
	