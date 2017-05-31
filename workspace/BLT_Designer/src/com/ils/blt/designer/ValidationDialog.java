/**
 *   (c) 2015-2017  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Enumeration;
import java.util.List;
import java.util.ResourceBundle;
import java.util.prefs.Preferences;

import javax.swing.DefaultListSelectionModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.ProjectBrowserRoot;

import net.miginfocom.swing.MigLayout;

/**
 * Scan all dialogs defined in the gateway and report any issues. 
 */

public class ValidationDialog extends JDialog implements ListSelectionListener {
	private static String TAG = "ValidationDialog";
	private final LoggerEx log;
	private static final long serialVersionUID = 2002388376824434427L;
	private static final String TABLE_NAME = "Table";
	private final int DIALOG_HEIGHT = 300;
	private final int DIALOG_WIDTH = 600;
	private final int TABLE_HEIGHT = 500;
	private final int TABLE_WIDTH = 1200;
	private String[] columnNames = {"Block","Issue"};
	private final ApplicationRequestHandler requestHandler;
	private final DesignerContext context;
	private final ResourceBundle rb;
	private JTabbedPane tabbedPane;
	private final TableHolder[] tables;
	private JTextField quietTimeField;
	private JComboBox<String>  classNameBox;
	private final Preferences prefs;

	public ValidationDialog(DesignerContext ctx) {
		super(ctx.getFrame());
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setTitle("Diagram Validity Analysis");
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.prefs = Preferences.userRoot().node(BLTProperties.PREFERENCES_NAME);
		this.requestHandler = new ApplicationRequestHandler();
		this.tables = new TableHolder[3];
		setModal(false);
		setAlwaysOnTop(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		initialize();
		updateInformation();  
	}

	private void initialize() {
		setLayout(new BorderLayout());
		tabbedPane = new JTabbedPane();
		add(tabbedPane,BorderLayout.CENTER);
		JPanel configurationPanel = createConfigurationPanel();
		tabbedPane.addTab(rb.getString("Validation.Tab.Configuration"), configurationPanel);

		// Subscription errors
		JPanel unresponsivePanel = createUnresponsiveInputsPanel();
		tabbedPane.addTab(rb.getString("Validation.Tab.UnresponsiveInputs"), unresponsivePanel);
		
		JPanel inactivePanel = createInactiveBlockPanel();
		tabbedPane.addTab(rb.getString("Validation.Tab.InactiveBlocks"), inactivePanel);
		tabbedPane.setSelectedIndex(0);

		
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		// The refresh button updates information
		JButton refreshButton = new JButton("Refresh");
		buttonPanel.add(refreshButton, "");
		refreshButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateInformation();
			}
		});

		// The OK button simply closes the dialog
		JButton okButton = new JButton("Dismiss");
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});

		// The Export button posts a file-choose dialog.
		// Export from the selected tab
		JButton exportButton = new JButton("Export");
		buttonPanel.add(exportButton, "");
		exportButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				SaveValidationResultsDialog dialog = new SaveValidationResultsDialog(rootPane,exportButton);
				dialog.pack();
				dialog.setVisible(true);   // Returns when dialog is closed
				File output = dialog.getFilePath();
				if( output!=null )  {
					// If we've been fed a Windows path, then we assume we're on a Windows system
					try {
						@SuppressWarnings("resource")
						BufferedWriter writer = new BufferedWriter(new FileWriter(output));

						int selection = tabbedPane.getSelectedIndex();
						JTable table = tables[selection].getTable();
						if( table!=null) {
							TableModel dataModel = table.getModel();
							int rows = dataModel.getRowCount();
							int cols = dataModel.getColumnCount();
							int row = 0;
							while(row<rows ) {
								int col = 0;
								while( col<cols ) {
									writer.write(dataModel.getValueAt(row, col).toString());
									col++;
									if(col==cols ) writer.write("\n");
									else 		   writer.write(",");
								}
								row++;
							}
						}
						writer.flush();
						writer.close();
					}
					catch(IOException ioe) {
						log.warnf("%s.openLog: Failed to open %s (%s)",TAG,output,ioe.getLocalizedMessage());
					}
				}
			}
		});
	}

	private JPanel createConfigurationPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 10","",""));
		panel.add(createListPanel(0),"wrap");
		return panel;
	}
	
	private JPanel createUnresponsiveInputsPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 10","",""));
		panel.add(createListPanel(1),"wrap");
		return panel;
	}
	private JPanel createInactiveBlockPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 10","",""));
		panel.add(createListPanel(2),"wrap");
		return panel;
	}
	
	// Query the controller for a list of "issues" with block configurations.
	// We have three separate categories.
	private void updateInformation() {
		List<SerializableBlockStateDescriptor> issues  = requestHandler.listConfigurationErrors();
		tables[0].updateIssues(issues);
		tables[0].getPanel().revalidate();
		tables[0].getPanel().repaint();
		
		issues  = requestHandler.listSubscriptionErrors();
		tables[1].updateIssues(issues);
		tables[1].getPanel().revalidate();
		tables[1].getPanel().repaint();
		
		
		try {
			double hrs = Double.parseDouble(quietTimeField.getText());
			this.prefs.put(BLTProperties.PREF_RESPONSE_INTERVAL, String.valueOf(hrs));
			String clss = classNameBox.getSelectedItem().toString();
			this.prefs.put(BLTProperties.PREF_RESPONSE_CLASS, clss);
			if( clss.equals("Any")) clss = "";
			issues  = requestHandler.listUnresponsiveBlocks(hrs,clss);
			tables[2].updateIssues(issues);
			tables[2].getPanel().revalidate();
			tables[2].getPanel().repaint();
		}
		catch(NumberFormatException nfe) {
			log.warnf("%s.updateInformation: Quiet time interval %s is not numeric (%s)",TAG,quietTimeField.getText(),nfe.getMessage());
		}
	}


	/**
	 * Create a panel that encloses a table that we fill with lists of issues.
	 * We include the index, so as to be able to retrieve the table from our holder.
	 */
	private JPanel createListPanel(int index)  {
		JPanel outerPanel = new JPanel();
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","",""));
		// The second panel has a few extra widgets at the top
		if( index==2 ) {
			JPanel controlPanel = new JPanel();
			controlPanel.add(createLabel("Validation.Tab.UnchangedTime"));
			quietTimeField = createIntervalField();
			controlPanel.add(quietTimeField);
			controlPanel.add(createLabel("Validation.Tab.BlockType"));
			classNameBox = createClassDropdown();
			controlPanel.add(classNameBox);
			outerPanel.add(controlPanel, "span,wrap");
		}
		JTable table = new JTable();
		table.setName(TABLE_NAME);
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0); 

		table = new JTable(dataModel);
		table.setRowSelectionAllowed(true);
		table.setColumnSelectionAllowed(false);
		table.setCellSelectionEnabled(false);
		// This trick makes the whole row selectable
		table.getColumnModel().setSelectionModel( new DefaultListSelectionModel() {
			private static final long serialVersionUID = 1L;
			@Override
			public int getLeadSelectionIndex() {
				return -1;
			}
		});
		table.setAutoCreateRowSorter(true);
		table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
		table.setPreferredScrollableViewportSize(new Dimension(TABLE_WIDTH, TABLE_HEIGHT));

		// Select on the row and navigate to the diagram. 
		table.getSelectionModel().addListSelectionListener(this);
		table.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		
		JScrollPane tablePane = new JScrollPane(table);
		tablePane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		tablePane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);

		table.setFillsViewportHeight(true);
		outerPanel.add(tablePane, "wrap");
		tables[index] = new TableHolder(outerPanel,table);
		return outerPanel;
	}

	/**
	 * Create a new label. The text is the bundle key.
	 */
	protected JLabel createLabel(String key) {
		JLabel label = new JLabel(rb.getString(key));
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		return label;
	}
	
	protected JTextField createIntervalField() {
		JTextField field = new JTextField();
		String val = this.prefs.get(BLTProperties.PREF_RESPONSE_INTERVAL, "1.0");
		field.setText(val);   // initialize to 1 hr
		return field;
	}
	
	
	protected JComboBox<String> createClassDropdown() {
		JComboBox<String> box = new JComboBox<String>();
		List<PalettePrototype> protos = requestHandler.getBlockPrototypes();
		box.addItem("Any");
		for(PalettePrototype proto:protos) {
			box.addItem(proto.getBlockDescriptor().getBlockClass());
		}
		String clss = prefs.get(BLTProperties.PREF_RESPONSE_CLASS, "Any");
		box.setSelectedItem(clss);
		return box;
	}

	/**
	 * We have not been successful with the findChild method .. so we've taken it on ourselves.
	 * @param root
	 * @param name
	 * @return
	 */
	private AbstractNavTreeNode findChildInTree(AbstractNavTreeNode root,String name) {
		AbstractNavTreeNode match = null;
		if( root!=null ) {
			@SuppressWarnings("unchecked")
			Enumeration<AbstractNavTreeNode> nodeWalker = root.children();
			AbstractNavTreeNode child = null;

			while( nodeWalker.hasMoreElements() ) {
				child = nodeWalker.nextElement();
				//log.infof("%s.findChildInTree: testing %s vs %s",TAG,name,child.getName());
				if( child.getName().equalsIgnoreCase(name)) {
					match = child;
					break;
				}
			}
		}
		return match;
	}
	
	// ========================= List Selection Listener =============================
	// We assume that we can only select from the current tab
	public void valueChanged(ListSelectionEvent event) {
		// On a click we get the nav tree path and display the diagram.
		// Get proper row even if sorted. First column is the nav tree path.
		int selection = tabbedPane.getSelectedIndex();
		JTable table = tables[selection].getTable();
		if( table==null ) {
			log.infof("%s.valueChanged: Unable to deduce table from selection model.",TAG);
			return;
		}
		log.infof("%s.valueChanged: Tab = %d.",TAG,selection);
		log.infof("%s.valueChanged: Event row = %d - %d.",TAG,event.getFirstIndex(),event.getLastIndex());
		int baseRow = table.convertRowIndexToModel(event.getFirstIndex());
		String path = table.getModel().getValueAt(baseRow, 0).toString();
		// Lop off the block name to get the diagram path
		int pos = path.lastIndexOf(":");
		if( pos>0 ) path = path.substring(0, pos);
		log.infof("%s.valueChanged: Tree path = %s.",TAG,path);
		ProjectBrowserRoot project = context.getProjectBrowserRoot();
		AbstractNavTreeNode root = null;
		AbstractNavTreeNode node = null;
		root = project.findChild("Project");
		if(root!=null) node = findChildInTree(root,"ROOT");
		// The specified path is colon-delimited.
		String[] pathArray = path.toString().split(":");

		int index = 1;  // Skip the leading colon
		while( index<pathArray.length ) {
			node = findChildInTree(node,pathArray[index]);
			if( node!=null ) {
				node.expand();
				try {
					Thread.sleep(100); 
				}
				catch(InterruptedException ignore) {}
			}
			else{
				log.warnf("%s.receiveNotification: Unable to find node (%s) on browser path",TAG,pathArray[index]);
				break;
			}
			index++;
		}

		if( node!=null ) {
			node.onDoubleClick();    // Opens the diagram
		}
		else {
			log.warnf("%s.receiveNotification: Unable to open browser path (%s)",TAG,path.toString());
		}
	}
	
	/**
	 * Create an instance for each tab. This carries the panel and associated table.
	 * @author chuckc
	 *
	 */
	private class TableHolder {
		private final JPanel panel;
		private final JTable table;
		public TableHolder(JPanel pnl,JTable tbl) {
			this.panel = pnl;
			this.table = tbl;
			
		}
		public JPanel getPanel() { return this.panel; }
		public JTable getTable() { return this.table; }
		public void updateIssues(List<SerializableBlockStateDescriptor> issues) {
			int nColumns = columnNames.length;
			table.removeAll();
			DefaultTableModel dataModel = (DefaultTableModel)table.getModel();
			dataModel.setRowCount(0);
			for( SerializableBlockStateDescriptor bsd:issues) {
				String[] row = new String[nColumns];
				row[0] = bsd.getAttributes().get(BLTProperties.BLOCK_ATTRIBUTE_PATH);
				row[1] = bsd.getAttributes().get(BLTProperties.BLOCK_ATTRIBUTE_ISSUE);
				dataModel.addRow(row);
			}
		}
	}
}
