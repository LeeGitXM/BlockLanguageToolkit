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

import javax.swing.DefaultListSelectionModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.ProjectBrowserRoot;

/**
 * Scan all dialogs defined in the gateway and report any issues. 
 */

public class ValidationDialog extends JDialog implements ListSelectionListener {
	private static String TAG = "ValidationDialog";
	private final LoggerEx log;
	private static final long serialVersionUID = 2002388376824434427L;
	private final int DIALOG_HEIGHT = 300;
	private final int DIALOG_WIDTH = 600;
	private final int TABLE_HEIGHT = 500;
	private final int TABLE_WIDTH = 1200;
	private String[] columnNames = {"Block","Issue"};
	private List<SerializableBlockStateDescriptor> issues = null;
	private final ApplicationRequestHandler requestHandler;
	private final DesignerContext context;
	private final ResourceBundle rb;
	private JTable table = null;
	private JPanel configurationPanel = null;

	public ValidationDialog(DesignerContext ctx) {
		super(ctx.getFrame());
		this.context = ctx;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setTitle("Diagram Validity Analysis");
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.requestHandler = new ApplicationRequestHandler();
		setModal(false);
		setAlwaysOnTop(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		initialize();
		queryController();
		updateInformation();  
	}

	private void initialize() {
		setLayout(new BorderLayout());
		JTabbedPane tabbedPane = new JTabbedPane();
		add(tabbedPane,BorderLayout.CENTER);
		configurationPanel = createConfigurationPanel();
		tabbedPane.addTab(rb.getString("Validation.Tab.Configuration"), configurationPanel);

		JPanel inactivePanel = createInactiveBlockPanel();
		tabbedPane.addTab(rb.getString("Validation.Tab.InactiveBlocks"), inactivePanel);

		JPanel unresponsivePanel = createUnresponsiveInputsPanel();
		tabbedPane.addTab(rb.getString("Validation.Tab.UnresponsiveInputs"), unresponsivePanel);
		tabbedPane.setSelectedIndex(0);
	}

	private JPanel createConfigurationPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 10","",""));

		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		// The OK button simply closes the dialog
		JButton okButton = new JButton("Dismiss");
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});

		// The Export button posts a file-choose dialog
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
		return panel;
	}
	private JPanel createInactiveBlockPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 10","",""));
		return panel;
	}

	private JPanel createUnresponsiveInputsPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 10","",""));
		return panel;
	}	
	// Query the controller for a list of "issues" with block configurations.
	private void queryController() {
		issues  = requestHandler.listConfigurationErrors();
		if( issues==null ) {
			log.infof("%s.queryController: no information returned",TAG);
		}
	}
	/**
	 * Update the UI per most current information from the controller
	 */
	private void updateInformation() {
		configurationPanel.removeAll();
		if( issues!=null ) configurationPanel.add(createListPanel(),"wrap");
		configurationPanel.revalidate();
		configurationPanel.repaint();
	}

	private void refresh() {
		queryController();
		updateInformation();
	}

	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createListPanel()  {
		JPanel outerPanel = new JPanel();
		table = new JTable();
		int nColumns = columnNames.length;
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","",""));
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0); 
		for( SerializableBlockStateDescriptor bsd:issues) {
			String[] row = new String[nColumns];
			row[0] = bsd.getAttributes().get(BLTProperties.BLOCK_ATTRIBUTE_PATH);
			row[1] = bsd.getAttributes().get(BLTProperties.BLOCK_ATTRIBUTE_ISSUE);
			dataModel.addRow(row);
		}
		table = new JTable(dataModel);
		table.setRowSelectionAllowed(true);
		table.setColumnSelectionAllowed(false);
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
			
		JScrollPane tablePane = new JScrollPane(table);
		tablePane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		tablePane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);

		table.setFillsViewportHeight(true);
		outerPanel.add(tablePane, "wrap");
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
				log.infof("%s.findChildInTree: testing %s vs %s",TAG,name,child.getName());
				if( child.getName().equalsIgnoreCase(name)) {
					match = child;
					break;
				}
			}
		}
		return match;
	}
	
	// ========================= List Selection Listener =============================
	public void valueChanged(ListSelectionEvent event) {
		// On a click we get the nav tree path and display the diagram.
		// Get proper row even if sorted. First column is the nav tree path.
		int baseRow = table.convertRowIndexToModel(table.getSelectedRow());
		String path = table.getModel().getValueAt(baseRow, 0).toString();
		// Lop off the block name to get the diagram path
		int pos = path.lastIndexOf(":");
		if( pos>0 ) path = path.substring(0, pos);
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
}
