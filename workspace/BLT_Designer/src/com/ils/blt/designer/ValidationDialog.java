/**
 *   (c) 2015  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.sqltags.model.TagProviderMeta;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Scan all dialogs defined in the gateway and report any issues. 
 */

public class ValidationDialog extends JDialog {
	private static String TAG = "ValidationDialog";
	private final LoggerEx log;
	private static final long serialVersionUID = 2002388376824434427L;
	private final int DIALOG_HEIGHT = 220;
	private final int DIALOG_WIDTH = 560;
	private String[] columnNames = {"Block","Issue"};
	private List<SerializableBlockStateDescriptor> issues = null;
	private final ApplicationRequestHandler requestHandler;
	private final ResourceBundle rb;
	private JPanel internalPanel = null;
	
	public ValidationDialog(DesignerContext ctx) {
		super(ctx.getFrame());
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setTitle("Diagram Validity Analysis");
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.requestHandler = new ApplicationRequestHandler();
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		initialize();
		queryController();
		updateInformation();  
	}
	
	private void initialize() {
		
		// The internal panel is a 3x4 grid
		setLayout(new BorderLayout());
		internalPanel = new JPanel();
		internalPanel.setLayout(new MigLayout("ins 10","",""));
		add(internalPanel,BorderLayout.CENTER);
		
		
		// The OK button simply closes the dialog
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton("Dismiss");
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveEntries();
				dispose();
			}
		});
		
		// The Eport button posts a file-choose dialog
		JButton exportButton = new JButton("Export");
		buttonPanel.add(exportButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveEntries();
			}
		});
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
		internalPanel.removeAll();
		if( issues!=null ) internalPanel.add(createListPanel(),"wrap");
		internalPanel.revalidate();
		internalPanel.repaint();
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
		JTable table = new JTable();
		int nColumns = columnNames.length;
		//outerPanel.setLayout(new MigLayout("ins 2,filly","",""));
		outerPanel.setLayout(new MigLayout("ins 2,fillx,filly","para[:480:]","[120]"));
		DefaultTableModel dataModel = new DefaultTableModel(columnNames,0); 
		for( SerializableBlockStateDescriptor bsd:issues) {
			String[] row = new String[nColumns];
			row[0] = bsd.getAttributes().get(BLTProperties.BLOCK_ATTRIBUTE_PATH);
			row[1] = bsd.getAttributes().get(BLTProperties.BLOCK_ATTRIBUTE_ISSUE);
			dataModel.addRow(row);
		}
        table = new JTable(dataModel);
        table.setRowSelectionAllowed(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);

        
        JScrollPane tablePane = new JScrollPane(table);
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
	

	// Read all widget values and save to persistent storage.
	// The validation has made them all legal
	private void saveEntries() {
		
	}
}
