/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.block.ActiveState;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * This is a read-only viewer for test results. We query the controller
 * periodically to update. 
 */

public class SetupDialog extends JDialog {
	private static String TAG = "TestResultDialog";
	private final LoggerEx log;
	protected static final Dimension COMBO_SIZE  = new Dimension(120,24);
	private static final long serialVersionUID = 2002388376824434427L;
	private final int DIALOG_HEIGHT = 320;
	private final int DIALOG_WIDTH = 520;
	private static final Dimension PANEL_SIZE  = new Dimension(480,240);
	private final ApplicationRequestHandler requestHandler;
	// These are the widgets that will contain the user selections
	protected JComboBox<String> mainDatabaseBox;
	protected JComboBox<String> secondaryDatabaseBox;
	protected JComboBox<String> mainProviderBox;
	protected JComboBox<String> secondaryProviderBox;
	
	public SetupDialog(DesignerContext context) {
		super(context.getFrame());
		this.setTitle("Diagram Configuration");
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
        this.requestHandler = new ApplicationRequestHandler();
	}
	
	private void initialize() {
		
		// The internal panel has two panes - one for the JTextPane, the other for the JTextArea.
		setLayout(new BorderLayout());
		JPanel internalPanel = new JPanel();
	
		internalPanel.setLayout(new MigLayout("ins 2","",""));
		mainDatabaseBox  = createDatabaseCombo("");
		secondaryDatabaseBox = createDatabaseCombo("");
		mainProviderBox = createProviderCombo("");
		secondaryProviderBox = createProviderCombo("");
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
	}
	
	/**
	 * Create a combo box that is populated with a list of database sources
	 */
	private JComboBox<String> createDatabaseCombo(String bundle) {
		JComboBox<String> box = new JComboBox<String>();
		for(ActiveState as : ActiveState.values()) {
			box.addItem(as.name());
		}
		//box.setToolTipText(rb.getString(bundle));
		//box.setSelectedItem(state.name());
		box.setPreferredSize(COMBO_SIZE);
		return box;
	}
	
	/**
	 * Create a combo box that is populated with a list of tag providers
	 */
	private JComboBox<String> createProviderCombo(String bundle) {
		JComboBox<String> box = new JComboBox<String>();
		for(ActiveState as : ActiveState.values()) {
			box.addItem(as.name());
		}
		//box.setToolTipText(rb.getString(bundle));
		//box.setSelectedItem(state.name());
		box.setPreferredSize(COMBO_SIZE);
		return box;
	}
}
