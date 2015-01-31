/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import com.ils.blt.common.block.ActiveState;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Parent dialog for Application and Family configurations.
 *    ConfigurationDialog cd = new ConfigurationDialog(frame, app or fam);
 *    cd.pack();
 *    cd.setVisible(true);   // Terminates when dialog closed.
 *    result = cd.getXXX();  // Data from user entries
 */

public class ConfigurationDialog extends JDialog { 
	private static final long serialVersionUID = 2882399376824334427L;
	protected final DesignerContext context;
	protected final ResourceBundle rb;
	protected static final Dimension COMBO_SIZE  = new Dimension(120,24);
	protected static final Dimension DESCRIPTION_BOX_SIZE  = new Dimension(280,80);
	protected static final Dimension NAME_BOX_SIZE  = new Dimension(280,24);
	protected static final Dimension NUMBER_BOX_SIZE  = new Dimension(50,24);
	protected final LoggerEx log;
	protected final Map<String,Object> properties;
	protected JTabbedPane parentTabPanel = null;
	protected JPanel contentPanel = null;
	protected JPanel buttonPanel = null;
	protected JButton okButton = null;
	protected JButton cancelButton = null;
	protected JTextArea descriptionArea;
	protected JTextField nameField;
	protected boolean cancelled = false;
	protected JComboBox<String> stateBox;

	// These are the keys to the map of properties that are common
	public final static String PROPERTY_DESCRIPTION = "description";

	
	public ConfigurationDialog(Frame frame,DesignerContext ctx) {
		super(frame);
		this.context = ctx;
		this.properties = new HashMap<>();
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        initialize();
	}
	

	/**
	 * Create the UI widgets at the root of either an Application or Family
	 * editor. The content pane holds a tabbed pane with multiple edit views.
	 * The pane at the bottom holds "OK" and "Cancel" buttons.
	 */
	private void initialize() {
		parentTabPanel = new JTabbedPane(SwingConstants.BOTTOM);
		parentTabPanel.setBorder(BorderFactory.createEtchedBorder());
		
		buttonPanel = new JPanel();
		contentPanel = new JPanel(new BorderLayout());
		contentPanel.add(parentTabPanel,BorderLayout.CENTER);
		okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cancelled = true;
				dispose();
			}			
		});
		buttonPanel.add(cancelButton,"");
		contentPanel.add(buttonPanel,BorderLayout.SOUTH);
		setContentPane(contentPanel);
	}
	/**
	 * Create a combo box for "active" state
	 */
	protected JComboBox<String> createActiveStateCombo(String bundle,ActiveState state) {
		JComboBox<String> box = new JComboBox<String>();
		for(ActiveState as : ActiveState.values()) {
			box.addItem(as.name());
		}
		box.setToolTipText(rb.getString(bundle));
		box.setSelectedItem(state.name());
		box.setPreferredSize(COMBO_SIZE);
		return box;
	}
	/**
	 * Create a new checkbox. The text is the bundle key for the tooltip.
	 */
	protected JCheckBox createCheckBox(String bundle,boolean initialValue) {
		JCheckBox box = new JCheckBox();
		box.setToolTipText(rb.getString(bundle)+": ");
		box.setSelected(initialValue);
		box.setText("");     // Don't use the standard label, it's on the wrong side.
		return box;
	}
	/**
	 * Create a new label. The text is the bundle key.
	 */
	protected JLabel createLabel(String bundle) {
		JLabel label = new JLabel(rb.getString(bundle)+": ");
		return label;
	}

	/*
	 * Create a text area for editing the description
	 */
	protected JTextArea createTextArea(String bundle,String text) {	
		final JTextArea area = new JTextArea(text);
		area.setBorder(BorderFactory.createLineBorder(Color.BLACK, 1));     // Thickness
		area.setPreferredSize(DESCRIPTION_BOX_SIZE);
		area.setEditable(true);
		area.setToolTipText(rb.getString(bundle));
		return area;
	}
	/**
	 * Create a text field for editing the name
	 */
	protected JTextField createTextField(String bundle,String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
		return field;
	}
	/**
	 * Add a separator to a panel using Mig layout
	 */
	protected void addSeparator(JPanel panel,String text) {
		JSeparator separator = new JSeparator();
		JLabel label = new JLabel(rb.getString(text));
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		panel.add(label, "split 2,span");
		panel.add(separator, "growx,wrap");
	}

	/*
	 * @return true if the user has selected the "Cancel" button.
	 */
	public boolean isCancelled() { return cancelled; }
}
