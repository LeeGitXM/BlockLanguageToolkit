/**
 *   (c) 2014-2016  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.config;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.ActiveState;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Parent dialog for Application, Family and other configurations.
 *    ConfigurationDialog cd = new ConfigurationDialog(frame, app or fam);
 *    cd.pack();
 *    cd.setVisible(true);   // Terminates when dialog closed.
 *    result = cd.getXXX();  // Data from user entries
 */

public class ConfigurationDialog extends JDialog { 
	private static final long serialVersionUID = 2882399376824334427L;
	protected final DesignerContext context;
	protected final ResourceBundle rb;
	protected static final Dimension BUTTON_SIZE  = new Dimension(90,28);
	protected static final Dimension COMBO_SIZE  = new Dimension(120,24);
	protected static final Dimension DESCRIPTION_AREA_SIZE  = new Dimension(280,160);
	protected static final Dimension NAME_BOX_SIZE  = new Dimension(280,24);
	protected static final Dimension NUMBER_BOX_SIZE  = new Dimension(50,24);
	protected final LoggerEx log;
	protected final ApplicationRequestHandler requestHandler;
	private final UtilityFunctions fcns = new UtilityFunctions();
	protected JPanel contentPanel = null;
	protected JPanel buttonPanel = null;
	protected JButton okButton = null;
	protected JButton cancelButton = null;
	protected JTextField nameField;
	protected boolean cancelled = false;

	public ConfigurationDialog(DesignerContext ctx) {
		super(ctx.getFrame());
		this.context = ctx;
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.requestHandler = new ApplicationRequestHandler();
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        initialize();
	}
	

	/**
	 * Create the UI widgets at the root of either an Application or Family
	 * editor. The content pane is an empty JPanel..
	 * The pane at the bottom holds "OK" and "Cancel" buttons.
	 */
	private void initialize() {
		buttonPanel = new JPanel();
		contentPanel = new JPanel(new BorderLayout());
		okButton = new JButton("OK");
		okButton.setPreferredSize(BUTTON_SIZE);
		buttonPanel.add(okButton,"");
		cancelButton = new JButton("Cancel");
		cancelButton.setPreferredSize(BUTTON_SIZE);
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
	 * Create a text area for editing the description. This should be placed inside a
	 * scroll pane.
	 */
	protected JTextArea createTextArea(String bundle,String text) {	
		final JTextArea area = new JTextArea(text);
		area.setEditable(true);
		area.setLineWrap(true);
		area.setWrapStyleWord(true);
		area.setToolTipText(rb.getString(bundle));
		return area;
	}
	/**
	 * Create a text field for editing strings
	 */
	protected JTextField createTextField(String bundle,String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
		return field;
	}
	/**
	 * Create a text field for editing floating point values
	 */
	protected JFormattedTextField createDoubleField(String bundle,String text) {	
		final JFormattedTextField field = new JFormattedTextField(NumberFormat.getInstance());
		double dbl = fcns.coerceToDouble(text);
		field.setValue(new Double(dbl));
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
		return field;
	}
	/**
	 * Create a text field for editing integer values
	 */
	protected JFormattedTextField createIntegerField(String bundle,String text) {	
		final JFormattedTextField field = new JFormattedTextField(NumberFormat.getInstance());
		int i = fcns.coerceToInteger(text);
		field.setValue(new Integer(i));
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
		return field;
	}
	/**
	 * Add a separator to a panel using Mig layout. For the 
	 * separator text, use either the string directly, or 
	 * the resource, if it exists.
	 */
	protected void addSeparator(JPanel panel,String text) {
		JSeparator separator = new JSeparator();
		try {
			text = rb.getString(text);
		}
		catch(MissingResourceException ignore) {}
		JLabel label = new JLabel(text);
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
