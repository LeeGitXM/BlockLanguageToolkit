/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.client.component.recmap;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;
import prefuse.visual.VisualItem;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * This is a read-only viewer for blocks for blocks that return internal state
 * (theoretically all of them). 
 */

public class LinkValueEditor extends JDialog {
	private static String TAG = "LinkValueEditor";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 2002388376824434427L;
	private final int DIALOG_HEIGHT = 180;
	private final int DIALOG_WIDTH = 240;
	
	private final VisualItem visualItem;
	private JTextField textField;
	
	public LinkValueEditor(VisualItem item) {
		super();
		this.visualItem = item;
		this.setTitle(BundleUtil.get().getString(PREFIX+".LinkValueEditor.Title"));
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();

	}
	
	private void initialize() {
		
		// The internal panel has two panes - one for the JTextPane, the other for the JTextArea.
		setLayout(new BorderLayout());
		JPanel internalPanel = new JPanel();
	
		internalPanel.setLayout(new MigLayout("ins 2","",""));
		addSeparator(internalPanel,"Edit value");
		textField = createTextField();
		internalPanel.add(textField,"growx,wrap");
		this.add(internalPanel, BorderLayout.CENTER);
		
		// The OK button simply closes the dialog
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Save the text and dimensions
				visualItem.setString(RecMapConstants.VALUE, textField.getText());
				dispose();
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton, "");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
	}
	
	/**
	 * Create a fixed size text area in a scroll area. This is
	 * where the user can change the text. Periodically we update
	 * the text pane with the contents.
	 * 
	 * Initialize it with the block text property.
	 * @return
	 */
	private JTextField createTextField()  {
		JTextField field = new JTextField();
		field.setEditable(true);
		field.setText(visualItem.getString(RecMapConstants.VALUE));
		return field;
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
	
	public String getEditedValue() { return textField.getText(); }

}
