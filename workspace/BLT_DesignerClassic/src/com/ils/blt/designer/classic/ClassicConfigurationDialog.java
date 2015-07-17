/**
 *   (c) 2015  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.classic;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ToolkitRequestHandler;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Allow the user to define properties of the block language toolkit - classic edition. 
 * Configurable properties are:
 *    Root name
 *    Extend tree hierarchy
 *    TRUE color
 *    FALSE COLOR
 */

public class ClassicConfigurationDialog extends JDialog {
	protected static final Dimension TEXT_FIELD_SIZE  = new Dimension(200,24);
	private static final long serialVersionUID = 2002388376824434427L;
	private final int DIALOG_HEIGHT = 220;
	private final int DIALOG_WIDTH = 560;
	private final DesignerContext context;
	private final ToolkitRequestHandler requestHandler;
	private final ResourceBundle rb;
	// These are the widgets that will contain the user selections
	protected JCheckBox enableEnhancedCheckbox;
	
	public ClassicConfigurationDialog(DesignerContext ctx,ToolkitRequestHandler handler) {
		super(ctx.getFrame());
		this.context = ctx;
		this.setTitle("Toolkit Configuration");
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.classic.designer");  // designer.properties
		this.requestHandler = handler;
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();  
	}
	
	private void initialize() {
		
		// The internal panel is a 3x4 grid
		setLayout(new BorderLayout());
		JPanel internalPanel = new JPanel();
	
		internalPanel.setLayout(new MigLayout("ins 4,fillx","para[growprio 0,100][180][180]para","para[30][30][30][30]"));

		
		// Navtree Configurations
		internalPanel.add(createLabel("Setup.EnhancedTree"),"");
		enableEnhancedCheckbox = createCheckbox("Setup.EnhancedTree.tooltip");
		enableEnhancedCheckbox.setSelected((requestHandler.getToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_ENHANCED_TREE).equalsIgnoreCase("TRUE")?true:false));
		internalPanel.add(enableEnhancedCheckbox, "wrap");
		
		add(internalPanel,BorderLayout.CENTER);
		
		// The OK button simply closes the dialog
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				validateEntries();
				saveEntries();
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
	 * Create a new label. The text is the bundle key.
	 */
	protected JLabel createLabel(String key) {
		JLabel label = new JLabel(rb.getString(key));
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		return label;
	}
	
	/**
	 * Create a check box. The text is tool-tip key.
	 */
	protected JCheckBox createCheckbox(String key) {
		JCheckBox box = new JCheckBox();
		box.setToolTipText(rb.getString(key));
		return box;
	}
	/**
	 * Create a combo box that is populated with a list of tag providers
	 */
	private JTextField createTextField(String bundle,String key) {
		JTextField field = new JTextField();
		field.setText("1.0");  // Default
		field.setForeground(Color.BLACK);
		if( key.length()>0 ) {
			String currentValue = requestHandler.getToolkitProperty(key);
			if( currentValue!=null && currentValue.length()>0 ) {
				field.setText(currentValue);
			}
		}
		field.setPreferredSize(TEXT_FIELD_SIZE);
		return field;
	}
	private void validateEntries() {
		
	}

	// Read all widget values and save to persistent storage.
	// The validation has made them all legal
	private void saveEntries() {
		// For these we set new values for the next time queried
		requestHandler.setToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_ENHANCED_TREE,(enableEnhancedCheckbox.isSelected()?"TRUE":"FALSE") );
	}
}
