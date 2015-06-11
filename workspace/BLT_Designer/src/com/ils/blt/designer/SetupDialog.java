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
import java.util.ResourceBundle;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.sqltags.model.TagProviderMeta;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Allow the user to define properties of the block language toolkit. 
 */

public class SetupDialog extends JDialog {
	protected static final Dimension COMBO_SIZE  = new Dimension(200,24);
	private static final long serialVersionUID = 2002388376824434427L;
	private final int DIALOG_HEIGHT = 220;
	private final int DIALOG_WIDTH = 560;
	private final DesignerContext context;
	private final ApplicationRequestHandler requestHandler;
	private final ResourceBundle rb;
	// These are the widgets that will contain the user selections
	protected JCheckBox useActiveCheckbox;
	protected JCheckBox useCompiledCheckbox;
	protected JCheckBox enableEnhancedCheckbox;
	
	public SetupDialog(DesignerContext ctx) {
		super(ctx.getFrame());
		this.context = ctx;
		this.setTitle("Toolkit Configuration");
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.requestHandler = new ApplicationRequestHandler();
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
		internalPanel.add(createLabel("Setup.UseActive"),"");
		useActiveCheckbox = createCheckbox("Setup.UseActive.tooltip");
		useActiveCheckbox.setSelected((requestHandler.getToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_ACTIVE_BLOCKS).equalsIgnoreCase("TRUE")?true:false));
		internalPanel.add(useActiveCheckbox, "wrap");
		internalPanel.add(createLabel("Setup.UseSchematic"),"");
		useCompiledCheckbox = createCheckbox("Setup.UseSchematic.tooltip");
		useCompiledCheckbox.setSelected((requestHandler.getToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_COMPILED_BLOCKS).equalsIgnoreCase("TRUE")?true:false));
		internalPanel.add(useCompiledCheckbox, "wrap");
		
		internalPanel.add(createLabel("Setup.EnhancedTree"),"");
		enableEnhancedCheckbox = createCheckbox("Setup.UseActive.tooltip");
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
	
	private void validateEntries() {
		
	}

	// Read all widget values and save to persistent storage.
	// The validation has made them all legal
	private void saveEntries() {
		// For these we set new values for the next time queried
		requestHandler.setToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_ACTIVE_BLOCKS,useActiveCheckbox.isSelected()?"TRUE":"FALSE");
		requestHandler.setToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_COMPILED_BLOCKS,useCompiledCheckbox.isSelected()?"TRUE":"FALSE" );
		requestHandler.setToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_ENHANCED_TREE,(enableEnhancedCheckbox.isSelected()?"TRUE":"FALSE") );
	}
}
