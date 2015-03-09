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
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Allow the user to define database connections and tag providers. This
 * applies to all projects globally. 
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
	protected JComboBox<String> mainDatabaseBox;
	protected JComboBox<String> secondaryDatabaseBox;
	protected JComboBox<String> mainProviderBox;
	protected JComboBox<String> secondaryProviderBox;
	protected JTextField mainTimeFactorField;
	protected JTextField secondaryTimeFactorField;
	
	public SetupDialog(DesignerContext ctx) {
		super(ctx.getFrame());
		this.context = ctx;
		this.setTitle("External Interface Configuration");
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
		// Initial row is just labels
		internalPanel.add(createLabel("Setup.Production"),"skip,gapx 50");  // Attempt to center
		internalPanel.add(createLabel("Setup.Isolation"),"gapx 50,wrap");
		
		// Databases
		internalPanel.add(createLabel("Setup.Database"),"");
		mainDatabaseBox  = createDatabaseCombo("Setup.ProductionDatabase.tooltip",BLTProperties.TOOLKIT_PROPERTY_DATABASE,false);
		internalPanel.add(mainDatabaseBox, "");
		secondaryDatabaseBox = createDatabaseCombo("Setup.IsolationDatabase.tooltip",BLTProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE,true);
		internalPanel.add(secondaryDatabaseBox, "wrap");
		
		// Tag providers
		internalPanel.add(createLabel("Setup.Provider"),"");
		mainProviderBox = createProviderCombo("Setup.ProductionProvider.tooltip",BLTProperties.TOOLKIT_PROPERTY_PROVIDER,false);
		internalPanel.add(mainProviderBox, "");
		secondaryProviderBox = createProviderCombo("Setup.IsolationProvider.tooltip",BLTProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER,true);
		internalPanel.add(secondaryProviderBox, "wrap");
		
		// Time factor
		internalPanel.add(createLabel("Setup.TimeFactor"),"");
		mainTimeFactorField = createTimeFactorTextField("Setup.ProductionTimeFactor.tooltip","");
		mainTimeFactorField.setEnabled(false);
		internalPanel.add(mainTimeFactorField, "");
		secondaryTimeFactorField = createTimeFactorTextField("Setup.IsolationTimeFactor.tooltip",BLTProperties.TOOLKIT_PROPERTY_ISOLATION_TIME);
		internalPanel.add(secondaryTimeFactorField, "wrap");
		
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
	 * Create a combo box that is populated with a list of database sources.
	 * Only those sources in "Valid" state are listed.
	 */
	private JComboBox<String> createDatabaseCombo(String bundle,String key,boolean isIsolation) {
		JComboBox<String> box = new JComboBox<String>();
		List<String> names = requestHandler.getDatasourceNames();
		box.removeAllItems();
		box.addItem("");
		for(String name:names) {
			if(name.length()>0) box.addItem(name);
		}
		box.setToolTipText(rb.getString(bundle));
		String currentValue = requestHandler.getToolkitProperty(key);
		if( (currentValue==null || currentValue.length()==0) && !isIsolation) currentValue = context.getDefaultDatasourceName();
		if( currentValue!=null ) box.setSelectedItem(currentValue);
		else {
			box.setSelectedIndex(0);  // The blank
		}
		// If the current value wasn't in the list, then add it.
		if( box.getSelectedIndex()<0 ) {
			box.addItem(currentValue);
			box.setSelectedItem(currentValue);
		}
		box.setPreferredSize(COMBO_SIZE);
		return box;
	}
	
	/**
	 * Create a combo box that is populated with a list of tag providers
	 */
	private JComboBox<String> createProviderCombo(String bundle,String key,boolean isIsolation) {
		JComboBox<String> box = new JComboBox<String>();
		List<TagProviderMeta> providers = context.getTagManager().getProviderInformation();
		box.removeAllItems();
		box.addItem("");
		for(TagProviderMeta meta:providers) {
			if( meta.getName().length()>0 ) box.addItem(meta.getName());
		}
		String currentValue = requestHandler.getToolkitProperty(key);
		if( currentValue.length()==0 && !isIsolation ) currentValue = context.getDefaultSQLTagsProviderName();
		box.setSelectedItem(currentValue);
		// If the current value wasn't in the list, then add it.
		if( box.getSelectedIndex()<0 ) {
			box.addItem(currentValue);
			box.setSelectedItem(currentValue);
		}
		box.setPreferredSize(COMBO_SIZE);
		return box;
	}
	/**
	 * Create a combo box that is populated with a list of tag providers
	 */
	private JTextField createTimeFactorTextField(String bundle,String key) {
		JTextField field = new JTextField();
		field.setText("1.0");  // Default
		field.setForeground(Color.BLACK);
		if( key.length()>0 ) {
			String currentValue = requestHandler.getToolkitProperty(key);
			if( currentValue!=null && currentValue.length()>0 ) {
				field.setText(currentValue);
			}
		}
		field.setPreferredSize(COMBO_SIZE);
		return field;
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
	
	private void validateEntries() {
		// Do not allow tag providers to be the same
		Object db1 = mainDatabaseBox.getSelectedItem();
		Object db2 = secondaryDatabaseBox.getSelectedItem();
		if( db1!=null && db2!=null && db1.toString().length()>0 && db2.toString().length()>0 &&
				db1.toString().equals(db2.toString()) )  {

			JOptionPane.showMessageDialog(context.getFrame(),
					"Isolation mode database connection must not be the same as production.",
					"Database equivalence warning",
					JOptionPane.WARNING_MESSAGE);
			secondaryDatabaseBox.setSelectedItem("");
		}
		else if( db1==null || db1.toString().length()==0 )  {

			JOptionPane.showMessageDialog(context.getFrame(),
					"Production database connection must be configured.",
					"Unset database warning",
					JOptionPane.WARNING_MESSAGE);
			mainDatabaseBox.setSelectedItem(context.getDefaultDatasourceName());
		}

		// Do not allow tag providers to be the same
		Object tp1 = mainProviderBox.getSelectedItem();
		Object tp2 = secondaryProviderBox.getSelectedItem();
		if( tp1!=null && tp2!=null && tp1.toString().length()>0 && tp2.toString().length()>0 &&
				tp1.toString().equals(tp2.toString()) )  {

			JOptionPane.showMessageDialog(context.getFrame(),
					"Isolation mode tag provider must not be the same as production.",
					"Tag provider equivalence warning",
					JOptionPane.WARNING_MESSAGE);
			secondaryProviderBox.setSelectedItem("");
		}
		else if( tp1==null || tp1.toString().length()==0 )  {
			JOptionPane.showMessageDialog(context.getFrame(),
					"Production tag provider must be configured.",
					"Unset provider warning",
					JOptionPane.WARNING_MESSAGE);
			mainProviderBox.setSelectedItem(context.getDefaultSQLTagsProviderName());
		}
		// Check numeric value of the speed factor
		String val = secondaryTimeFactorField.getText();
		try {
			double speedup = Double.parseDouble(val);
			if( speedup<=0.0001 ) {
				JOptionPane.showMessageDialog(context.getFrame(),
						"Time speedup ("+val+") must be greater than zero",
						"Invalid time factor warning",
						JOptionPane.WARNING_MESSAGE);
				secondaryTimeFactorField.setText("1.0");
			}
		}
		catch(NumberFormatException nfe) {
			JOptionPane.showMessageDialog(context.getFrame(),
					"Time speedup ("+val+") must be numeric",
					"Invalid time factor warning",
					JOptionPane.WARNING_MESSAGE);
			secondaryTimeFactorField.setText("1.0");
		}
	}

	// Read all widget values and save to persistent storage.
	// The validation has made them all legal
	private void saveEntries() {
		// For these we set new values for the next time queried
		requestHandler.setToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_DATABASE,mainDatabaseBox.getSelectedItem().toString() );
		requestHandler.setToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_ISOLATION_DATABASE,secondaryDatabaseBox.getSelectedItem().toString() );
		requestHandler.setToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_PROVIDER,mainProviderBox.getSelectedItem().toString() );
		requestHandler.setToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_ISOLATION_PROVIDER,secondaryProviderBox.getSelectedItem().toString() );
		requestHandler.setToolkitProperty(BLTProperties.TOOLKIT_PROPERTY_ISOLATION_TIME,secondaryTimeFactorField.getText());
		// This causes an immediate active update.
		double speedup = Double.parseDouble(secondaryTimeFactorField.getText());  // We've already validated the field ...
		requestHandler.setTimeFactor(speedup);
	}
}
