/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer.config;

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

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;

/**
 * Allow the user to set a value to be propagated on each block output. 
 */

public class ForceValueSettingsDialog extends JDialog {
	private static final long serialVersionUID = 4224388376825535527L;
	private final Dimension ENTRY_BOX_SIZE = new Dimension(200,24);
	private final int DIALOG_HEIGHT = 100;
	private final int DIALOG_WIDTH = 300;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final Map<String,JComponent> componentMap;
	private final ResourceBundle rb;
	private final ApplicationRequestHandler requestHandler;
	
	public ForceValueSettingsDialog(Frame frame,ProcessDiagramView diag,ProcessBlockView view) {
		super(frame);
		this.diagram = diag;
		this.block = view;
		this.setTitle("Force Block Output Values");
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.block");  // block.properties
		setAlwaysOnTop(true);
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		// Extend the height depending on the count
		int count = countOutputs(block);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT+30*count));
		this.componentMap = new HashMap<>();
		this.requestHandler = new ApplicationRequestHandler();
        initialize();
	}
	
	private void initialize() {
		// The internal panel contains a list of outputs with entry
		// boxes for each according to type.
		setLayout(new BorderLayout());
		JPanel internalPanel = new JPanel();
		
		internalPanel.add(createOutputsPane(),"wrap");
		add(internalPanel, BorderLayout.CENTER);
		
		// The OK button reads the values from the widgets and propagates to output
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton(rb.getString("Force.ForceButton"));
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Loop over the values and place on output
				for(String port:componentMap.keySet()) {
					JComponent component = componentMap.get(port);
					String value = null;
					if( component instanceof JComboBox ) {
						value = ((JComboBox)component).getSelectedItem().toString();
					}
					else if(component instanceof JTextField) {
						value = ((JTextField)component).getText();
					}
					requestHandler.postResult(diagram.getId().toString(),block.getId().toString(),port,value);
				}
			}
		});
		JButton cancelButton = new JButton(rb.getString("Force.CancelButton"));
		buttonPanel.add(cancelButton, "");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
	}

		
	/**
	 * A list add panel is a panel appending a string element in the list. It contains:-
	 *        Scroll pane with the table, two buttons at the bottom.
	 */
	private JPanel createOutputsPane()  {
		JPanel outerPanel = new JPanel();		
		outerPanel.setLayout(new MigLayout("top,flowy,ins 2,gapy 0:10:15","","[top]0[]"));
		
		for( ProcessAnchorDescriptor pad:block.getAnchors()) {
			if( pad.getType().equals(AnchorType.Origin) ) {
				addSeparator(outerPanel,pad.getDisplay());
				if( pad.getConnectionType().equals(ConnectionType.TRUTHVALUE) ) {
					JComboBox<String> box = createTruthValueCombo();
					outerPanel.add(box,"gaptop 0,gapbottom 0,wrap");
					componentMap.put(pad.getDisplay(), box);
				}
				// For most types, just a text box
				else {
					JTextField field = createTextField();
					outerPanel.add(field,"gaptop 0,gapbottom 0,wrap");
					componentMap.put(pad.getDisplay(), field);
				}
			}
		}
		return outerPanel;
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
	/**
	 * Create a text box for the binding field. This is editable.
	 */
	private JComboBox<String> createTruthValueCombo() {	
		final JComboBox<String> valueCombo = new JComboBox<String>();
		for(TruthValue tv : TruthValue.values()) {
			valueCombo.addItem(tv.name());
		}
		valueCombo.setEditable(true);
		valueCombo.setPreferredSize(ENTRY_BOX_SIZE);
		return valueCombo;
	}
	/**
	 * Create a text box for the binding field. This is editable.
	 */
	private JTextField createTextField() {	
		JTextField field = new JTextField();
		field.setEditable(true);
		field.setPreferredSize(ENTRY_BOX_SIZE);
		return field;
	}
	
	private int countOutputs(ProcessBlockView blk) {
		int count = 0;
		for( ProcessAnchorDescriptor pad:block.getAnchors()) {
			if( pad.getType().equals(AnchorType.Origin) ) count++;
		}
		return count;
	}
	
}
	