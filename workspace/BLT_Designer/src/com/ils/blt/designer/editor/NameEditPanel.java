/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import com.ils.blt.client.ClientScriptExtensionManager;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;

import net.miginfocom.swing.MigLayout;

/**
 * Display a panel to edit the name of a block and its 
 * attribute display.  This is one of the sliding panels
 * in the block editor.   
 */

public class NameEditPanel extends BasicEditPanel {
	private static final long serialVersionUID = 1L;
	private static final String columnConstraints = "[para]0[]0[]0[]0[]";
	private static final String layoutConstraints = "ins 2";
	private static final String rowConstraints = "0[]0[]0[]0[]20[]";
	private ProcessBlockView block = null;
	private final JLabel headingLabel;
	private final JTextField nameField;
	private final JCheckBox annotationCheckBox;
	private final JTextField xfield;
	private final JTextField yfield;

	public NameEditPanel(final BlockPropertyEditor editor) {
		super(editor);
		setLayout(new BorderLayout());
		JPanel interiorPanel = new JPanel();
		interiorPanel.setLayout(new MigLayout("top,flowy,ins 2","",""));
		headingLabel = addHeading(interiorPanel);
		//Create three panels - binding type, data type, display option.
		JPanel namePanel = new JPanel();
		namePanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		addSeparator(namePanel,"Block Name");
		namePanel.add(createLabel("Name"),"skip");
		nameField = createNameTextField("");
		namePanel.add(nameField,"span 2,growx,wrap");
		interiorPanel.add(namePanel,"");
		
		JPanel displayPanel = new JPanel();
		displayPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,"[]10[]"));
		addSeparator(displayPanel,"Attribute Display");
		annotationCheckBox = new JCheckBox("Display ?");
		annotationCheckBox.setHorizontalTextPosition(SwingConstants.LEFT);
		displayPanel.add(annotationCheckBox,"gapafter 15");
		displayPanel.add(createLabel("X offset"),"");
		xfield = createOffsetTextField("0");
		displayPanel.add(xfield,"");
		displayPanel.add(createLabel("Y offset"),"gapbefore 15");
		yfield = createOffsetTextField("0");
		displayPanel.add(yfield,"wrap");
		interiorPanel.add(displayPanel,"");
		//add(new JSeparator(),"");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel();
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				boolean nameError = false;
				if( !nameField.getText().isEmpty()) {
					
					DesignerContext context = editor.getContext();
					// only check if name has changed.
					if (block.isDiagnosis() && nameField.getText().equalsIgnoreCase(block.getName()) == false) {
						ProcessDiagramView diagram = editor.getDiagram();
						
						BLTDesignerHook hook = (BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID);
						String msg = hook.scanForDiagnosisNameConflicts(diagram, nameField.getText());// send name and block
						if (msg != null && msg.length() > 1) {
							log.infof("Naming error: " + msg);
							ErrorUtil.showError("Naming error, duplicate diagnosis name: " + msg);
							return;  // abort save
							
						}
					}
					ClientScriptExtensionManager sem = ClientScriptExtensionManager.getInstance();
					if( sem.getClassNames().contains(block.getClassName()) ) {
						try {
							sem.runScript(context.getScriptManager(),block.getClassName(), ScriptConstants.NODE_RENAME_SCRIPT, 
									editor.getDiagram().getId().toString(),block.getName(),nameField.getText());     // Old name, new name
						}
						catch( Exception ex ) {
							log.errorf("NameEditPanel.constructor: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
						}
					}
					block.setName(nameField.getText());
				}
				try {
					block.setNameDisplayed(annotationCheckBox.isSelected());
					block.setNameOffsetX(Integer.parseInt(xfield.getText()));
					block.setNameOffsetY(Integer.parseInt(yfield.getText()));
					setSelectedPane(BlockEditConstants.HOME_PANEL);
					editor.updatePanelForBlock(BlockEditConstants.HOME_PANEL, block);
					editor.saveDiagram();
				}
				catch(NumberFormatException nfe) {
					JOptionPane.showMessageDialog(NameEditPanel.this, String.format("Illegal value for offset--please re-enter (%s)",nfe.getLocalizedMessage()),
							"Display Parameter Entry Error",JOptionPane.ERROR_MESSAGE);
					block.setNameDisplayed(false);
				}
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton,"");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setSelectedPane(BlockEditConstants.HOME_PANEL);
			}			
		});
		
		add(interiorPanel,BorderLayout.CENTER);
		add(buttonPanel, BorderLayout.SOUTH);
	}
	
	/**
	 * Create a text field for editing the name
	 */
	protected JTextField createNameTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(BlockEditConstants.ENTRY_BOX_SIZE);
		field.setEditable(true);
		return field;
	}

	/**
	 * Change the values displayed given a new block.
	 * @param blk
	 */
	public void updateForBlock(ProcessBlockView blk) {
		this.block = blk;
		headingLabel.setText(block.getName());
		nameField.setText(block.getName());
		annotationCheckBox.setSelected(blk.isNameDisplayed());
		xfield.setText(String.valueOf(blk.getNameOffsetX()));
		yfield.setText(String.valueOf(blk.getNameOffsetY()));
		// Repaint to update the name display
		SwingUtilities.invokeLater(new WorkspaceRepainter());
	}
}
