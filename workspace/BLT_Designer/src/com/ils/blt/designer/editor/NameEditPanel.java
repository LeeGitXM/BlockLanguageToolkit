/**
 *   (c) 2014-2020  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.BusinessRules;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.script.CommonScriptExtensionManager;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

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
				if( !nameField.getText().isEmpty()) {
					DesignerContext context = editor.getContext();
					ProcessDiagramView diagram = editor.getDiagram();
					
					// only check if name has changed.
					if (block.isDiagnosis() && nameField.getText().equalsIgnoreCase(block.getName()) == false) {
						
						BLTDesignerHook hook = (BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID);
						String msg = hook.scanForDiagnosisNameConflicts(diagram, nameField.getText());// send name and block
						if (msg != null && msg.length() > 1) {
							log.infof("Naming error: " + msg);
							ErrorUtil.showError("Naming error, duplicate diagnosis name: " + msg);
							return;  // abort save
							
						}
					}
					CommonScriptExtensionManager sem = CommonScriptExtensionManager.getInstance();
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
					editor.updateCorePanel(BlockEditConstants.HOME_PANEL,block);
					if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) ) {
						BlockProperty prop = block.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
						String path = prop.getBinding();
						ApplicationRequestHandler handler = editor.getRequestHandler();
						// If the tag is is in the standard location, rename it
						// otherwise, create a new one. Name must be a legal tag path element.
						if( !BusinessRules.isStandardConnectionsFolder(path) ) {
							String provider = getProvider();
							path = String.format("[%s]%s/%s",provider,BlockConstants.SOURCE_SINK_TAG_FOLDER,nameField.getText());
							handler.createTag(DataType.String,path);
						}
						else {
							handler.renameTag(nameField.getText(), path);
							path = renamePath(nameField.getText(), path);
						}
						prop.setBinding(path);
						
						// Perform similar modification on connected sources
						// Use the scripting interface to handle diagrams besides the current
						// The block name has already changed.
						for(SerializableBlockStateDescriptor desc:handler.listSourcesForSink(diagram.getId().toString(),block.getId().toString())) {
							SerializableResourceDescriptor rd = handler.getDiagramForBlock(desc.getIdString());
							if( rd==null ) continue;
							log.infof("NameEditPanel.actionPerformed: sink connected to %s",desc.getName());
							handler.setBlockPropertyBinding(rd.getId(), desc.getIdString(),BlockConstants.BLOCK_PROPERTY_TAG_PATH,path);
							handler.renameBlock(rd.getId(), desc.getIdString(), nameField.getText());
							editor.saveDiagram(rd.getResourceId());
						}
					}
				}
				try {
					block.setNameDisplayed(annotationCheckBox.isSelected());
					block.setNameOffsetX(Integer.parseInt(xfield.getText()));
					block.setNameOffsetY(Integer.parseInt(yfield.getText()));
					setSelectedPane(BlockEditConstants.HOME_PANEL);
					editor.updateCorePanel(BlockEditConstants.HOME_PANEL, block);
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
	// Replace the last element of path with name
	private String renamePath(String name,String path) {
		int index = path.lastIndexOf("/");
		if( index>0 ) {
			path = path.substring(0, index+1);
			path = path + name;
		}
		return path;
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
