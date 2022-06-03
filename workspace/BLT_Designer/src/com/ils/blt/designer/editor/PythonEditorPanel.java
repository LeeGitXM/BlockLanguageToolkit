/**
 *   (c) 2015  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import com.ils.blt.common.block.BlockProperty;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Display a panel to edit a script associated with the tag. 
 * For sample code see com.inductiveautomation.ignition.designer.scripteditor
 * Make use of the JIDE CodeEdit widget.
 */

public class PythonEditorPanel extends BasicEditPanel {
	private static final long serialVersionUID = 1L;
	private BlockProperty property = null;

	
	public PythonEditorPanel(DesignerContext ctx,final BlockPropertyEditor editor) {
		super(editor);
		setLayout(new BorderLayout());
		JPanel editorPanel = new JPanel();
		JScrollPane treePane = new JScrollPane(editorPanel);
		treePane.setPreferredSize(BlockEditConstants.TREE_SIZE);
		add(treePane,BorderLayout.CENTER);
		JPanel buttonPanel = new JPanel();

		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton);
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {

				if(property!=null) {
					editor.updatePanelForProperty(BlockEditConstants.HOME_PANEL,property);
					editor.getDiagram().fireStateChanged();
					setSelectedPane(BlockEditConstants.HOME_PANEL);
				}
			}
		});

		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setSelectedPane(BlockEditConstants.HOME_PANEL);
			}			
		});
		add(buttonPanel,BorderLayout.SOUTH);
	}
	public void updateForProperty(BlockProperty prop) {
		this.property = prop;
		if(property.getBinding()!=null && (property.getBinding().length()>0) ) {
			log.debugf("PythonEditorPanel.updateForProperty binding = %s",property.getBinding());
		}
		else {
			log.warnf("TagBrowserPanel.updateForProperty: Current binding,%s, is not a script",property.getBinding());
		}

	}
}

