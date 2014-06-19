/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import com.ils.block.common.BlockProperty;
import com.inductiveautomation.ignition.client.sqltags.tree.TagRenderer;
import com.inductiveautomation.ignition.client.sqltags.tree.TagTreeNode;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Display a panel to find and select tag paths.   
 */

public class TagBrowserPanel extends BasicEditPanel {


	private static final long serialVersionUID = 1L;
	private final DesignerContext context;
	private String selectedPath = "";
	private BlockProperty property = null;
	public TagBrowserPanel(BlockPropertyEditor editor,DesignerContext ctx) {
		super(editor);
		this.context = ctx;
		setLayout(new BorderLayout());
		JTree tagTree = new JTree();
		tagTree.setCellRenderer(new TagRenderer());
		tagTree.setModel(context.getTagBrowser().getSqlTagTreeModel());
		final TreeSelectionModel tagTreeSelectionModel = tagTree.getSelectionModel();
		tagTree.setBackground(getBackground());
		add(tagTree, BorderLayout.CENTER);
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton);
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TreePath[] selectedPaths = tagTreeSelectionModel.getSelectionPaths();
				if(selectedPaths.length == 1) {
					TagTreeNode node = (TagTreeNode)(selectedPaths[0].getLastPathComponent());					
					selectedPath = node.getTagPath().toString();
					//tagTreeSelectionModel.clearSelection();

				}
				else if(selectedPaths.length > 1) {
					JOptionPane.showMessageDialog(TagBrowserPanel.this, "More than one tag is selected--please select only one.");
				}
				else {
					JOptionPane.showMessageDialog(TagBrowserPanel.this, "No tag is selected.");					
				}
			}
		});

		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tagTreeSelectionModel.clearSelection();
				setSelectedPane(0);
			}			
		});
	}
	public void updateForProperty(BlockProperty prop) {
		this.property = prop;

	}
}

