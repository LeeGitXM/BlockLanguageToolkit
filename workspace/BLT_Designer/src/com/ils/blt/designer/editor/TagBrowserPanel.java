/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import com.ils.blt.common.block.BlockProperty;
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
	private final TagRenderer cellRenderer;
	private final TreeSelectionModel tagTreeSelectionModel;
	
	public TagBrowserPanel(BlockPropertyEditor editor,DesignerContext ctx) {
		super(editor);
		this.context = ctx;
		this.cellRenderer = new TagRenderer();
		setLayout(new BorderLayout());
		JTree tagTree = new JTree();
		tagTree.setCellRenderer(cellRenderer);
		tagTree.setModel(context.getTagBrowser().getSqlTagTreeModel());
		tagTreeSelectionModel = tagTree.getSelectionModel();
		tagTreeSelectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		tagTree.setBackground(getBackground());
		cellRenderer.setBackground(Color.GREEN);
		add(tagTree, BorderLayout.CENTER);
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton);
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TreePath[] selectedPaths = tagTreeSelectionModel.getSelectionPaths();
				if(selectedPaths.length == 1) {
					log.infof("TagBrowserPanel 1");
					TagTreeNode node = (TagTreeNode)(selectedPaths[0].getLastPathComponent());
					log.infof("TagBrowserPanel 2");
					selectedPath = node.getTagPath().toString();
					log.infof("TagBrowserPanel %s",selectedPath);
					if(property!=null) {
						log.infof("TagBrowserPanel set property");
						property.setBinding(selectedPath);
					}
					updatePanelForProperty(BlockEditConstants.HOME_PANEL,property);
					setSelectedPane(BlockEditConstants.HOME_PANEL);
					log.infof("TagBrowserPanel set panel HOME");
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
				setSelectedPane(BlockEditConstants.HOME_PANEL);
			}			
		});
	}
	public void updateForProperty(BlockProperty prop) {
		this.property = prop;
		if(property.getBinding()!=null ) {
			TreePath treePath = new TreePath(property.getBinding().toString());
			tagTreeSelectionModel.setSelectionPath(treePath);
		}
	}
}

