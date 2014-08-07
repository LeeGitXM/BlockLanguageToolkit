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
import com.inductiveautomation.ignition.client.sqltags.tree.SQLTagTreeModel;
import com.inductiveautomation.ignition.client.sqltags.tree.TagRenderer;
import com.inductiveautomation.ignition.client.sqltags.tree.TagTreeNode;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.parser.TagPathParser;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Display a panel to find and select tag paths.   
 */

public class TagBrowserPanel extends BasicEditPanel {
	private static final long serialVersionUID = 1L;
	private final DesignerContext context;
	private String selectedPath = "";
	private BlockProperty property = null;
	private final JTree tagTree;
	private final TagRenderer cellRenderer;
	private final TreeSelectionModel tagTreeSelectionModel;
	
	public TagBrowserPanel(BlockPropertyEditor editor,DesignerContext ctx) {
		super(editor);
		this.context = ctx;
		this.cellRenderer = new TagRenderer();
		setLayout(new BorderLayout());
		tagTree = new JTree();
		tagTree.setOpaque(true);
		tagTree.setCellRenderer(cellRenderer);
		tagTree.setModel(context.getTagBrowser().getSqlTagTreeModel());
		tagTreeSelectionModel = tagTree.getSelectionModel();
		tagTreeSelectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		tagTree.setBackground(getBackground());
		cellRenderer.setBackgroundSelectionColor(Color.cyan);
		cellRenderer.setBackgroundNonSelectionColor(getBackground());
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
					if(property!=null) {
						log.infof("TagBrowserPanel set property %s, binding now %s",property.getName(),selectedPath);
						property.setBinding(selectedPath);
					}
					parent.updateBlockPropertyInEngine(property);
					updatePanelForProperty(BlockEditConstants.HOME_PANEL,property);
					setSelectedPane(BlockEditConstants.HOME_PANEL);
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
		if(property.getBinding()!=null && (property.getBinding().length()>0) ) {
			log.infof("TagBrowserPanel.updateForProperty binding = %s",property.getBinding());
			TagPath tp = TagPathParser.parseSafe(property.getBinding());
			if( tp!=null ) {
				log.infof("TagBrowserPanel.updateForProperty binding parsed as %s",tp.toString());
				SQLTagTreeModel sttm = (SQLTagTreeModel)tagTree.getModel();
				try {
					TreePath treePath = sttm.getPathForTag(tp);
					if( treePath!=null ) {
						tagTreeSelectionModel.setSelectionPath(treePath);
						tagTree.expandPath(treePath); 
						log.infof("TagBrowserPanel.updateForProperty %s",treePath.toString());
					}
					else {
						log.infof("TagBrowserPanel.updateForProperty: No tree path found for: %s:%s",property.getName(),property.getBinding());
					}
				}
				// getTagForPath() can throw a NPE
				catch(Exception ex) {
					log.infof("TagBrowserPanel.updateForProperty: Exception %s",ex.getLocalizedMessage());
				}
			}
			else {
				log.warnf("TagBrowserPanel.updateForProperty: Current binding,%s, is not a tag path",property.getBinding());
			}
		}
	}
}

