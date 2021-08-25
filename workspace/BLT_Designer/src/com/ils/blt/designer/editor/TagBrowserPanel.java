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
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import com.ils.blt.common.BusinessRules;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.inductiveautomation.ignition.client.tags.tree.TagRenderer;
import com.inductiveautomation.ignition.client.tags.tree.TagTreeModel;
import com.inductiveautomation.ignition.common.tags.model.TagPath;
import com.inductiveautomation.ignition.common.tags.paths.parser.TagPathParser;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.tags.tree.TagTreeModelImpl;
import com.inductiveautomation.ignition.designer.tags.tree.node.TagTreeNode;

/**
 * Display a panel to find and select tag paths.   
 */

public class TagBrowserPanel extends BasicEditPanel {
	private static final String CLSS = "TagBrowserPanel";
	private static final long serialVersionUID = 1L;
	private final DesignerContext context;
	private static String selectedPath = "";
	private BlockProperty property = null;
	private final JTree jTree;
	private final TagRenderer cellRenderer;
	private final TreeSelectionModel tagTreeSelectionModel;
	private static TreePath lastSelected = null;
	
	public TagBrowserPanel(DesignerContext ctx,final BlockPropertyEditor editor) {
		super(editor);
		this.context = ctx;
		this.cellRenderer = new TagRenderer();
		setLayout(new BorderLayout());
		jTree = new JTree();
		jTree.setOpaque(true);
		jTree.setCellRenderer(cellRenderer);
		TagTreeModel model = new TagTreeModelImpl(context);
		model.resetRoot();
		jTree.setModel(model);
		tagTreeSelectionModel = jTree.getSelectionModel();
		tagTreeSelectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		jTree.setBackground(getBackground());
		cellRenderer.setBackground(Color.cyan);
		JScrollPane treePane = new JScrollPane(jTree);
		treePane.setPreferredSize(BlockEditConstants.TREE_SIZE);
		add(treePane,BorderLayout.CENTER);
		JPanel buttonPanel = new JPanel();
		if (lastSelected != null) {
			tagTreeSelectionModel.setSelectionPath(lastSelected);  // open to last selected path
		}
		
		JButton okButton = new JButton("OK");
		okButton.setPreferredSize(ApplicationPropertyEditor.NAV_BUTTON_SIZE);
		buttonPanel.add(okButton);
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TreePath[] selectedPaths = tagTreeSelectionModel.getSelectionPaths();
				if(selectedPaths.length == 1) {
					// It's possible to select something that's not a node.
					if(selectedPaths[0].getLastPathComponent() instanceof TagTreeNode ) {
						TagTreeNode node = (TagTreeNode)(selectedPaths[0].getLastPathComponent());
						lastSelected = selectedPaths[0];
						selectedPath = node.getTagPath().toString();
						selectedPath = editor.modifyPathForProvider(selectedPath);
						if(property!=null) {
							log.debugf("TagBrowserPanel set property %s, binding now %s",property.getName(),selectedPath);
							// Before changing the value check some business rules.
							if( property.getName().equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH) &&
								!BusinessRules.isStandardConnectionsFolder(selectedPath) &&
								(editor.getBlock().getClassName().equals(BlockConstants.BLOCK_CLASS_SINK)||
								 editor.getBlock().getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE)) ) {
								JOptionPane.showMessageDialog(TagBrowserPanel.this, 
										String.format("The tag path for a source or sink block must be in the %s tag folder.",
												BlockConstants.SOURCE_SINK_TAG_FOLDER));	
							}
							else if( property.getName().equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH) &&
									BusinessRules.isStandardConnectionsFolder(selectedPath) &&
									(editor.getBlock().getClassName().equals(BlockConstants.BLOCK_CLASS_INPUT)||
									 editor.getBlock().getClassName().equals(BlockConstants.BLOCK_CLASS_OUTPUT)) ) {
									JOptionPane.showMessageDialog(TagBrowserPanel.this, 
										String.format("The tag path for an input or output block cannot be in the %s tag folder.",
													BlockConstants.SOURCE_SINK_TAG_FOLDER));	
							}
							else {
								property.setBinding(selectedPath);
								editor.updatePanelForProperty(BlockEditConstants.HOME_PANEL,property);
								editor.saveDiagramClean();      // Immediate update in gateway
								setSelectedPane(BlockEditConstants.HOME_PANEL);
							}
						}
					}
				}
				else {
					JOptionPane.showMessageDialog(TagBrowserPanel.this, "No tag is selected.");					
				}
			}
		});
		
		JButton cancelButton = new JButton("Cancel");
		cancelButton.setPreferredSize(ApplicationPropertyEditor.NAV_BUTTON_SIZE);
		buttonPanel.add(cancelButton);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tagTreeSelectionModel.clearSelection();
				setSelectedPane(BlockEditConstants.HOME_PANEL);
			}			
		});
		add(buttonPanel,BorderLayout.SOUTH);
	}
	// The path has already been altered for the correct provider
	public void updateForProperty(BlockProperty prop) {
		this.property = prop;
		String path = property.getBinding();
		if(path!=null && path.length()>0 ) {
			log.debugf("%s.updateForProperty %s, binding = %s",CLSS,property.getName(),path);
			TagPath tp = TagPathParser.parseSafe(path);
			if( tp!=null ) {
				TagTreeModel sttm = (TagTreeModel)jTree.getModel();

				try {
					TreeNode[] nodes = sttm.getPathToRoot(sttm.);
					TreePath treePath = sttm.getPathForTag(tp);
					if( treePath!=null ) {
						tagTreeSelectionModel.setSelectionPath(treePath);
						jTree.expandPath(treePath); 
						log.debugf("TagBrowserPanel.updateForProperty %s",treePath.toString());
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
				log.warnf("TagBrowserPanel.updateForProperty: Current binding,%s, is not a tag path",path);
			}
		}
	}
}
