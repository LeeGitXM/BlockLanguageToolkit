/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BusinessRules;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.inductiveautomation.ignition.common.tags.model.TagPath;
import com.inductiveautomation.ignition.common.tags.paths.parser.TagPathParser;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.tags.tree.selection.TagSelectionComponent;
import com.inductiveautomation.ignition.designer.tags.tree.selection.TagSelectionTreePanel;

/**
 * Display a panel to find and select tag paths.   
 */

public class TagTreePanel extends BasicEditPanel {
	private static final String CLSS = "TagTreePanel";
	private static final long serialVersionUID = 3130849715158553260L;
	private static Dimension NAV_BUTTON_SIZE = new Dimension(60,40);
	private final DesignerContext context;
	private BlockProperty property = null;
	private final TagSelectionComponent tagPanel;

	
	public TagTreePanel(DesignerContext ctx,final BlockPropertyEditor editor) {
		super(editor);
		this.context = ctx;
		String provider = new ApplicationRequestHandler().getProjectProductionTagProvider(context.getProjectName());
		this.tagPanel = TagSelectionTreePanel.simpleSingleProvider(context, provider);
		setLayout(new BorderLayout());
		JScrollPane treePane = new JScrollPane((Component)tagPanel);
		treePane.setPreferredSize(BlockEditConstants.TREE_SIZE);
		add(treePane,BorderLayout.CENTER);
		JPanel buttonPanel = new JPanel();
		
		JButton okButton = new JButton("OK");
		okButton.setPreferredSize(NAV_BUTTON_SIZE);
		buttonPanel.add(okButton);
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TagPath selectedTagPath = tagPanel.getSelectedTagPath();
				String selectedPath = selectedTagPath.toString();
				selectedPath = editor.modifyPathForProvider(selectedPath);
				if(property!=null) {
					log.infof("%s.actionPerformed set property %s, binding now %s",CLSS,property.getName(),selectedPath);
					// Before changing the value check some business rules.
					if( property.getName().equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH) &&
							!BusinessRules.isStandardConnectionsFolder(selectedPath) &&
							(editor.getBlock().getClassName().equals(BlockConstants.BLOCK_CLASS_SINK)||
									editor.getBlock().getClassName().equals(BlockConstants.BLOCK_CLASS_SOURCE)) ) {
						JOptionPane.showMessageDialog(TagTreePanel.this, 
								String.format("The tag path for a source or sink block must be in the %s tag folder.",
										BlockConstants.SOURCE_SINK_TAG_FOLDER));	
					}
					else if( property.getName().equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH) &&
							BusinessRules.isStandardConnectionsFolder(selectedPath) &&
							(editor.getBlock().getClassName().equals(BlockConstants.BLOCK_CLASS_INPUT)||
									editor.getBlock().getClassName().equals(BlockConstants.BLOCK_CLASS_OUTPUT)) ) {
						JOptionPane.showMessageDialog(TagTreePanel.this, 
								String.format("The tag path for an input or output block cannot be in the %s tag folder.",
										BlockConstants.SOURCE_SINK_TAG_FOLDER));	
					}
					else {
						editor.updatePanelForBinding(property.getName(), selectedPath);
						editor.updatePanelForProperty(BlockEditConstants.HOME_PANEL,property);
						setSelectedPane(BlockEditConstants.HOME_PANEL);
					}
				}
				else {
					JOptionPane.showMessageDialog(TagTreePanel.this, "No tag is selected.");					
				}
			}
		});
		
		JButton cancelButton = new JButton("Cancel");
		cancelButton.setPreferredSize(NAV_BUTTON_SIZE);
		buttonPanel.add(cancelButton);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tagPanel.reset();
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
			log.infof("%s.updateForProperty %s, binding = %s",CLSS,property.getName(),path);
			TagPath tp = TagPathParser.parseSafe(path);
			if( tp!=null ) {
				try {
					tagPanel.setSelectedTagPath(tp);
					log.infof("%s.updateForProperty %s",CLSS, tp.toString());
				}
				catch(Exception ex) {
					log.infof("%s.updateForProperty: Exception %s",CLSS,ex.getLocalizedMessage());
				}
			}
			else {
				log.warnf("%s.updateForProperty: Current binding,%s, is not a tag path",CLSS,path);
			}
		}
	}
}
