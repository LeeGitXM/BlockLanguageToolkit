package com.ils.blt.designer.applicationConfiguration;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import net.miginfocom.swing.MigLayout;

import com.inductiveautomation.ignition.client.sqltags.tree.TagRenderer;
import com.inductiveautomation.ignition.client.sqltags.tree.TagTreeNode;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

class TagSelectorPane extends JPanel implements ApplicationConfigurationController.EditorPane{
	private final ApplicationConfigurationController controller;
	private final OutputEditorPane outputEditorPane;
	private static Icon previousIcon = new ImageIcon(TagSelectorPane.class.getResource("/images/arrow_left_green.png"));
	final JButton previousButton = new JButton(previousIcon);
	final JButton saveButton = new JButton("Ok");
	
	// Copied from Chuck...
	private static final long serialVersionUID = 1L;
	private final DesignerContext context;
	private String selectedPath = "";
	private final JTree tagTree;
	private final TagRenderer cellRenderer;
	private final TreeSelectionModel tagTreeSelectionModel;
	public static final Dimension TREE_SIZE = new Dimension(600,500);
	
	
	// The constructor
	public TagSelectorPane(ApplicationConfigurationController controller,OutputEditorPane outputEditorPane) {
		super(new BorderLayout(20, 30));
		System.out.println("In TagSelector pane constructor");
		this.controller = controller;
		this.outputEditorPane = outputEditorPane;
		this.context = controller.context;
		
		JPanel mainPanel = new JPanel(new MigLayout("", "[right]"));
		
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
		JScrollPane treePane = new JScrollPane(tagTree);
		treePane.setPreferredSize(TREE_SIZE);
		mainPanel.add(treePane,BorderLayout.CENTER);

		// The previous button should be all the way at the bottom, hugging the left side.
		JPanel bottomPanel = new JPanel(new MigLayout("","[25%, left][50%, center][25%]",""));
		add(bottomPanel,BorderLayout.SOUTH);
		
		bottomPanel.add(previousButton);
		previousButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doPrevious();}
		});
		
		bottomPanel.add(saveButton);
		saveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doSave();}
		});
		
		add(mainPanel,BorderLayout.CENTER);
	}
	
	//  I don't think this is used...
	@Override
	public void activate() {
		controller.slideTo(ApplicationConfigurationDialog.TAGSELECTOR);
	}
	
	protected void doPrevious() {
		controller.slideTo(ApplicationConfigurationDialog.EDITOR);		
	}
	
	protected void doSave() {
		System.out.println("Saving...");
		
		TreePath[] selectedPaths = tagTreeSelectionModel.getSelectionPaths();
		if(selectedPaths.length == 1) {
			// It's possible to select something that's not a node.
			if(selectedPaths[0].getLastPathComponent() instanceof TagTreeNode ) {
				TagTreeNode node = (TagTreeNode)(selectedPaths[0].getLastPathComponent());
				selectedPath = node.getTagPath().toString();
				System.out.println("Selected: " + selectedPath);
				outputEditorPane.tagField.setText((String) selectedPath);
			}
		}
		else {
			JOptionPane.showMessageDialog(TagSelectorPane.this, "Please select a tag.");
			return;
		}
		
		controller.slideTo(ApplicationConfigurationDialog.EDITOR);		
	}
}
