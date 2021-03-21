package com.ils.blt.designer.editor;

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

class TagSelectorPane extends JPanel {
	private final OutputEditorPane outputEditorPane;
	private static Icon previousIcon = new ImageIcon(TagSelectorPane.class.getResource("/images/arrow_left_green.png"));
	final JButton previousButton = new JButton(previousIcon);
	final JButton cancelButton = new JButton("Cancel");
	
	// Copied from Chuck...
	private static final long serialVersionUID = 1L;
	private final DesignerContext context;
	private final ApplicationPropertyEditor editor;
	private String selectedPath = "";
	private final JTree tagTree;
	private final TagRenderer cellRenderer;
	private final TreeSelectionModel tagTreeSelectionModel;
	public static final Dimension TREE_SIZE = new Dimension(600,500);
	
	
	// The constructor
	public TagSelectorPane(ApplicationPropertyEditor editor,OutputEditorPane outputEditorPane) {
		super(new BorderLayout(20, 30));
		this.editor = editor;
		System.out.println("In TagSelector pane constructor");
		this.outputEditorPane = outputEditorPane;
		this.context = editor.context;
		
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
		previousButton.setPreferredSize(ApplicationPropertyEditor.BUTTON_SIZE);
		previousButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doPrevious();}
		});
		
		bottomPanel.add(cancelButton);
		cancelButton.setPreferredSize(ApplicationPropertyEditor.BUTTON_SIZE);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doCancel();}
		});
		
		add(mainPanel,BorderLayout.CENTER);
	}
	
	public void activate() {
		editor.setSelectedPane(ApplicationPropertyEditor.TAGSELECTOR);
	}
	
	protected void doCancel() {
		editor.setSelectedPane(ApplicationPropertyEditor.EDITOR);		
	}
	
	protected void doPrevious() {
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
		
		editor.setSelectedPane(ApplicationPropertyEditor.EDITOR);		
	}
}
