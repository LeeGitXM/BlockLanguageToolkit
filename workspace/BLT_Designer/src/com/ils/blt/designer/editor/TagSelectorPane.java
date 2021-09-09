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

import com.inductiveautomation.ignition.client.tags.tree.TagRenderer;
import com.inductiveautomation.ignition.client.tags.tree.node.TagTreeNode;
import com.inductiveautomation.ignition.designer.DesignerContextImpl;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.tags.frame.TagBrowserFrame;
import com.inductiveautomation.ignition.designer.tags.tree.TagBrowserPanel;

import net.miginfocom.swing.MigLayout;

/**
 * Display a tag selection panel inside the PropertyEditor
 */
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
	private final TagRenderer tagRenderer;
	private final TreeSelectionModel tagTreeSelectionModel;
	public static final Dimension TREE_SIZE = new Dimension(300,250);
	
	
	// The constructor
	public TagSelectorPane(ApplicationPropertyEditor editor,OutputEditorPane outputEditorPane) {
		super(new BorderLayout(20, 30));
		this.editor = editor;
		this.outputEditorPane = outputEditorPane;
		this.context = editor.context;
		
		//JPanel mainPanel = new JPanel(new MigLayout("", "[right]"));
		JPanel mainPanel = new JPanel(new BorderLayout());
		setPreferredSize(ApplicationPropertyEditor.PANEL_SIZE);
		
		this.tagRenderer = new TagRenderer();
		//setLayout(new BorderLayout());
		tagTree = new JTree();
		tagTree.setOpaque(true);
		tagTree.setCellRenderer(tagRenderer);
		TagBrowserFrame frame = ((DesignerContextImpl)context).getTagBrowser();
		TagBrowserPanel panel = frame.getTagBrowserPanel();
		tagTree.setModel(panel.getTagTreeModel().);
		tagTreeSelectionModel = tagTree.getSelectionModel();
		tagTreeSelectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		tagTree.setBackground(getBackground());
		tagRenderer.setBackground(Color.cyan);
		JScrollPane treePane = new JScrollPane(tagTree);
		treePane.setPreferredSize(TREE_SIZE);
		mainPanel.add(treePane,BorderLayout.CENTER);

		// The previous button should be all the way at the bottom, hugging the left side.
		JPanel bottomPanel = new JPanel(new MigLayout("","[25%, left][50%, center][25%]",""));
		
		previousButton.setPreferredSize(ApplicationPropertyEditor.BUTTON_SIZE);
		previousButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doPrevious();}
		});
		bottomPanel.add(previousButton,"");
	
		cancelButton.setPreferredSize(ApplicationPropertyEditor.BUTTON_SIZE);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doCancel();}
		});
		bottomPanel.add(cancelButton,"wrap");
		mainPanel.add(bottomPanel,BorderLayout.SOUTH);
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
