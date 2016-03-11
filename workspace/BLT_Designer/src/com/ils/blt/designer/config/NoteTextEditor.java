/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *   http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/SharedModelDemoProject/src/components/SharedModelDemo.java
 */
package com.ils.blt.designer.config;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.WorkspaceRepainter;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * This is a read-only viewer for blocks for blocks that return internal state
 * (theoretically all of them). 
 */

public class NoteTextEditor extends JDialog {
	private static String TAG = "NoteTextEditor";
	private final LoggerEx log;
	// A panel is designed to edit properties that are lists of strings.
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Required for text strings
	private static final long serialVersionUID = 2002388376824434427L;
	private final int DIALOG_HEIGHT = 320;
	private final int DIALOG_WIDTH = 500;
	private static final Dimension PANEL_SIZE  = new Dimension(480,120);
	private final NodeStatusManager statusManager;
	private final UpdateTask updateTask;
	private final ScheduledExecutorService executor;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private JLabel textLabel;    // Use a label to display the results
	private JTextArea textArea;
	
	public NoteTextEditor(DesignerContext context,ProcessDiagramView diag,ProcessBlockView view) {
		super(context.getFrame());
		this.diagram = diag;
		this.block = view;
		this.setTitle(BundleUtil.get().getString(PREFIX+".NoteTextEdit.Title"));
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
        this.statusManager = ((BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID)).getNavTreeStatusManager();
        this.updateTask = new UpdateTask(textLabel,textArea);
        this.executor = Executors.newSingleThreadScheduledExecutor();
        executor.scheduleAtFixedRate(updateTask, 3, 10, TimeUnit.SECONDS);
	}
	
	private void initialize() {
		
		// The internal panel has two panes - one for the JTextPane, the other for the JTextArea.
		setLayout(new BorderLayout());
		JPanel internalPanel = new JPanel();
		internalPanel.setLayout(new MigLayout("ins 2","",""));
		addSeparator(internalPanel,"Edit Text here (use HTML for formatting)");
		textArea = createTextArea(internalPanel);
		addSeparator(internalPanel,"View Formatted Results here");
	
		textLabel = createTextLabel(internalPanel);
		add(internalPanel,BorderLayout.CENTER);
		

		// The OK button simply closes the dialog
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton, "");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				executor.shutdown();
				// Save the text and dimensions
				for(BlockProperty property:block.getProperties() ) {
					if( property.getName().equals(BlockConstants.BLOCK_PROPERTY_TEXT) ) {
						property.setValue(textArea.getText());
					}
					else if( property.getName().equals(BlockConstants.BLOCK_PROPERTY_WIDTH) ) {
						property.setValue(textLabel.getWidth());
					}
					else if( property.getName().equals(BlockConstants.BLOCK_PROPERTY_HEIGHT) ) {
						property.setValue(textLabel.getHeight());
					}
				}
				block.setDirty(true);
				statusManager.clearDirtyChildCount(diagram.getResourceId());
				SwingUtilities.invokeLater(new WorkspaceRepainter());
				dispose();
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton, "");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				executor.shutdown();
				dispose();
			}
		});
	}
	
	/**
	 * Create a fixed size text area in a scroll area. This is
	 * where the user can change the text. Periodically we update
	 * the text pane with the contents.
	 * 
	 * Initialize it with the block text property.
	 * @return
	 */
	private JTextArea createTextArea(JPanel outerPanel)  {
		JTextArea area = new JTextArea();
		area.setEditable(true);
		area.setLineWrap(true);
		area.setWrapStyleWord(true);
		// Look for the text property
		for(BlockProperty property:block.getProperties() ) {
			if( property.getName().equals(BlockConstants.BLOCK_PROPERTY_TEXT) ) {
				area.setText(property.getValue().toString());
				break;
			}
		}
		JScrollPane areaScrollPane = new JScrollPane(area);
		areaScrollPane.setVerticalScrollBarPolicy(
		                JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		areaScrollPane.setPreferredSize(PANEL_SIZE);
        outerPanel.add(areaScrollPane, "wrap");
		return area;
	}
	
	/**
	 * Create a stretchable text pane to display the contents of the
	 * text area. Add it to the supplied panel.
	 * @return
	 */
	private JLabel createTextLabel(JPanel outerPanel)  {
		JLabel label = new JLabel();
		label.setBackground(new Color(245,247,239));   // Light beige
        outerPanel.add(label, "wrap");
		return label;
	}
	
	
	/**
	 * Add a separator to a panel using Mig layout
	 */
	private JLabel addSeparator(JPanel panel,String text) {
		JSeparator separator = new JSeparator();
		JLabel label = new JLabel(text);
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		panel.add(label, "split 2,span");
		panel.add(separator, "growx,wrap");
		return label;
	}




	//================================= Execution Task ========================================
	/** 
	 * Run periodically to update the text area from the text pane.
	 */
	private class UpdateTask implements Runnable{
		private final JLabel pane;
		private final JTextArea area;
		/**
		 * Constructor for the text area update task
		 * @param p properties that appear in the header of each results list
		 */
		public UpdateTask(JLabel tpane,JTextArea tarea) {
			this.pane = tpane;
			this.area = tarea;
		}

		/**
		 * Execute a single update.
		 */
		public void run() {
			pane.setText(area.getText());
		}
	}
}
