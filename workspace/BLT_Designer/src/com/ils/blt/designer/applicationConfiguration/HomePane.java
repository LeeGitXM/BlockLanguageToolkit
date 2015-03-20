package com.ils.blt.designer.applicationConfiguration;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.border.BevelBorder;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;












import net.miginfocom.swing.MigLayout;









import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.block.RampMethod;
import com.inductiveautomation.ignition.common.BundleUtil;
//import com.ils.sfc.client.step.AbstractIlsStepUI;
import com.inductiveautomation.ignition.common.config.BasicPropertySet;
import com.inductiveautomation.ignition.common.config.PropertyValue;

public class HomePane extends JPanel implements ApplicationConfigurationController.EditorPane {
	private ApplicationConfigurationController controller;
	private Application application;
	private static final long serialVersionUID = 2882399376824334427L;
	protected static final Dimension COMBO_SIZE  = new Dimension(300,24);
	protected static final Dimension AREA_SIZE  = new Dimension(300,80);

	final JPanel buttonPanel;
	final JPanel mainPanel;
	
	final JTextField nameField = new JTextField();
	final JTextArea descriptionTextArea = new JTextArea();
	final JComboBox<String> consoleComboBox = new JComboBox<String>();
	final JComboBox<String> queueComboBox = new JComboBox<String>();
	final JTextField groupRampMethodField = new JTextField();
	final JTextField postField = new JTextField();
	final JTextField unitField = new JTextField();
	
	private static Icon addIcon = new ImageIcon(Application.class.getResource("/images/add.png"));
//	final JButton nextButton = new JButton(nextIcon);
	final JButton nextButton = new JButton("Outputs");
	final JButton cancelButton = new JButton("Cancel");
	final JButton okButton = new JButton("OK");
	// Don't add an Apply button becaus ethen I need to manage getting the id's of any quant outputs they create 
	// back from the extension manager.
	

	public HomePane(ApplicationConfigurationController controller, Application app) {
		super(new BorderLayout());
		this.controller = controller;
		this.application=app;

		// Add a couple of panels to the main panel
		buttonPanel = new JPanel(new FlowLayout());
		add(buttonPanel,BorderLayout.SOUTH);
		
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 10,gapy 3,gapx 5,fillx";
		final String rowConstraints = "para[][][][][][][][][]";		
		mainPanel = new JPanel(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		add(mainPanel,BorderLayout.CENTER);
		
		// Add components to the main panel
		mainPanel.add(new JLabel("Name:"),"");
		nameField.setText(application.getName());
		nameField.setPreferredSize(COMBO_SIZE);
		nameField.setEditable(false);
		nameField.setToolTipText("The name can only be changed from the project tree.");
		mainPanel.add(nameField,"span,wrap");

		mainPanel.add(new JLabel("Description:"),"gaptop 2,aligny top");
		String description = (String)application.getDescription();
		if( description==null) description="";
		descriptionTextArea.setText(description);
		descriptionTextArea.setEditable(true);
		descriptionTextArea.setToolTipText("Optional description of this application");

		JScrollPane scrollPane = new JScrollPane(descriptionTextArea);
		scrollPane.setPreferredSize(AREA_SIZE);
		mainPanel.add(scrollPane,"gaptop 2,aligny top,span,wrap");

		mainPanel.add(new JLabel("Console:"), "cell 0 5");
		for(Object console : application.getConsoles()) {
			consoleComboBox.addItem((String) console);
		}
		consoleComboBox.setToolTipText("The console where diagnosis will be added!");
		consoleComboBox.setSelectedItem(application.getConsole());
		consoleComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(consoleComboBox);
		
		mainPanel.add(new JLabel("Queue:"), "cell 0 6");
		for(Object q : application.getQueues()) {
			queueComboBox.addItem((String) q);
		}
		queueComboBox.setToolTipText("The message queue where messages for this application will be posted!");
		queueComboBox.setSelectedItem(application.getQueue());
		queueComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(queueComboBox);
		
		mainPanel.add(new JLabel("Ramp Method:"),"cell 0 7");
		groupRampMethodField.setText(application.getGroupRampMethod());
		groupRampMethodField.setPreferredSize(COMBO_SIZE);
		groupRampMethodField.setToolTipText("The method for calculating group ramps.");
		mainPanel.add(groupRampMethodField,"span,wrap");
		
		mainPanel.add(new JLabel("Post:"),"cell 0 8");
		postField.setText(application.getPost());
		postField.setPreferredSize(COMBO_SIZE);
		postField.setToolTipText("The name of the post for this application.");
		mainPanel.add(postField,"span,wrap");
		
		mainPanel.add(new JLabel("Unit:"),"cell 0 9");
		unitField.setText(application.getUnit());
		unitField.setPreferredSize(COMBO_SIZE);
		unitField.setToolTipText("The name of the unit for this application.");
		mainPanel.add(unitField,"span,wrap");
		
		
		/**
		 * Create a combo box for ramp method
		 */
/*
		final JComboBox<String> createRampMethodCombo(String bundle,String method) {
			
			JComboBox<String> box = new JComboBox<String>();
			for(RampMethod as : RampMethod.values()) {
				box.addItem(as.name());
			}
			box.setToolTipText(BundleUtil.get().getString(bundle));
			box.setSelectedItem(method.toUpperCase());
//			box.setPreferredSize(COMBO_SIZE);
			return box;
		}
*/		
		// Add buttons to the button panel
		buttonPanel.add(okButton); //, "cell 1 9");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doOk();}
		});
	
		buttonPanel.add(cancelButton); //, "cell 3 9");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doCancel();}
		});
		
		buttonPanel.add(nextButton); //, "cell 5 9");
		nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doNext();}			
		});
	}

	protected void doOk() {
		save();
		controller.doOK();
	}
	
	protected void doApply() {
		save();
	}
	
	protected void save(){
		// Set attributes from fields on this pane
		application.setConsole((String) consoleComboBox.getSelectedItem());
		application.setDescription(descriptionTextArea.getText());
		application.setQueue((String) queueComboBox.getSelectedItem());
		application.setGroupRampMethod(groupRampMethodField.getText());
		application.setPost(postField.getText());
		application.setUnit(unitField.getText());
	}

	protected void doCancel() {
		controller.doCancel();
	}

	protected void doNext() {
		controller.getSlidingPane().setSelectedPane(1);
	}

	@Override
	public void activate() {
		controller.slideTo(ApplicationConfigurationController.HOME);
	}

}