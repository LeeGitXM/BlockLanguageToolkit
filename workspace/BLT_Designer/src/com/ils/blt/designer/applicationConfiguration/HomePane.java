package com.ils.blt.designer.applicationConfiguration;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import net.miginfocom.swing.MigLayout;

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
	final JComboBox<String> postComboBox = new JComboBox<String>();
	final JComboBox<String> queueComboBox = new JComboBox<String>();
	final JComboBox<String> groupRampMethodComboBox = new JComboBox<String>();
	final JComboBox<String> unitComboBox = new JComboBox<String>();
	
	private static Icon nextIcon = new ImageIcon(Application.class.getResource("/images/arrow_right_green.png"));
	final JButton nextButton = new JButton("Outputs", nextIcon);
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
		
		mainPanel = new JPanel(new MigLayout());
		add(mainPanel,BorderLayout.CENTER);
		
		// Add components to the main panel
		mainPanel.add(new JLabel("Name:"),"align right");
		nameField.setText(application.getName());
		nameField.setPreferredSize(COMBO_SIZE);
		nameField.setEditable(false);
		nameField.setToolTipText("The name can only be changed from the project tree.");
		mainPanel.add(nameField,"span,wrap");

		mainPanel.add(new JLabel("Description:"),"align right");
		String description = (String)application.getDescription();
		if( description==null) description="";
		descriptionTextArea.setText(description);
		descriptionTextArea.setEditable(true);
		descriptionTextArea.setToolTipText("Optional description of this application");

		JScrollPane scrollPane = new JScrollPane(descriptionTextArea);
		scrollPane.setPreferredSize(AREA_SIZE);
		mainPanel.add(scrollPane,"gaptop 2,aligny top,span,wrap");

		// Set up the Post Combo Box
		mainPanel.add(new JLabel("Post:"), "align right");
		List<String> posts = application.getPosts();
		if( posts!=null ) {
			for(Object post : application.getPosts()) {
				postComboBox.addItem((String) post);
			}
		}
		postComboBox.setToolTipText("The console where diagnosis will be added!");
		postComboBox.setSelectedItem(application.getPost());
		postComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(postComboBox, "wrap");
		
		// Set up the Message Queue Combo Box
		mainPanel.add(new JLabel("Queue:"), "align right");
		if( application.getQueues()!=null ) {
			for(Object q : application.getQueues()) {
				queueComboBox.addItem((String) q);
			}
		}
		
		queueComboBox.setToolTipText("The message queue where messages for this application will be posted!");
		queueComboBox.setSelectedItem(application.getQueue());
		queueComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(queueComboBox, "wrap");

		// Set up the Group Ramp Method Combo Box
		mainPanel.add(new JLabel("Ramp Method:"),"align right");
		if( application.getGroupRampMethods()!=null ) {
			for(Object o : application.getGroupRampMethods()) {
				groupRampMethodComboBox.addItem((String) o);
			}
		}
		
		groupRampMethodComboBox.setToolTipText("The Group Ramp Method that will be used for outputs in this application!");
		groupRampMethodComboBox.setSelectedItem(application.getGroupRampMethod());
		groupRampMethodComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(groupRampMethodComboBox, "wrap");
		
		// Set up the Unit Combo Box
		mainPanel.add(new JLabel("Unit:"),"align right");
		if( application.getUnits()!=null ) {
			for(Object o : application.getUnits()) {
				unitComboBox.addItem((String) o);
			}
		}
		
		unitComboBox.setToolTipText("The unit associated with this application!");
		unitComboBox.setSelectedItem(application.getUnit());
		unitComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(unitComboBox, "wrap");

		mainPanel.add(nextButton,"cell 1 9,right");
		nextButton.setHorizontalTextPosition(SwingConstants.LEFT);
		nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doNext();}			
		});
		
		// Add buttons to the button panel
		buttonPanel.add(okButton);
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doOk();}
		});
	
		buttonPanel.add(cancelButton);
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doCancel();}
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
		application.setPost((String) postComboBox.getSelectedItem());
		application.setDescription(descriptionTextArea.getText());
		application.setQueue((String) queueComboBox.getSelectedItem());
		application.setGroupRampMethod((String) groupRampMethodComboBox.getSelectedItem());
		application.setUnit((String) unitComboBox.getSelectedItem());
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