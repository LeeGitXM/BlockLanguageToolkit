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

import com.ils.common.GeneralPurposeDataContainer;

public class HomePane extends JPanel implements ApplicationConfigurationController.EditorPane {
	private final ApplicationConfigurationController controller;
	private final GeneralPurposeDataContainer model;
	private static final long serialVersionUID = 2882399376824334427L;
	
	protected static final Dimension AREA_SIZE  = new Dimension(300,80);
	protected static final Dimension BUTTON_SIZE  = new Dimension(80,28);
	protected static final Dimension COMBO_SIZE  = new Dimension(300,24);

	final JPanel buttonPanel;
	final JPanel mainPanel;
	
	final JTextField nameField = new JTextField();
	final JTextArea descriptionTextArea = new JTextArea();
	final JComboBox<String> postComboBox = new JComboBox<String>();
	final JComboBox<String> queueComboBox = new JComboBox<String>();
	final JComboBox<String> groupRampMethodComboBox = new JComboBox<String>();
	final JComboBox<String> unitComboBox = new JComboBox<String>();
	
	private static Icon nextIcon = new ImageIcon(HomePane.class.getResource("/images/arrow_right_green.png"));
	final JButton nextButton = new JButton("Outputs", nextIcon);
	final JButton cancelButton = new JButton("Cancel");
	final JButton okButton = new JButton("OK");

	// Don't add an Apply button because then I need to manage getting the id's of any quant outputs they create 
	// back from the extension manager.
	

	public HomePane(ApplicationConfigurationController controller) {
		super(new BorderLayout());
		this.controller = controller;
		this.model = controller.getModel();
		
		okButton.setPreferredSize(BUTTON_SIZE);
		cancelButton.setPreferredSize(BUTTON_SIZE);

		// Add a couple of panels to the main panel
		buttonPanel = new JPanel(new FlowLayout());
		add(buttonPanel,BorderLayout.SOUTH);
		
		mainPanel = new JPanel(new MigLayout());
		add(mainPanel,BorderLayout.CENTER);
		
		// Add components to the main panel
		mainPanel.add(new JLabel("Name:"),"align right");
		nameField.setText(model.getProperties().get("Name"));
		nameField.setPreferredSize(COMBO_SIZE);
		nameField.setEditable(false);
		nameField.setToolTipText("The name can only be changed from the project tree.");
		mainPanel.add(nameField,"span,wrap");

		mainPanel.add(new JLabel("Description:"),"align right");
		String description = model.getProperties().get("Description");
		if( description==null) description="";
		descriptionTextArea.setText(description);
		descriptionTextArea.setEditable(true);
		descriptionTextArea.setToolTipText("Optional description of this application");

		JScrollPane scrollPane = new JScrollPane(descriptionTextArea);
		scrollPane.setPreferredSize(AREA_SIZE);
		mainPanel.add(scrollPane,"gaptop 2,aligny top,span,wrap");

		// Set up the Post Combo Box
		mainPanel.add(new JLabel("Post:"), "align right");
		List<String> posts = model.getLists().get("Posts");
		if( posts!=null ) {
			for(String post : posts) {
				postComboBox.addItem(post);
			}
		}
		postComboBox.setToolTipText("The console where diagnosis will be added!");
		String post = model.getProperties().get("Post");
		if( post!=null ) postComboBox.setSelectedItem(post);
		else if( postComboBox.getItemCount()>0) {
			postComboBox.setSelectedIndex(0);
		}
		postComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(postComboBox, "wrap");
		
		// Set up the Message Queue Combo Box
		mainPanel.add(new JLabel("Queue:"), "align right");
		List<String> mqueues = model.getLists().get("MessageQueues");
		if(mqueues!=null ) {
			for(String q : mqueues) {
				queueComboBox.addItem(q);
			}
		}
		
		queueComboBox.setToolTipText("The message queue where messages for this application will be posted!");
		String queue = model.getProperties().get("MessageQueue");
		if( queue!=null ) queueComboBox.setSelectedItem(queue);
		else if( queueComboBox.getItemCount()>0) {
			queueComboBox.setSelectedIndex(0);
		}
		queueComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(queueComboBox, "wrap");

		// Set up the Group Ramp Method Combo Box
		mainPanel.add(new JLabel("Ramp Method:"),"align right");
		List<String> methods = model.getLists().get("GroupRampMethods");
		if( methods!=null ) {
			for(String o : methods) {
				groupRampMethodComboBox.addItem(o);
			}
		}
		
		groupRampMethodComboBox.setToolTipText("The Group Ramp Method that will be used for outputs in this application!");
		String method = model.getProperties().get("GroupRampMethod");
		if( method!=null ) groupRampMethodComboBox.setSelectedItem(method);
		else if( groupRampMethodComboBox.getItemCount()>0) {
			groupRampMethodComboBox.setSelectedIndex(0);
		}
		groupRampMethodComboBox.setPreferredSize(COMBO_SIZE);
		mainPanel.add(groupRampMethodComboBox, "wrap");
		
		// Set up the Unit Combo Box
		mainPanel.add(new JLabel("Unit:"),"align right");
		List<String> units = model.getLists().get("Units");
		if( units!=null ) {
			for(String o : units) {
				unitComboBox.addItem(o);
			}
		}
		
		unitComboBox.setToolTipText("The unit associated with this application!");
		String unit = model.getProperties().get("Unit");
		if( unit!=null ) unitComboBox.setSelectedItem(unit);
		else if( unitComboBox.getItemCount()>0) {
			unitComboBox.setSelectedIndex(0);
		}
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
		model.getProperties().put("Post",(String)postComboBox.getSelectedItem());
		model.getProperties().put("Description",descriptionTextArea.getText());
		model.getProperties().put("MessageQueue",(String) queueComboBox.getSelectedItem());
		model.getProperties().put("GroupRampMethod",(String) groupRampMethodComboBox.getSelectedItem());
		model.getProperties().put("Unit",(String) unitComboBox.getSelectedItem());
	}

	protected void doCancel() {
		controller.doCancel();
	}

	protected void doNext() {
		controller.slideTo(ApplicationConfigurationDialog.OUTPUTS);
	}

	@Override
	public void activate() {
		controller.slideTo(ApplicationConfigurationDialog.HOME);
	}

}