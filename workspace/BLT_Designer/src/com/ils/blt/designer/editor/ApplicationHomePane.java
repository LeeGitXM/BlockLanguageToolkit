package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import com.ils.blt.common.UtilityFunctions;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;

import net.miginfocom.swing.MigLayout;

public class ApplicationHomePane extends JPanel implements ActionListener, FocusListener {
	private final ApplicationPropertyEditor editor;
	private final GeneralPurposeDataContainer model;
	private static final long serialVersionUID = 2882399376824334427L;
	
	protected static final Dimension AREA_SIZE  = new Dimension(250,80);

	private final JPanel mainPanel;
	private final JTextField nameField = new JTextField();
	private final JTextArea descriptionTextArea = new JTextArea();
	private final JComboBox<String> queueComboBox = new JComboBox<String>();
	private final JComboBox<String> groupRampMethodComboBox = new JComboBox<String>();
	private final JComboBox<String> unitComboBox = new JComboBox<String>();
	final JCheckBox managedCheckBox = new JCheckBox();
	
	private static Icon nextIcon = new ImageIcon(ApplicationHomePane.class.getResource("/images/arrow_right_green.png"));
	private final JButton nextButton = new JButton("Outputs", nextIcon);;
	private final UtilityFunctions fcns = new UtilityFunctions();
	protected final ILSLogger log;

	// Don't add an Apply button because then I need to manage getting the id's of any quant outputs they create 
	// back from the extension manager.
	

	public ApplicationHomePane(ApplicationPropertyEditor editor) {
		super(new BorderLayout());
		this.editor = editor;
		this.model = editor.getModel();
		this.log = LogMaker.getLogger(this);
		
		mainPanel = new JPanel(new MigLayout());
		add(mainPanel,BorderLayout.CENTER);
		
		// Add components to the main panel
		mainPanel.add(new JLabel("Name:"),"align right");
		nameField.setText(model.getProperties().get("Name"));
		nameField.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		nameField.setEditable(false);
		nameField.setToolTipText("The name can only be changed from the project tree.");
		mainPanel.add(nameField,"span,wrap");

		mainPanel.add(new JLabel("Description:"),"align right");
		String description = model.getProperties().get("Description");
		if( description==null) description="";
		descriptionTextArea.setText(description);
		descriptionTextArea.setEditable(true);
		descriptionTextArea.setLineWrap(true);
		descriptionTextArea.setWrapStyleWord(true);
		descriptionTextArea.setToolTipText("Optional description of this application");
		descriptionTextArea.addFocusListener(this);
		descriptionTextArea.setPreferredSize(AREA_SIZE);
		
		JScrollPane scrollPane = new JScrollPane(descriptionTextArea);
		
		mainPanel.add(scrollPane,"gaptop 2,aligny top,span,wrap");
		
		// Add the Managed check box
		mainPanel.add(new JLabel("Managed:"), "gap 10");
		mainPanel.add(managedCheckBox, "wrap, align left");
		managedCheckBox.setSelected(fcns.coerceToBoolean( model.getProperties().get("Managed")));
		managedCheckBox.addActionListener(this);

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
		queueComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		queueComboBox.addActionListener(this);
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
		groupRampMethodComboBox.addActionListener(this);
		
		String method = model.getProperties().get("GroupRampMethod");
		if( method!=null ) groupRampMethodComboBox.setSelectedItem(method);
		else if( groupRampMethodComboBox.getItemCount()>0) {
			groupRampMethodComboBox.setSelectedIndex(0);
		}
		groupRampMethodComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
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
		unitComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		unitComboBox.addActionListener(this);
		mainPanel.add(unitComboBox, "wrap");
		
		mainPanel.add(nextButton,"cell 1 13,center");
		nextButton.setHorizontalTextPosition(SwingConstants.LEFT);
		nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doNext();}			
		});
	}

	
	protected void save(){
		// Set attributes from fields on this pane
		model.getProperties().put("Description",descriptionTextArea.getText());
		model.getProperties().put("MessageQueue",(String) queueComboBox.getSelectedItem());
		model.getProperties().put("GroupRampMethod",(String) groupRampMethodComboBox.getSelectedItem());
		model.getProperties().put("Unit",(String) unitComboBox.getSelectedItem());
		
		model.getProperties().put("Managed",(managedCheckBox.isSelected()?"1":"0"));
		editor.saveResource();
	}


	protected void doNext() {
		editor.setSelectedPane(ApplicationPropertyEditor.OUTPUTS);
	}
	public void activate() {
		editor.setSelectedPane(ApplicationPropertyEditor.HOME);
	}
	
	// ============================================== Action listener ==========================================
	@Override
	public void actionPerformed(ActionEvent event) {
		save();
		editor.saveResource();
	}	
	// ============================================== Focus listener ==========================================
	@Override
	public void focusGained(FocusEvent event) {
	}
	@Override
	public void focusLost(FocusEvent event) {
		save();
		editor.saveResource();
	}

}