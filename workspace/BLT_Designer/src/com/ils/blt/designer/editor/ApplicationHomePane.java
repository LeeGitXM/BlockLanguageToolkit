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
import javax.swing.SwingUtilities;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.NotificationHandler;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

import net.miginfocom.swing.MigLayout;

public class ApplicationHomePane extends JPanel implements FocusListener, NotificationChangeListener {
	private static String CLSS = "ApplicationHomePane";
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private final ApplicationPropertyEditor editor;
	private final ApplicationRequestHandler requestHandler;
	private final GeneralPurposeDataContainer model;
	private static final long serialVersionUID = 2882399376824334427L;
	
	protected static final Dimension AREA_SIZE  = new Dimension(250,80);
	private final String key;
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
	private final String provider;
	private final String database;

	// Don't add an Apply button because then I need to manage getting the id's of any quant outputs they create 
	// back from the extension manager.
	

	public ApplicationHomePane(ApplicationPropertyEditor editor) {
		super(new BorderLayout());
		this.editor = editor;
		this.requestHandler = new ApplicationRequestHandler();
		this.model = editor.getModel();
		this.log = LogMaker.getLogger(this);
		this.database = requestHandler.getProductionDatabase();
		this.provider = requestHandler.getProductionTagProvider();
		this.key = NotificationKey.keyForAuxData(editor.getApplication().getId().toString());
		
		mainPanel = new JPanel(new MigLayout());
		add(mainPanel,BorderLayout.CENTER);
		
		// Add components to the main panel
		mainPanel.add(new JLabel("Name:"),"align right");
		
		nameField.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		nameField.setEditable(false);
		nameField.setToolTipText("The name can only be changed from the project tree.");
		mainPanel.add(nameField,"span,wrap");

		mainPanel.add(new JLabel("Description:"),"align right");
		descriptionTextArea.setEditable(true);
		descriptionTextArea.setLineWrap(true);
		descriptionTextArea.setWrapStyleWord(true);
		descriptionTextArea.setToolTipText("Optional description of this application");
		descriptionTextArea.setPreferredSize(AREA_SIZE);
		
		JScrollPane scrollPane = new JScrollPane(descriptionTextArea);
		
		mainPanel.add(scrollPane,"gaptop 2,aligny top,span,wrap");
		
		// Add the Managed check box
		mainPanel.add(new JLabel("Managed:"), "gap 10");
		mainPanel.add(managedCheckBox, "wrap, align left");

		// Set up the Message Queue Combo Box
		mainPanel.add(new JLabel("Queue:"), "align right");
		List<String> mqueues = model.getLists().get("MessageQueues");
		if(mqueues!=null ) {
			for(String q : mqueues) {
				queueComboBox.addItem(q);
			}
		}
		queueComboBox.setToolTipText("The message queue where messages for this application will be posted!");
		queueComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
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
		unitComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		mainPanel.add(unitComboBox, "wrap");
		
		mainPanel.add(nextButton,"cell 1 13,center");
		nextButton.setHorizontalTextPosition(SwingConstants.LEFT);
		nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doNext();}			
		});
		
		setUI();
		// Register for notifications
		log.infof("%s: adding listener %s",CLSS,key);
		mainPanel.addFocusListener(this);
	}

	// Fill widgets with current values
	private void setUI() {
		nameField.setText(model.getProperties().get("Name"));
		String description = model.getProperties().get("Description");
		if( description==null) description="";
		descriptionTextArea.setText(description);
		managedCheckBox.setSelected(fcns.coerceToBoolean( model.getProperties().get("Managed")));
		String queue = model.getProperties().get("MessageQueue");
		if( queue!=null ) queueComboBox.setSelectedItem(queue);
		else if( queueComboBox.getItemCount()>0) {
			queueComboBox.setSelectedIndex(0);
		}
		String method = model.getProperties().get("GroupRampMethod");
		if( method!=null ) groupRampMethodComboBox.setSelectedItem(method);
		else if( groupRampMethodComboBox.getItemCount()>0) {
			groupRampMethodComboBox.setSelectedIndex(0);
		}
		String unit = model.getProperties().get("Unit");
		if( unit!=null ) unitComboBox.setSelectedItem(unit);
		else if( unitComboBox.getItemCount()>0) {
			unitComboBox.setSelectedIndex(0);
		}
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
	public void shutdown() {
		log.infof("%s: removing listener %s",CLSS,key);
		notificationHandler.removeNotificationChangeListener(key,CLSS);
	}
	
	// ============================================== Focus listener ==========================================
	@Override
	public void focusGained(FocusEvent event) {
		if(event.getSource().equals(mainPanel)) {
			log.infof("%s.focusGained: ... for %s",CLSS,model.getProperties().get("Name"));
			requestHandler.refreshAuxData(editor.getContext().getProject().getId(),editor.getResource().getResourceId(), provider, database);
		}
	}
	@Override
	public void focusLost(FocusEvent event) {
		if(event.getSource().equals(mainPanel)) {
			log.infof("%s.focusLost: ... for %s",CLSS,model.getProperties().get("Name"));
			save();
		};
	}

	// ======================================= Notification Change Listener ===================================
	@Override
	public void bindingChange(String binding) {}
	@Override
	public void diagramStateChange(long resId, String state) {}
	@Override
	public void nameChange(String name) {}

	// The value is the aux data of the application. Note that the method is not
	// called on the Swing thread
	@Override
	public void valueChange(final QualifiedValue value) {
		log.infof("%s.valueChange: new aux data for %s",CLSS,model.getProperties().get("Name"));
		if( value==null ) return;
		GeneralPurposeDataContainer container = (GeneralPurposeDataContainer)value.getValue();
		if( container==null) return;
		model.setLists(container.getLists());
		model.setMapLists(container.getMapLists());
		model.setProperties(container.getProperties());
		SwingUtilities.invokeLater( new Runnable() {
			public void run() {
				setUI();
			}
		});
	}
	@Override
	public void watermarkChange(String mark) {}

}