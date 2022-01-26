package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
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
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.script.Script;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.designer.NotificationHandler;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import net.miginfocom.swing.MigLayout;

public class ApplicationHomePane extends JPanel implements  NotificationChangeListener {
	private static String CLSS = "ApplicationHomePane";
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
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
	protected final LoggerEx log;
	private final String provider;

	// Don't add an Apply button because then I need to manage getting the id's of any quant outputs they create 
	// back from the extension manager.
	

	public ApplicationHomePane(ApplicationPropertyEditor editor) {
		super(new BorderLayout());
		this.editor = editor;
		this.requestHandler = new ApplicationRequestHandler();
		this.model = editor.getModel();
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.provider = requestHandler.getProjectProductionTagProvider(editor.getContext().getProjectName());
		this.key = NotificationKey.keyForAuxData(editor.getApplication().getResourcePath().getPath().toString());
		this.setPreferredSize(ApplicationPropertyEditor.PANEL_SIZE);
		
		mainPanel = new JPanel(new MigLayout("fillx", "[right]rel[grow, fill]"));
		add(mainPanel,BorderLayout.CENTER);
		
		// Add components to the main panel
		mainPanel.add(new JLabel("Name:"),"align right");
		
		nameField.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		nameField.setEditable(false);
		nameField.setToolTipText("The name can only be changed from the project tree.");
		mainPanel.add(nameField,"span,wrap");

		mainPanel.add(new JLabel("Description:"),"gaptop 2, aligny top, align right");
		descriptionTextArea.setEditable(true);
		descriptionTextArea.setLineWrap(true);
		descriptionTextArea.setWrapStyleWord(true);
		descriptionTextArea.setToolTipText("Optional description of this application");
		descriptionTextArea.setPreferredSize(AREA_SIZE);
		
		JScrollPane scrollPane = new JScrollPane(descriptionTextArea);
		
		mainPanel.add(scrollPane,"gaptop 2,aligny top,span,wrap");
		
		// Add the Managed check box
		mainPanel.add(new JLabel("Managed:"), "gap 10, align right");
		mainPanel.add(managedCheckBox, "wrap, align left");

		// Combo boxes are populated from a database query. The database used is a function
		// of the application state
		SerializableApplication app = ((ApplicationPropertyEditor)editor).getApplication();
		String projectName = editor.getContext().getProjectName();
		String db = requestHandler.getProjectProductionDatabase(projectName);
		String tag = requestHandler.getProjectProductionTagProvider(projectName);
		if( app.getState().equals(DiagramState.ISOLATED)) {
			db = requestHandler.getProjectIsolationDatabase(projectName);
			tag = requestHandler.getProjectIsolationTagProvider(projectName);
		}
		Script script = extensionManager.createExtensionScript(ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.GET_LIST_OPERATION, tag);
	
		// Create up the Message Queue Combo Box
		mainPanel.add(new JLabel("Queue:"), "align right");
		queueComboBox.setToolTipText("The message queue where messages for this application will be posted!");
		queueComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		List<String> items = new ArrayList<>();
		extensionManager.runScript(editor.context.getScriptManager(),script,ScriptConstants.LIST_KEY_MESSAGE_QUEUE,items,db);
		for(String item:items) {
			queueComboBox.addItem(item);
		}
		mainPanel.add(queueComboBox, "wrap");

		// Create up the Group Ramp Method Combo Box
		mainPanel.add(new JLabel("Ramp Method:"),"align right");
		groupRampMethodComboBox.setToolTipText("The Group Ramp Method that will be used for outputs in this application!");
		groupRampMethodComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		items = new ArrayList<>();
		extensionManager.runScript(editor.context.getScriptManager(),script,ScriptConstants.LIST_KEY_GROUP_RAMP,items,db);
		for(String item:items) {
			groupRampMethodComboBox.addItem(item);
		}
		mainPanel.add(groupRampMethodComboBox, "wrap");
		
		// Create the unit combo box
		mainPanel.add(new JLabel("Unit:"),"align right");
		unitComboBox.setToolTipText("The unit associated with this application!");
		unitComboBox.setPreferredSize(ApplicationPropertyEditor.COMBO_SIZE);
		items = new ArrayList<>();
		extensionManager.runScript(editor.context.getScriptManager(),script,ScriptConstants.LIST_KEY_UNIT,items,db);
		for(String item:items) {
			unitComboBox.addItem(item);
		}
		mainPanel.add(unitComboBox, "wrap");
		
		//extensionManager.runScript(context.getProjectManager().getProjectScriptManager(node.getProjectId()), 
		//		script, node.getSelf().toString(),node.getAuxiliaryData());
		
		mainPanel.add(nextButton,"cell 1 13, right");
		
		// Added to set a max and min size so that when the pane was streched it maintained its size and was anchored right PH 6/29/2021
		nextButton.setMaximumSize(ApplicationPropertyEditor.NAV_BUTTON_WITH_TEXT_SIZE);
		nextButton.setMinimumSize(ApplicationPropertyEditor.NAV_BUTTON_WITH_TEXT_SIZE);
		
		nextButton.setHorizontalTextPosition(SwingConstants.LEFT);
		nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doNext();}			
		});
		
		setUI();
		
		// Register for notifications - returns the current setting
		log.infof("%s: adding change listener %s",CLSS,key);
		notificationHandler.addNotificationChangeListener(key,CLSS,this);
	}

	// Fill widgets with current values
	private void setUI() {
		nameField.setText(editor.getApplication().getName());
		String description = model.getProperties().get("Description");
		if( description==null) description="";
		descriptionTextArea.setText(description);
		
		String managed = (String)model.getProperties().get("Managed");
		if( managed==null) managed="0";
		managedCheckBox.setSelected(managed.equals("0")?false:true);
		
		// Set combo boxes to current values
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
		log.infof("%s: removing change listener %s",CLSS,key);
		notificationHandler.removeNotificationChangeListener(key,CLSS);
		save();
	}

	// ======================================= Notification Change Listener ===================================
	@Override
	public void bindingChange(String pname,String binding) {}
	@Override
	public void diagramStateChange(String path, String state) {}
	@Override
	public void nameChange(String name) {}
	@Override
	public void propertyChange(String pname,Object value) {}
	// The value is the aux data of the application. Note that the method is not
	// called on the Swing thread. It gets triggereed as soon as the panel is displayed.
	@Override
	public void valueChange(final QualifiedValue value) {
		if( value==null ) return;
		GeneralPurposeDataContainer container = (GeneralPurposeDataContainer)value.getValue();
		if( container==null) return;
		log.infof("%s.valueChange: new aux data %s for %s",CLSS,container.toString(),editor.getApplication().getName());
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