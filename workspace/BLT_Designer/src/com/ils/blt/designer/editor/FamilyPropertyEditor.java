/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.editor;


import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.designer.NotificationHandler;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayException;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.IgnitionDesigner;
import com.inductiveautomation.ignition.designer.gateway.DTGatewayInterface;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * Display a sliding pane in the property edit window to configure a Family node
 */
public class FamilyPropertyEditor extends AbstractPropertyEditor implements NotificationChangeListener { 
	private final static String CLSS = "FamilyPropertyEditor";
	private static final long serialVersionUID = 2882399376824334427L;
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private static final Dimension DESCRIPTION_AREA_SIZE  = new Dimension(200,160);
	private static final Dimension NUMBER_BOX_SIZE  = new Dimension(50,24);
	private static final Dimension PANEL_SIZE = new Dimension(250,300);
	protected final DesignerContext context;
	private final SerializableFamily family;
	private final String key;
	protected final LoggerEx log;
	private final String provider;
	private final String database;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	private final ApplicationRequestHandler requestHandler;
	private JPanel mainPanel = null;
	private JTextArea descriptionArea;
	private JTextField nameField;
	private JTextField priorityField;
	JTextField uuidField;
	
	public FamilyPropertyEditor(DesignerContext ctx,SerializableFamily fam,ProjectResource res) {
		super(res);
		this.context = ctx;
		this.family = fam;
		this.model = family.getAuxiliaryData();
		this.key = NotificationKey.keyForAuxData(family.getId().toString());
		this.requestHandler = new ApplicationRequestHandler();
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.database = requestHandler.getProductionDatabase();
		this.provider = requestHandler.getProductionTagProvider();
        initialize();
        setUI();
		// Register for notifications
		notificationHandler.addNotificationChangeListener(key,CLSS,this);
	}
	/**
	 * The super class takes care of making a central tabbed pane --- but
	 * we don't want it. Simply put our mainPanel as the content pane.
	 * Here we add the tabs ...
	 * 1) Core attributes
	 * 2) Python hook definitions.
	 */
	private void initialize() {
		model.getProperties().put("FamilyName", family.getName());   // Use as a key when fetching
		mainPanel = createMainPanel();
		add(mainPanel,BorderLayout.CENTER);
		validate();
	}

	public void shutdown() {
		save();
		notificationHandler.removeNotificationChangeListener(key,CLSS);
	}
	
	/**
	 * Create the main data pane as a grid 4 columns wide:
	 *     label | value | label | value
	 *     label | value -- span 3
	 */
	private JPanel createMainPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("fillx", "[right]rel[grow, fill]"));
		panel.setPreferredSize(PANEL_SIZE);

		panel.add(new JLabel("Name:"),"align right");
		nameField = new JTextField();
		nameField.setEditable(false);;
		panel.add(nameField,"span,growx,wrap");

		panel.add(new JLabel("UUID:"),"align right");
		uuidField = new JTextField();
		uuidField.setEditable(false);
		panel.add(uuidField,"span,growx,wrap");
		
		panel.add(new JLabel("Description:"),"gaptop 2,aligny top, align right");
		descriptionArea = new JTextArea();
		JScrollPane scrollPane = new JScrollPane(descriptionArea);
		scrollPane.setPreferredSize(DESCRIPTION_AREA_SIZE);
		panel.add(scrollPane,"gaptop 2,aligny top,spanx,growx,growy,wrap");

		panel.add(new JLabel("Priority:"),"align right");
		priorityField = new JTextField();
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(priorityField,"span,wrap");
		return panel;
	}
	/**
	 * @return the family that we are editing. 
	 */
	public SerializableFamily getFamily() { return family; }

	// Fill widgets with current values
	private void setUI() {
		nameField.setText(family.getName());
		uuidField.setText(family.getId().toString());
		String description = model.getProperties().get("Description");
		if( description==null) description="";
		descriptionArea.setText(description);
		String priority = model.getProperties().get("Priority");
		if( priority==null) priority="0.0";
		priorityField.setText(priority);
	}
	
	// On save we get values from the widgets and place back into the model and database (for external functions).
	private void save(){
		model.getProperties().put("Description",descriptionArea.getText());
		model.getProperties().put("Priority", priorityField.getText());
		log.infof("%s.save():  state = %s",CLSS,family.getState().name());
		saveResource();
		requestHandler.writeAuxData(context.getProject().getId(),getResource().getResourceId(),family.getId().toString(),model,provider, database);
	}
	
	@Override
	public void saveResource() {
		ObjectMapper mapper = new ObjectMapper();
		Project proj = context.getProject();
		try{	
			if( context.requestLock(resource.getResourceId()) ) {
				synchronized(this) {
					byte[] bytes = mapper.writeValueAsBytes(family);
					//log.tracef("%s.run JSON = %s",CLSS,new String(bytes));
					resource.setData(bytes);
					context.updateResource(resource);
					context.updateLock(resource.getResourceId());
					context.releaseLock(resource.getResourceId());
				}
			}
			else {
				log.warnf("%s.saveResource: Failed to obtain lock on resource save (%s)",CLSS,resource.getName());
			}
			// Update the project
			DTGatewayInterface.getInstance().saveProject(IgnitionDesigner.getFrame(), proj, false, "Committing ...");  // Don't publish	
		}
		catch(GatewayException ge) {
			log.warnf("%s.run: Exception saving project %d (%s)",CLSS,proj.getName(),ge.getMessage());
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s.saveResource: Exception serializing application, resource %d (%s)",CLSS,resource.getResourceId(),jpe.getMessage());
		}
	}	

	// ======================================= Notification Change Listener ===================================
	@Override
	public void bindingChange(String pname,String binding) {}
	@Override
	public void diagramStateChange(long resId, String state) {}
	@Override
	public void nameChange(String name) {}
	@Override
	public void propertyChange(String pname,Object value) {}

	// The value is the aux data of the family. Note that the method is not
	// called on the Swing thread. It is called immediately on display of the editor.
	@Override
	public void valueChange(final QualifiedValue value) {
		if( value==null ) return;
		GeneralPurposeDataContainer container = (GeneralPurposeDataContainer)value.getValue();
		if( container==null) return;
		log.infof("%s.valueChange: new aux data for family %s",CLSS,family.getName());
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
