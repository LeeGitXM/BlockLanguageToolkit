/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Dimension;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.swing.JPanel;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.SortedListModel;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.client.gateway_interface.GatewayException;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.designer.IgnitionDesigner;
import com.inductiveautomation.ignition.designer.gateway.DTGatewayInterface;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Display a dialog to configure an Application node
 */
public class ApplicationPropertyEditor extends AbstractPropertyEditor { 
	private final static String CLSS = "ApplicationPropertyEditor";
	private static final long serialVersionUID = 2882399376824334427L;
	public static final Dimension BUTTON_SIZE = new Dimension(80,36);   // 16x16
	public static final Dimension NAV_BUTTON_SIZE = new Dimension(60,40);
	public static final Dimension NAV_BUTTON_WITH_TEXT_SIZE = new Dimension(120,40);
	public static final Dimension COMBO_SIZE  = new Dimension(120,24);
	public static final Dimension EDIT_BUTTON_SIZE  = new Dimension(60,36);
	public static final Dimension PANEL_SIZE  = new Dimension(350,350);

	// Indices for the sub-panes. We add them in this order ...
	public static final int HOME = 0;
	public static final int OUTPUTS = 1;
	public static final int EDITOR = 2;
	public static final int TAGSELECTOR = 3;
	
	protected final DesignerContext context;
	private final SerializableApplication application;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	private final ApplicationHomePane homePanel;
	private final ILSLogger log;
	private final SortedListModel<String> outputKeys;

	public ApplicationPropertyEditor(DesignerContext ctx, SerializableApplication app, ProjectResource res) {
		super(res);
		this.log = LogMaker.getLogger(this);
		this.context = ctx;
		this.application = app;
		this.model = application.getAuxiliaryData();
		this.homePanel = new ApplicationHomePane(this);
		this.outputKeys = new SortedListModel<>();
		buildOutputListModel();
		initialize();
	}
	
	// Add all the tabs to the sliding pane. The controller holds the model data.
	private void initialize() {
		model.getProperties().put("ApplicationName", application.getName());   // Use as a key when fetching
		// sub-panes added according to the indexes above:
		add(homePanel);
		OutputEditorPane outputEditor = new OutputEditorPane(this);
		add(new OutputsPane(this,outputEditor));
		add(outputEditor);
		add(new TagSelectorPane(this, outputEditor));
		add(new JPanel());  // a blank pane
		setSelectedPane(HOME);
	}
	public SerializableApplication getApplication() { return application; }
	public DesignerContext getContext() { return this.context; }
	public GeneralPurposeDataContainer getModel() { return this.model; }
	public SortedListModel<String> getOutputKeys() { return this.outputKeys; }
	
	@Override
	public void shutdown() {
		homePanel.shutdown();
	}

	/*
	 * Guarantee that the structure of the output list model is in place. Create a sorted list model, 
	 * a string for every output map. This allows us to iterate over lists in alphabetical order.
	 */
	private void buildOutputListModel(){
		List< Map<String,String> > outputMapList = model.getMapLists().get("QuantOutputs");
		log.tracef("OutputList: " + outputKeys);
		if( outputMapList==null ) {
			outputMapList = new ArrayList<>();
			model.getMapLists().put("QuantOutputs", outputMapList);
		}
		else {
			for(Map<String,String> outmap:outputMapList) {
				outputKeys.add(outmap.get("QuantOutput"));
			}
		}
	}
	private void clearOutputKeyList(){
		outputKeys.clear();
	}
	
	public void refreshOutputs() {
		clearOutputKeyList();
		buildOutputListModel();
	}
	
	// This class does not represent a panel. This serializes the model 
	// callable from all panels. Write to the project resource without
	// updating the entire project. Trigger a notification.
	@Override
	public void saveResource() {
		application.setAuxiliaryData(model);
		ObjectMapper mapper = new ObjectMapper();
		Project proj = context.getProject();
		try{		
			if( context.requestLock(resource.getResourceId()) ) {
				synchronized(this) {
					byte[] bytes = mapper.writeValueAsBytes(application);
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
}