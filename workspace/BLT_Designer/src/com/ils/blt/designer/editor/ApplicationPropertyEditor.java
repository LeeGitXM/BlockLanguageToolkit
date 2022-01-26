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
import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.SortedListModel;
import com.inductiveautomation.ignition.common.execution.ExecutionManager;
import com.inductiveautomation.ignition.common.execution.impl.BasicExecutionEngine;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
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
	private final LoggerEx log;
	private final SortedListModel<String> outputKeys;
	private final ApplicationRequestHandler requestHandler;
	private final ExecutionManager executionEngine;

	public ApplicationPropertyEditor(DesignerContext ctx, SerializableApplication app, ProjectResource res) {
		super(res);
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.context = ctx;
		this.application = app;
		this.model = application.getAuxiliaryData();
		this.executionEngine = new BasicExecutionEngine(1,CLSS);
		this.homePanel = new ApplicationHomePane(this);
		this.outputKeys = new SortedListModel<>();
		buildOutputListModel();
		this.requestHandler = new ApplicationRequestHandler();
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
		log.infof("%s: OutputList: %s", CLSS, outputKeys.toString());
		if( outputMapList==null ) {
			outputMapList = new ArrayList<>();
			model.getMapLists().put("QuantOutputs", outputMapList);
		}
		else {
			Integer i = 0;
			for(Map<String,String> outputMap:outputMapList) {
				outputMap.put("QuantOutputId", i.toString());
				log.infof("%s: Output Name: %s (%s) - %s", CLSS, outputMap.get("QuantOutput"), outputMap.get("QuantOutputId"), outputMap.toString());
				outputKeys.add(outputMap.get("QuantOutput"));
				i = i + 1;
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
		log.infof("%s: Saving...", CLSS);
		application.setAuxiliaryData(model);
		ObjectMapper mapper = new ObjectMapper();
		try {
			byte[] bytes = mapper.writeValueAsBytes(application);
			executionEngine.executeOnce(new ResourceUpdateManager(resource,bytes));
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s.saveResource: JSON exception deserializing %s:%s (%s)",CLSS,resource.getResourceId().getProjectName(),
				resource.getResourceId().getResourcePath().getPath().toString());
		}
		
	}
}