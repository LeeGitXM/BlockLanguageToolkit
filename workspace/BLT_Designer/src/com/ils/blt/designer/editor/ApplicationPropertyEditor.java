/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JPanel;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.blt.designer.config.OutputsPane;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.SortedListModel;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Display a dialog to configure an Application node
 */
public class ApplicationPropertyEditor extends AbstractPropertyEditor implements FocusListener { 
	private final static String CLSS = "ApplicationPropertyEditor";
	private static final long serialVersionUID = 2882399376824334427L;
	public static final Dimension BUTTON_SIZE  = new Dimension(80,36);
	public static final Dimension COMBO_SIZE  = new Dimension(180,24);
	public static final Dimension EDIT_BUTTON_SIZE  = new Dimension(80,36);

	// Indices for the sub-panes. We add them in this order ...
	public static final int HOME = 0;
	public static final int OUTPUTS = 1;
	public static final int EDITOR = 2;
	public static final int TAGSELECTOR = 3;
	
	protected final DesignerContext context;
	private final SerializableApplication application;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	private final ILSLogger log;
	private final SortedListModel<String> outputListModel;
	
	// Getters
	public ILSLogger getLog() { return log; }

	public ApplicationPropertyEditor(DesignerContext ctx,SerializableApplication app,ProjectResource res) {
		super(res);
		this.log = LogMaker.getLogger(this);
		this.context = ctx;
		this.application = app;
		this.model = app.getAuxiliaryData().clone();
		this.outputListModel = new SortedListModel<>();
		initialize();
	}
	
	// Add all the tabs to the sliding pane. The controller holds the model data.
	private void initialize() {
		model.getProperties().put("Name", application.getName());   // Use as a key when fetching
		// sub-panes added according to the indexes above:
		add(new HomePane(this));
		OutputEditorPane outputEditor = new OutputEditorPane(this);
		add(new OutputsPane(this,outputEditor));
		add(outputEditor);
		add(new TagSelectorPane(this, outputEditor));
		add(new JPanel());  // a blank pane
		setSelectedPane(HOME);
	}

	public GeneralPurposeDataContainer getModel() { return this.model; }
	public SortedListModel getOutputListModel() { return outputListModel; }
	
	@Override
	public void shutdown() {}

	
	public SerializableApplication getApplication() { return application; }
	
	private void buildOutputListModel(){
		List< Map<String,String> > outputList = model.getMapLists().get("QuantOutputs");
		log.tracef("OutputList: " + outputList);
		if( outputList!=null ) {
			for(Map<String,String> outMap : outputList) {
				outputListModel.add(outMap.get("QuantOutput"));
			}
		}
	}
	private void clearOutputListModel(){
		outputListModel.clear();
	}
	
	public void refreshOutputs() {
		clearOutputListModel();
		buildOutputListModel();
	}
	
	// On save we get values from the widgets and place back into the model
	private void save(){
		log.infof("%s.save()",CLSS);
		application.setAuxiliaryData(model);
		ObjectMapper mapper = new ObjectMapper();
		try{
			byte[] bytes = mapper.writeValueAsBytes(application);
			//log.tracef("%s.run JSON = %s",CLSS,new String(bytes));
			resource.setData(bytes);
			if( context.requestLockQuietly(resource.getResourceId()) )  {
				context.updateResource(resource.getResourceId(),resource.getData());   // Force an update
			}
			else {
				log.infof("%s.save: Failed to lock resource",CLSS);
			}
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s.run: Exception serializing application, resource %d (%s)",CLSS,resource.getResourceId(),jpe.getMessage());
		}
	}
		
		// ============================================== Focus listener ==========================================
		@Override
		public void focusGained(FocusEvent event) {
		}
		@Override
		public void focusLost(FocusEvent event) {
			save();
		}
}