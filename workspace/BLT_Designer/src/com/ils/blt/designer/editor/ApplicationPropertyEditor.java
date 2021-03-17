/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.util.List;
import java.util.Map;

import javax.swing.JPanel;

import com.ils.blt.common.serializable.SerializableApplication;
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
public class ApplicationPropertyEditor extends AbstractPropertyEditor { 
	private static final long serialVersionUID = 2882399376824334427L;
	protected final DesignerContext context;
	private final SerializableApplication sa;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	protected final ILSLogger log;
	protected boolean cancelled = false;
	private final SortedListModel<String> outputListModel;
	
	// Getters
	public ILSLogger getLog() { return log; }

	public ApplicationPropertyEditor(DesignerContext ctx,SerializableApplication app,ProjectResource res) {
		super(res);
		this.log = LogMaker.getLogger(this);
		this.context = ctx;
		this.model = new GeneralPurposeDataContainer();
		this.sa = app;
		this.outputListModel = new SortedListModel<>();
		initialize();
	}
	
	// Add all the tabs to the sliding pane. The controller holds the model data.
	private void initialize() {
		// sub-panes added according to the indexes above:
		add(new HomePane(this));
		OutputEditorPane outputEditor = new OutputEditorPane(this);
		add(new OutputsPane(this,outputEditor));
		add(outputEditor);
		add(new TagSelectorPane(this, outputEditor));
		add(new JPanel());  // a blank pane
		setSelectedPane(ApplicationEditConstants.HOME);
	}

	public GeneralPurposeDataContainer getModel() { return this.model; }
	public SortedListModel getOutputListModel() { return outputListModel; }
	
	
	public void shutdown() {}
	public void setCancelled(boolean flag) { this.cancelled = flag; }
	/*
	 * @return true if the user has selected the "Cancel" button.
	 */
	public boolean isCancelled() { return cancelled; }
	
	public SerializableApplication getApplication() { return sa; }
	
	private void buildOutputListModel(){
		List< Map<String,String> > outputList = model.getMapLists().get("QuantOutputs");
		log.tracef("OutputList: " + outputList);
		if( outputList!=null ) {
			for(Map<String,String> outMap : outputList) {
				log.tracef("Adding " + outMap + " a " + outMap.getClass().getName());
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
}