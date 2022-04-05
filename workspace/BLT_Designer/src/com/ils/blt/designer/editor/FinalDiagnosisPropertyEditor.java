/**
 *   (c) 2015-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.ResourceBundle;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.ui.DualListBox;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * Display a dialog to configure the properties for a Final Diagnosis.  This includes the outputs used by the 
 * Final Diagnosis.  The list of outputs available for the Final Diagnosis comes from the superior Application.
 * The constants contained herein are defined in designer.properties  (as defined in ConfigurationDialog*)
 * This dialog allows for the display and editing of auxiliary data in the proxy block. There is no extension
 * function interaction until the block is saved as part of a diagram-save.
 */
public class FinalDiagnosisPropertyEditor extends AbstractPropertyEditor implements NotificationChangeListener, PropertyChangeListener, FocusListener {
	private static final long serialVersionUID = 7211480530910862375L;
	private static final String CLSS = "FinalDiagnosisPropertyEditor";
	private static final boolean DEBUG = false;
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private final NodeStatusManager nodeStatusMgr;			// PH 06/30/2021
	private final DiagramTreeNode diagramTreeNode;		// PH 06/30/2021
	private final ApplicationRequestHandler requestHandler;
	private final int DIALOG_HEIGHT = 700;
	private final int DIALOG_WIDTH = 300;
	private final DiagramWorkspace workspace;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private JPanel corePanel = null;
	private JPanel propertiesPanel = null;
	
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels

	protected DualListBox dual;
	
	protected JTextField finalDiagnosisClassField;
	protected JTextField finalDiagnosisUUIDField;
	
	protected JTextField finalDiagnosisLabelField;
	protected JTextField calculationMethodField;
	protected JTextArea textRecommendationArea;
	protected JCheckBox postTextRecommendationCheckBox;
	protected JCheckBox showExplanationWithRecommendationCheckBox;
	protected JTextField priorityField;
	protected JTextField refreshRateField;
	protected JTextField postProcessingCallbackField;
	protected JCheckBox constantCheckBox;
	protected JCheckBox trapBox;
	protected JCheckBox manualMoveAllowedCheckBox;
	protected JTextArea explanationArea;
	protected JTextArea commentArea;
	private final String provider;
	private final String database;
	private final String key;
	protected final LoggerEx log;
	protected static final Dimension DUAL_SCROLL_AREA_SIZE  = new Dimension(250,600);
	protected static final Dimension EXPLANATION_AREA_SIZE  = new Dimension(250,300);
	protected static final Dimension TEXT_RECOMMENDATION_AREA_SIZE  = new Dimension(250,300);
	protected static final Dimension COMMENT_AREA_SIZE  = new Dimension(250,300);
	
	// from configuration dialog
	protected final DesignerContext context;
	protected final ResourceBundle rb;
	protected static final Dimension BUTTON_SIZE  = new Dimension(90,28);
	protected static final Dimension COMBO_SIZE  = new Dimension(120,24);
	protected static final Dimension NAME_BOX_SIZE  = new Dimension(280,24);
	protected static final Dimension NUMBER_BOX_SIZE  = new Dimension(50,24);
	private final UtilityFunctions fcns = new UtilityFunctions();
	protected JTextField nameField;

	//	Now figure out how to get this to refresh when the diagram state (active/disabled) changes
	public FinalDiagnosisPropertyEditor(DesignerContext context, DiagramWorkspace wrkspc, ProcessBlockView blk) {
		this.block = blk;
		this.model = blk.getAuxiliaryData();
		this.workspace = wrkspc;
		this.key = NotificationKey.keyForAuxData(block.getId().toString());
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.requestHandler = new ApplicationRequestHandler();
		this.context = context;
		this.corePanel = createCorePanel(block);
		this.log = LogUtil.getLogger(getClass().getPackageName());
		this.database = requestHandler.getProjectProductionDatabase(context.getProjectName());
		this.provider = requestHandler.getProjectProductionTagProvider(context.getProjectName());
        this.diagram = workspace.getActiveDiagram();
		log.infof("%s: creating a Final Diagnosis Editor", CLSS);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		
		/*
		 * We need to get the application superior to the Final Diagnosis.  The information is in the ProjectTree.
		 *  A final Diagnosis is on a diagram and there is a DiagramTreeNode in the project tree that corresponds to the diagram.
		 *  From that node we can walk up the project tree until we hit an application.  There might be a problem if some 
		 *  knucklehead puts a final diagnosis onto a diagram that is not built under the Application / Family framework.
		 *  NOTE: That would be me --- clc
		 *  The steps are:
		 *  	get the resource tree manager
		 *  	get the node for the diagram 
		 *  	get the Application node in tree superior to the diagram node
		 *  	get the resource that corresponds to the application node
		 *     convert the application resource to a serializable application
		 *  	get the auxiliary data out of the application
		 */
		this.nodeStatusMgr = wrkspc.getNodeStatusManager();
		this.diagramTreeNode = (DiagramTreeNode) nodeStatusMgr.findNode(diagram.getResourceId());
        initialize(block);
        setUI();
		// Register for notifications
		log.tracef("%s: adding notification listener %s", CLSS, key);
		notificationHandler.addNotificationChangeListener(key, CLSS, this);
	
	}
	/**
	 * The super class takes care of making a central tabbed pane --- but
	 * we don't want it. Simply put our mainPanel as the content pane.
	 * Here we add the tabs ... there are no buttons
	 * 1) Core attributes
	 * 2) Python hook definitions.
	 * 
	 * Previously, if the diagram was disabled, then it would build a panel without data even though 
	 * every other block on the diagram could be configured.   I think there was some confusion about
	 * which database to use if the diagram is disabled.  There are 3 states: ACTIVE, ISOLATED, and DISABLED.
	 * Use the production database if the diagram is DISABLED.  Configuring a FD really has nothing to 
	 * do with the state of the diagram.  (Pete - 4/28/2021) 
	 */
	private void initialize(ProcessBlockView blk) {
		//Fix the size for the dual scroll widget
		setLayout(new MigLayout("", "", "[] [] [] [150!] [] []"));
		
		addSeparator(this, "Core");
		
		corePanel = createCorePanel(blk);
		add(corePanel,"grow, spanx, wrap");
		
		addSeparator(this,"QuantOutputs");
		
		dual = new DualListBox();
		dual.setPreferredSize(DUAL_SCROLL_AREA_SIZE);
		dual.addPropertyChangeListener(this);
		add(dual, "gapx 5 5, grow, spanx, wrap");
		
		addSeparator(this,"Properties");
		
		propertiesPanel = createPropertiesPanel();
		add(propertiesPanel,"grow, spanx, push");;
	}
	
	public void shutdown() {
		/*
		 * This is a concrete method for the abstract method defined on AbstractPropertyEditor.  
		 * This is called by the setEditor() method in PropertyEditorFrame, which encapsulates the editor, whenever something in
		 * the project tree is selected or another block on the diagram.
		 */
		log.tracef("%s.shutdown: removing change listener and saving", CLSS);
		notificationHandler.removeNotificationChangeListener(key, CLSS);		
	}
	
	
	/**
	 * Create the main data pane as a grid 2 columns wide:
	 *     label | value
	 */
	private JPanel createCorePanel(ProcessBlockView blk) {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 2", "[para]0[]0[]", "[para]0[]0[]"));
		
		panel.add(new JLabel("Name"),"skip");
		nameField = new JTextField(blk.getName());
		nameField.setEditable(false);
		panel.add(nameField,"growx,pushx");
		nameField.setEditable(true);
		nameField.addFocusListener(this);
		
		panel.add(new JLabel("Class"),"newline,skip");
		finalDiagnosisClassField = new JTextField(blk.getClassName());
		finalDiagnosisClassField.setEditable(false);
		panel.add(finalDiagnosisClassField, "growx,pushx");
		
		panel.add(new JLabel("UUID"),"newline, skip");
		finalDiagnosisUUIDField = new JTextField(blk.getId().toString());
		finalDiagnosisUUIDField.setEditable(false);
		panel.add(finalDiagnosisUUIDField, "growx,pushx");
		return panel;
	}


	/**
	 * This panel holds the "simple" attributes of the block
	 * Create the main data pane as a grid 2 columns wide:
	 *     label | value
	 */
	private JPanel createPropertiesPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("fillx", "[right]rel[grow, fill]"));

		panel.add(createLabel("FinalDiagnosis.Label"),"gaptop 2,aligny top, align right");
		finalDiagnosisLabelField = createTextField("FinalDiagnosis.Label.Desc","");
		panel.add(finalDiagnosisLabelField,"span, growx, wrap");
		
		panel.add(createLabel("FinalDiagnosis.Comment"),"gaptop 2,aligny top, align right");
		commentArea = createTextArea("FinalDiagnosis.Comment.Desc","");
		JScrollPane commentScrollPane = new JScrollPane(commentArea);
		commentScrollPane.setPreferredSize(COMMENT_AREA_SIZE);
		panel.add(commentScrollPane,"spanx, wrap");
		
		panel.add(createLabel("FinalDiagnosis.Explanation"),"gaptop 2, aligny top, align right");
		explanationArea = createTextArea("FinalDiagnosis.Explanation.Desc","");
		JScrollPane explanationScrollPane = new JScrollPane(explanationArea);
		explanationScrollPane.setPreferredSize(EXPLANATION_AREA_SIZE);
		panel.add(explanationScrollPane,"growx,growy,wrap");
		
		panel.add(createLabel("FinalDiagnosis.TextRecommendation"),"gaptop 2, aligny top, align right");
		textRecommendationArea = createTextArea("FinalDiagnosis.TextRecommendation.Desc","");
		JScrollPane textRecommendationScrollPane = new JScrollPane(textRecommendationArea);
		textRecommendationScrollPane.setPreferredSize(TEXT_RECOMMENDATION_AREA_SIZE);
		panel.add(textRecommendationScrollPane,"growx,growy,wrap");
		
		panel.add(createLabel("FinalDiagnosis.CalcMethod"),"gaptop 2, aligny top, align right");
		calculationMethodField = createTextField("FinalDiagnosis.CalcMethod.Desc","");
		panel.add(calculationMethodField,"span");

		panel.add(createLabel("FinalDiagnosis.Priority"),"gaptop 2, aligny top, align right");
		priorityField = createTextField("FinalDiagnosis.Priority.Desc","");
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(priorityField,"wrap");
		
		panel.add(createLabel("FinalDiagnosis.RefreshRate"),"gaptop 2, aligny top, align right");
		refreshRateField = createTextField("FinalDiagnosis.RefreshRate.Desc","");
		refreshRateField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(refreshRateField,"wrap");
		
		panel.add(createLabel("FinalDiagnosis.PostProcessingCallback"),"gaptop 2,aligny top");
		postProcessingCallbackField = createTextField("FinalDiagnosis.PostProcessingCallback.Desc","");
		panel.add(postProcessingCallbackField,"growx,wrap");
		
		constantCheckBox = createCheckBox("FinalDiagnosis.Constant.Desc",false);
		panel.add(constantCheckBox,"alignx right");
		panel.add(createLabel("FinalDiagnosis.Constant"),"gaptop 2,aligny top,wrap");
		
		postTextRecommendationCheckBox = createCheckBox("FinalDiagnosis.PostTextRecommendation.Desc",false);
		panel.add(postTextRecommendationCheckBox,"alignx right");
		panel.add(createLabel("FinalDiagnosis.PostTextRecommendation"),"gaptop 2, aligny top,wrap");
		
		showExplanationWithRecommendationCheckBox = createCheckBox("FinalDiagnosis.ShowExplanationWithRecommendation.Desc",false);
		panel.add(showExplanationWithRecommendationCheckBox,"alignx right");
		panel.add(createLabel("FinalDiagnosis.ShowExplanationWithRecommendation"),"gaptop 2,aligny top,wrap");
		
		manualMoveAllowedCheckBox = createCheckBox("FinalDiagnosis.ManualMoveAllowed.Desc",false);
		panel.add(manualMoveAllowedCheckBox,"alignx right");
		panel.add(createLabel("FinalDiagnosis.ManualMoveAllowed"),"gaptop 2,aligny top,wrap");
		
		trapBox = createCheckBox("FinalDiagnosis.TrapInsignificant.Desc",false);
		panel.add(trapBox,"alignx right");
		panel.add(createLabel("FinalDiagnosis.TrapInsignificant"),"gaptop 2,aligny top");
		return panel;
	}

	private void setUI() {
		Map<String,String> properties = model.getProperties();
		
		/*
		 * Populating the Dual List of Quant Outputs is a little tricky / clever.
		 * The list of all available outs comes from the application superior to the final diagnosis.
		 * The Final Diagnosis has a list of outputs already configured as being used by the Final diagnosis, this list
		 * is known as q1 and goes into the list on the right.  The list on the left, known as q0, is the list of all
		 * outputs minus the inputs that are already in use.
		 */
		
		List<String> q1 = model.getLists().get("OutputsInUse");
		if( q1==null ) q1 = new ArrayList<>();
		dual.setDestinationElements(q1);
		
		// Get the list of quant outputs that have been defined for the application
		List< Map<String,String> > outputMapList = new ArrayList<>();      // TODO
		
		// Convert the list of output maps to a list of strings (output names)
		List<String> q0 = new ArrayList<>();
		if(outputMapList!=null ) {
			for(Map<String,String> outmap:outputMapList) {
				q0.add(outmap.get("QuantOutput"));
			}
			log.tracef("Total list of outputs: %s", q0);
		}
		
		// Remove the outputs that are already in use
		for( String inUse:q1) {
			q0.remove(inUse);
		}
		
		log.tracef("Left list: %s", q0);
		log.tracef("Right list: %s", q1);

		dual.setSourceElements(q0);

		String label = properties.get("FinalDiagnosisLabel");
		if( label==null) label="";
		finalDiagnosisLabelField.setText(label);
		
		String comment = (String)properties.get("Comment");
		if( comment==null) comment="";
		commentArea .setText(comment);
		
		String explanation = (String)properties.get("Explanation");
		if( explanation==null) explanation="";
		explanationArea.setText(explanation);
		
		String recommendation = (String)properties.get("TextRecommendation");
		if( recommendation==null) recommendation="";
		textRecommendationArea.setText(recommendation);
		
		String calculationMethod = properties.get("CalculationMethod");
		if( calculationMethod==null) calculationMethod="";
		calculationMethodField.setText(calculationMethod);
		
		String priority = (String)properties.get("Priority");
		if( priority==null) priority="";
		priorityField.setText(priority);
		
		String rate = (String)properties.get("RefreshRate");
		if( rate==null) rate="";
		refreshRateField.setText(rate);
		
		String method = (String)properties.get("PostProcessingCallback");
		if( method==null) method="";
		postProcessingCallbackField.setText(method);
		
		String constantValue = properties.get("Constant");
		if( constantValue==null) constantValue="0";
		constantCheckBox.setSelected(constantValue.equalsIgnoreCase("1"));
		
		String postTextRec = (String)properties.get("PostTextRecommendation");
		if( postTextRec==null) postTextRec="0";
		postTextRecommendationCheckBox.setSelected(postTextRec.equals("0")?false:true);
		
		String showExplanation = (String)properties.get("ShowExplanationWithRecommendation");
		if( showExplanation==null) showExplanation="0";
		showExplanationWithRecommendationCheckBox.setSelected(showExplanation.equals("0")?false:true);
		
		String manualMoveAllowed = properties.get("ManualMoveAllowed");
		if( manualMoveAllowed==null) manualMoveAllowed="0";
		manualMoveAllowedCheckBox.setSelected(manualMoveAllowed.equalsIgnoreCase("1"));
		
		String tf = (String)properties.get("TrapInsignificantRecommendations");
		if( tf==null) tf="0";
		trapBox.setSelected(tf.equals("0")?false:true);
	}
	

	/*
	 * Copy values from the UI back into the block's aux data.
	 * The block is a view valid only while the diagram is displayed. Once the diagram is saved 
	 * these values are written to the database.
	 * If this is working correctly, you should be able to select a FD, edit something, select a different block, select the FD again and you 
	 * will see the edit, quit the designer (without a File->Save), open designer, select the same block, and you will not see the change. 
	 */
	private void save(){
		log.tracef("%s:save() copying the AUX data back into the block's aux data...",CLSS);
		model.getProperties().put("Constant", (constantCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("ManualMoveAllowed", (manualMoveAllowedCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("CalculationMethod",calculationMethodField.getText());
		model.getProperties().put("FinalDiagnosisLabel",finalDiagnosisLabelField.getText());
		model.getProperties().put("TextRecommendation", textRecommendationArea.getText());
		model.getProperties().put("Comment", commentArea.getText());
		model.getProperties().put("Explanation", explanationArea.getText());
		model.getProperties().put("PostTextRecommendation", (postTextRecommendationCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("ShowExplanationWithRecommendation", (showExplanationWithRecommendationCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("Priority", priorityField.getText());
		model.getProperties().put("RefreshRate", refreshRateField.getText());
		model.getProperties().put("PostProcessingCallback", postProcessingCallbackField.getText());
		model.getProperties().put("TrapInsignificantRecommendations", (trapBox.isSelected()?"1":"0"));
		
		List<String> inUseList = dual.getDestinations();
		model.getLists().put("OutputsInUse",inUseList);
		block.setAuxiliaryData(model);
		workspace.setDiagramDirty(workspace.getActiveDiagram());
	}
	
	/*
	 * Create a text area for editing the description. This should be placed inside a
	 * scroll pane.
	 */
	protected JTextArea createTextArea(String bundle,String text) {	
		final JTextArea area = new JTextArea(text);
		area.setEditable(true);
		area.setLineWrap(true);
		area.setWrapStyleWord(true);
		area.setToolTipText(rb.getString(bundle));
		area.addFocusListener(this);
		return area;
	}
	
	/**
	 * Add a separator to a panel using Mig layout
	 */
	protected JLabel addSeparator(JPanel panel, String text) {
		JSeparator separator = new JSeparator();
		JLabel label = new JLabel(text);
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		panel.add(label, "split 2,span");
		panel.add(separator, "growx,wrap");
		return label;
	}
	/**
	 * Create a text field for editing strings
	 */
	protected JTextField createTextField(String bundle,String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
		field.addFocusListener(this);
		return field;
	}
	/**
	 * Create a text field for editing floating point values
	 */
	protected JFormattedTextField createDoubleField(String bundle,String text) {	
		final JFormattedTextField field = new JFormattedTextField(NumberFormat.getInstance());
		double dbl = fcns.coerceToDouble(text);
		field.setValue(dbl);
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
		field.addFocusListener(this);
		return field;
	}
	/**
	 * Create a text field for editing integer values
	 */
	protected JFormattedTextField createIntegerField(String bundle,String text) {	
		final JFormattedTextField field = new JFormattedTextField(NumberFormat.getInstance());
		int i = fcns.coerceToInteger(text);
		field.setValue(i);
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
		field.addFocusListener(this);
		return field;
	}

	/**
	 * Create a combo box for "active" state
	 */
	protected JComboBox<String> createActiveStateCombo(String bundle,ActiveState state) {
		JComboBox<String> box = new JComboBox<String>();
		for(ActiveState as : ActiveState.values()) {
			box.addItem(as.name());
		}
		box.setToolTipText(rb.getString(bundle));
		box.setSelectedItem(state.name());
		box.setPreferredSize(COMBO_SIZE);
		box.addFocusListener(this);
		return box;
	}
	/**
	 * Create a new checkbox. The text is the bundle key for the tooltip.
	 */
	protected JCheckBox createCheckBox(String bundle,boolean initialValue) {
		JCheckBox box = new JCheckBox();
		box.setToolTipText(rb.getString(bundle)+": ");
		box.setSelected(initialValue);
		box.setText("");     // Don't use the standard label, it's on the wrong side.
		box.addFocusListener(this);
		return box;
	}
	
	/**
	 * Create a new label. The text is the bundle key.
	 */
	protected JLabel createLabel(String bundle) {
		JLabel label = new JLabel(rb.getString(bundle)+": ");
		return label;
	}
	
	
	// FinalDiagnoses must have unique names.
	public void saveName() {
		if( !nameField.getText().equalsIgnoreCase(block.getName()) ) {
			BLTDesignerHook hook = (BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID);
			block.setName(nameField.getText());
			workspace.setDiagramDirty(workspace.getActiveDiagram());
		}
	}
	
	// ============================================== PropertyChange listener ==========================================
	@Override
	public void propertyChange(PropertyChangeEvent event) {
		/*
		 * This is called when the editor is built - I think the purpose is to handle changes made via a tag binding where the tag changes value while
		 * the editor is open.  Not sure why the code below seems to single out the dual list box. PAH 10/6/21 (This is just a theory)
		 */
		if (event.getPropertyName().equalsIgnoreCase(DualListBox.PROPERTY_CHANGE_UPDATE)) {
			log.infof("%s: in propertyChange()",CLSS);
			save();
		}
	}	
	// ======================================= Notification Change Listener ===================================
	@Override
	public void bindingChange(String pname,String binding) {}
	@Override
	public void diagramStateChange(String path, String state) {}
	@Override
	public void nameChange(String name) {
		log.tracef("%s.nameChange()", CLSS);
	}
	@Override
	public void propertyChange(String pname,Object value) {
		log.tracef("%s.propertyChange()", CLSS);
	}

	// The value is the aux data of the application. Note that the method is not
	// called on the Swing thread
	@Override
	public void valueChange(final QualifiedValue value) {
		log.tracef("%s.valueChange: new aux data for %s",CLSS,block.getName());
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

	// ======================================= Focus Change Listener ===================================
	@Override
	public void focusGained(FocusEvent e) {
		log.tracef("%s: focusGained()...",CLSS);
	}

	@Override
	public void focusLost(FocusEvent event) {
		log.infof("%s: focusLost() for a %s...", CLSS, event.getSource().getClass().getName());
		
		Map<String,String> properties = model.getProperties();
		Object source = event.getSource();
		/*
		 * If the value was changed then set the diagram dirty
		 */
		if(source.equals(finalDiagnosisLabelField) ) {
			if( !finalDiagnosisLabelField.getText().equals(properties.get("FinalDiagnosisLabel")) ){
				log.infof("--------  THE LABEL HAS BEEN CHANGED -------------");
				save();
			}
		}
		else if(source.equals(calculationMethodField) ) {
			if( !calculationMethodField.getText().equals(properties.get("CalculationMethod")) ){
				log.infof("--------  THE CALCULATION METHOD HAS BEEN CHANGED -------------");
				save();
			}
		}
		else if (source.equals(postProcessingCallbackField) ) {
			if( !postProcessingCallbackField.getText().equals(properties.get("PostProcessingCallback")) ){
				log.infof("--------  THE POST PROCESSING CALLBACK HAS BEEN CHANGED -------------");
				save();
			}
		}
		else if (source.equals(priorityField) ) {
			if( !priorityField.getText().equals(properties.get("Priority")) ){
				log.tracef("--------  THE PRIORITY HAS BEEN CHANGED -------------");
				save();
			}
		}
		else if (source.equals(refreshRateField) ) {
			if( !refreshRateField.getText().equals(properties.get("RefreshRate")) ){
				log.tracef("--------  THE REFRESH RATE HAS BEEN CHANGED -------------");
				save();
			}
		}
		else if( source.equals(nameField) ) {
			if( !nameField.getText().equals(block.getName())){
				log.tracef("--------  THE FINAL DIAGNOSIS NAME HAS BEEN CHANGED -------------");
				log.tracef("Old name: %s", block.getName());
				log.tracef("New name: %s", nameField.getText());
				saveName();
			}
		}
		else if( source.equals(textRecommendationArea) ) {
			if( !textRecommendationArea.getText().equals(properties.get("TextRecommendation")) ){
				log.tracef("--------  THE TEXT RECOMMENDATION HAS BEEN CHANGED -------------");
				save();
			}
		}
		else if (source.equals(explanationArea) ) {
			if( !explanationArea.getText().equals(properties.get("Explanation"))){
				log.tracef("--------  THE EXPLANATION HAS BEEN CHANGED -------------");
				save();
			}
		}
		else if( source.equals(commentArea) ) {
			if( !commentArea.getText().equals(properties.get("Comment"))) {
				log.tracef("--------  THE COMMENT HAS BEEN CHANGED -------------");
				save();
			}
		}
		else if( source.equals(constantCheckBox) ) {
			if( !(constantCheckBox.isSelected()?"1":"0").equals(properties.get("Constant"))){
				log.infof("--------  THE CONSTANT CHECK BOX HAS BEEN CHANGED -------------");				
				save();
			}
		}
		else if (source.equals(postTextRecommendationCheckBox) ) {
			if( !(postTextRecommendationCheckBox.isSelected()?"1":"0").equals(properties.get("PostTextRecommendation"))){
				log.infof("--------  THE POST TEXT RECOMMENDATION CHECK BOX HAS BEEN CHANGED -------------");				
				save();
			}
		}
		else if( source.equals(showExplanationWithRecommendationCheckBox) ) {
			if( !(showExplanationWithRecommendationCheckBox.isSelected()?"1":"0").equals(properties.get("ShowExplanationWithRecommendation"))){
				log.infof("--------  THE SHOW EXPLANATION CHECK BOX HAS BEEN CHANGED -------------");				
				save();
			}
		}
		else if( source.equals(manualMoveAllowedCheckBox) ) {
			if( !(manualMoveAllowedCheckBox.isSelected()?"1":"0").equals(properties.get("ManualMoveAllowed"))){
				log.infof("--------  THE MANUAL MOVE ALLOWED CHECK BOX HAS BEEN CHANGED -------------");				
				save();
			}
		}
		else if( source.equals(trapBox) ) {
			if( !(trapBox.isSelected()?"1":"0").equals(properties.get("TrapInsignificantRecommendations"))){
				log.infof("--------  THE TRAP INSIGNIFICANT RECOMMENDATIONS CHECK BOX HAS BEEN CHANGED -------------");				
				save();
			}			
		}
	}
}
