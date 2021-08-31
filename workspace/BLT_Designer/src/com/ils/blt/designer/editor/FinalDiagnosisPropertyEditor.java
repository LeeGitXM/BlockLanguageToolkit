/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.designer.BLTDesignerHook;
import com.ils.blt.designer.NodeStatusManager;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.editor.PropertyPanel.EditableField;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.ils.common.ui.DualListBox;
import com.inductiveautomation.ignition.client.util.gui.ErrorUtil;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;

import net.miginfocom.swing.MigLayout;

/**
 * Display a dialog to configure the properties for a Final Diagnosis.  This includes the outputs used by the 
 * Final Diagnosis.  The list of outputs available for the Final Diagnosis comes from the superior Application.
 * The constants contained herein are defined in designer.properties  (as defined in ConfigurationDialog*)
 * This dialog allows for the display and editing of auxiliary data in the proxy block. There is no extension
 * function interaction until the block is saved as part of a diagram-save.
 */
public class FinalDiagnosisPropertyEditor extends AbstractPropertyEditor implements NotificationChangeListener, PropertyChangeListener {
	private static final long serialVersionUID = 7211480530910862375L;
	private static final String CLSS = "FinalDiagnosisPanel";
	private static final boolean DEBUG = true;
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private final NodeStatusManager nodeStatusMgr;			// PH 06/30/2021
	private final DiagramTreeNode diagramTreeNode;		// PH 06/30/2021
	private final GeneralPurposeTreeNode appNode;			// PH 06/30/2021
	private final ProjectResource applicationResource;		// PH 06/30/2021
	private final ApplicationRequestHandler requestHandler;
	private final int DIALOG_HEIGHT = 700;
	private final int DIALOG_WIDTH = 300;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private BasicEditPanel mainPanel = null;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	private final GeneralPurposeDataContainer appModel;           // Data container operated on by panels PH 06/30/2021
	protected DualListBox dual;
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
	protected final ILSLogger log;
	protected static final Dimension EXPLANATIION_AREA_SIZE  = new Dimension(250,300);
	protected static final Dimension TEXT_RECOMMENDATION_AREA_SIZE  = new Dimension(250,300);
	protected static final Dimension COMMENT_AREA_SIZE  = new Dimension(250,300);
	private final CorePropertyPanel corePanel;
	
	// from configuration dialog
	protected final DesignerContext context;
	protected final ResourceBundle rb;
	protected static final Dimension BUTTON_SIZE  = new Dimension(90,28);
	protected static final Dimension COMBO_SIZE  = new Dimension(120,24);
	protected static final Dimension DESCRIPTION_AREA_SIZE  = new Dimension(250,160);
	protected static final Dimension NAME_BOX_SIZE  = new Dimension(280,24);
	protected static final Dimension NUMBER_BOX_SIZE  = new Dimension(50,24);
	private final UtilityFunctions fcns = new UtilityFunctions();
	protected JTextField nameField;

	//	Now figure out how to get this to refresh when the diagram state (active/disabled) changes
	public FinalDiagnosisPropertyEditor(DesignerContext context, DiagramWorkspace wrkspc, ProcessBlockView blk) {
		this.block = blk;
		this.model = blk.getAuxiliaryData();
		this.key = NotificationKey.keyForAuxData(block.getId().toString());
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.requestHandler = new ApplicationRequestHandler();
		this.context = context;
        this.diagram = wrkspc.getActiveDiagram();
		this.corePanel = new CorePropertyPanel(this, block);
		this.log = LogMaker.getLogger(this);
		this.database = requestHandler.getProductionDatabase();
		this.provider = requestHandler.getProductionTagProvider();
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		
		/*
		 * We need to get the application superior to the Final Diagnosis.  The information is in the ProjectTree.
		 *  A final Diagnosis is on a diagram and there is a DiagramTreeNode in the project tree that corresponds to the diagram.
		 *  From that node we can walk up the project tree until we hit an application.  There might be a problem if some 
		 *  knucklehead puts a final diagnosis onto a diagram that is not built under the Application / Family framework.
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
		this.appNode = this.diagramTreeNode.getApplicationTreeNode();
		if (this.appNode == null) {
			log.errorf("**** ERROR APPLICATION NOT FOUND ****");
			// Need to somehow bail here and let the user know they are screwed!
		}
		this.applicationResource = appNode.getProjectResource();
		
		// Somehow I need to get the serializable application, I have the tree node that represents the 
		// application and I have the resource for the application.  
		SerializableApplication sap = this.appNode.deserializeApplication(this.applicationResource);
		this.appModel = sap.getAuxiliaryData();
		
        initialize();
        setUI();
		// Register for notifications
		if (DEBUG) log.infof("%s: adding notification listener %s", CLSS, key);
		notificationHandler.addNotificationChangeListener(key, CLSS, this);
	}

	/**
	 * The super class takes care of making a central tabbed pane --- but
	 * we don't want it. Simply put our mainPanel as the content pane.
	 * Here we add the tabs ... there are no buttons
	 * 1) Core attributes
	 * 2) Python hook definitions.
	 */
	private void initialize() {
		setLayout(new MigLayout("top,flowy,ins 2,gapy 0:10:15", "", "[top]0[]"));
		//setLayout(new MigLayout("fillx", "[right]rel[grow, fill]"));
		//mainPanel.setLayout(new MigLayout("fillx", "[right]rel[grow, fill]"));
		add(corePanel,"grow,push");
		
		/*
		 * Previously, if the diagram was disabled, then it would build a panel without data even though 
		 * every other block on the diagram could be configured.   I think there was some confusion about
		 * which database to use if the diagram is disabled.  There are 3 states: ACTIVE, ISOLATED, and DISABLED.
		 * Use the production database if the diagram is DISABLED.  Configuring a FD really has nothing to 
		 * do with the state of the diagram.  (Pete - 4/28/2021) 
		 */
		
		mainPanel = createMainPanel();
		add(mainPanel,"grow, spanx, push");;
	}
	
	public void shutdown() {
		notificationHandler.removeNotificationChangeListener(key,CLSS);
		save();
		requestHandler.writeAuxData(context.getProject().getId(), diagram.getResourceId(), block.getId().toString(), model, provider, database);
		if (DEBUG) log.infof("%s.shutdown: writing aux data",CLSS);
	}
	
	private BasicEditPanel createMainPanel() {	
		// The internal panel has two panes
		// - one for the dual list box, the other for the remaining attributes
		//setLayout(new BorderLayout());
		mainPanel = new BasicEditPanel(this);
		mainPanel.setLayout(new MigLayout("ins 2,fill","[][]","[][growprio 60,150:150:2000][]"));
		//mainPanel.setLayout(new MigLayout("fillx", "[right]rel[grow, fill]"));
		//panel.setLayout(new MigLayout("fillx", "[right]rel[grow, fill]"));
		
		mainPanel.addSeparator(mainPanel,"QuantOutputs");
		dual = new DualListBox();

		dual.addPropertyChangeListener(this);
		mainPanel.add(dual, "gapx 5 5,grow,wrap");
		mainPanel.add(createPropertiesPanel(),"grow,wrap");
		return mainPanel;
	}
	
	/**
	 * This panel holds the "simple" attributes of the block
	 * @return
	 */
	/**
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
		//panel.add(commentScrollPane,"growx,growy,wrap");
		panel.add(commentScrollPane,"spanx, wrap");
		
		panel.add(createLabel("FinalDiagnosis.Explanation"),"gaptop 2, aligny top, align right");
		explanationArea = createTextArea("FinalDiagnosis.Explanation.Desc","");
		JScrollPane explanationScrollPane = new JScrollPane(explanationArea);
		explanationScrollPane.setPreferredSize(EXPLANATIION_AREA_SIZE);
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
		List< Map<String,String> > outputMapList = appModel.getMapLists().get("QuantOutputs");
		if (DEBUG) log.infof("Application Output Map List: %s", outputMapList);
		
		// Convert the list of output maps to a list of strings (output names)
		List<String> q0 = new ArrayList<>();
		for(Map<String,String> outmap:outputMapList) {
			q0.add(outmap.get("QuantOutput"));
		}
		if (DEBUG) log.infof("Total list of outputs: %s", q0);
		
		// Remove the outputs that are already in use
		for( String inUse:q1) {
			q0.remove(inUse);
		}
		
		if (DEBUG) log.infof("Left list: %s", q0);
		if (DEBUG) log.infof("Right list: %s", q1);

		dual.setSourceElements(q0);

		String method = properties.get("FinalDiagnosisLabel");
		if( method==null) method="";
		finalDiagnosisLabelField.setText(method);
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
		method = (String)properties.get("PostProcessingCallback");
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
	
	/**
	 * Create a panel for core properties: 
	 * name, class, UUID
	 */
	public class CorePropertyPanel extends BasicEditPanel implements FocusListener {
		private static final long serialVersionUID = -7849105885687872683L;
		private static final String columnConstraints = "[para]0[]0[]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "[para]0[]0[]";

		public CorePropertyPanel(AbstractPropertyEditor edtr,ProcessBlockView blk) {
			super(edtr);
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
			addSeparator(this,"Core");
			add(new JLabel("Name"),"skip");
			nameField = createTextField(blk.getName());
			add(nameField,"growx,pushx");
			nameField.setEditable(true);
			nameField.addFocusListener(this);
			add(new JLabel("Class"),"newline,skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(new JLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
		}
		
		// FinalDiagnoses must have unique names.
		public void saveName() {
			if( !nameField.getText().equalsIgnoreCase(block.getName()) ) {
				BLTDesignerHook hook = (BLTDesignerHook)context.getModule(BLTProperties.MODULE_ID);
				String msg = hook.scanForDiagnosisNameConflicts(diagram, nameField.getText());// send name and block
				if (msg != null && msg.length() > 1) {
					log.errorf("Naming error: " + msg);
					ErrorUtil.showError("Naming error, duplicate diagnosis name: " + msg);
					return;  // abort save
					
				}
				block.setName(nameField.getText());
			}
		}
		
		public void updateCorePanel(int tab,ProcessBlockView blk) {
			
		}
		// ============================================== Focus listener ==========================================
		@Override
		public void focusGained(FocusEvent event) {
			if (DEBUG) log.infof("%s: focusGained()...",CLSS);
		}
		@Override
		public void focusLost(FocusEvent event) {
			if (DEBUG) log.infof("%s: focusLost()...",CLSS);
			if( event.getSource() instanceof EditableField ) {
				log.infof("%s.focusLost(): %s", CLSS,event.getSource().getClass().getName());
				EditableField field = (EditableField)event.getSource();
				BlockProperty prop = field.getProperty();
				if( DEBUG ) log.infof("%s.focusLost(): %s (%s:%s)", CLSS, prop.getName(), prop.getType().name(), prop.getBindingType().name());
				// If there is a value change, then update the property (or binding)
				//updatePropertyForField(field,false);
			}
			saveName();
		}
	}
	

	// Copy the FinalDiagnosis auxiliary data back into the block's aux data
	private void save(){
		if (DEBUG) log.infof("%s:save() copying the AUX data back into the block's aux data...",CLSS);
		model.getProperties().put("Constant", (constantCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("ManualMoveAllowed", (manualMoveAllowedCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("CalculationMethod",calculationMethodField.getText());
		model.getProperties().put("FinalDiagnosisLabel",finalDiagnosisLabelField.getText());
		model.getProperties().put("TextRecommendation", textRecommendationArea.getText());
		model.getProperties().put("Comment", commentArea.getText());
		if (DEBUG) log.infof("Comment: %s", commentArea.getText());
		model.getProperties().put("Explanation", explanationArea.getText());
		model.getProperties().put("PostTextRecommendation", (postTextRecommendationCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("ShowExplanationWithRecommendation", (showExplanationWithRecommendationCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("Priority", priorityField.getText());
		model.getProperties().put("RefreshRate", refreshRateField.getText());
		model.getProperties().put("PostProcessingCallback", postProcessingCallbackField.getText());
		model.getProperties().put("TrapInsignificantRecommendations", (trapBox.isSelected()?"1":"0"));
		
		List<String> inUseList = dual.getDestinations();
		model.getLists().put("OutputsInUse",inUseList);
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
		//area.addFocusListener(l);
		return area;
	}
	/**
	 * Create a text field for editing strings
	 */
	protected JTextField createTextField(String bundle,String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
		return field;
	}
	/**
	 * Create a text field for editing floating point values
	 */
	protected JFormattedTextField createDoubleField(String bundle,String text) {	
		final JFormattedTextField field = new JFormattedTextField(NumberFormat.getInstance());
		double dbl = fcns.coerceToDouble(text);
		field.setValue(new Double(dbl));
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
		return field;
	}
	/**
	 * Create a text field for editing integer values
	 */
	protected JFormattedTextField createIntegerField(String bundle,String text) {	
		final JFormattedTextField field = new JFormattedTextField(NumberFormat.getInstance());
		int i = fcns.coerceToInteger(text);
		field.setValue(new Integer(i));
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(rb.getString(bundle));
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
		return box;
	}
	
	/**
	 * Create a new label. The text is the bundle key.
	 */
	protected JLabel createLabel(String bundle) {
		JLabel label = new JLabel(rb.getString(bundle)+": ");
		return label;
	}
	

	// ============================================== PropertyChange listener ==========================================
	@Override
	public void propertyChange(PropertyChangeEvent event) {
		if (DEBUG) log.infof("%s: in propertyChange()",CLSS);
		if (event.getPropertyName().equalsIgnoreCase(DualListBox.PROPERTY_CHANGE_UPDATE)) {
			save();
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

	// The value is the aux data of the application. Note that the method is not
	// called on the Swing thread
	@Override
	public void valueChange(final QualifiedValue value) {
		log.infof("%s.valueChange: new aux data for %s",CLSS,block.getName());
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
