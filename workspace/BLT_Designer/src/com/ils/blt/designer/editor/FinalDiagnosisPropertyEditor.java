/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
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
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.ils.common.ui.DualListBox;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * Display a dialog to configure the outputs available for a Final Diagnosis.
 * The constants contained herein are defined in designer.properties  (as defined in ConfigurationDialog*)
 * This dialog allows for the display and editing of auxiliary data in the proxy block. There is no extension
 * function interaction until the block is saved as part of a diagram-save.
 */
public class FinalDiagnosisPropertyEditor extends AbstractPropertyEditor implements NotificationChangeListener, PropertyChangeListener {
	private static final long serialVersionUID = 7211480530910862375L;
	private static final String CLSS = "FinalDiagnosisPanel";
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private final ApplicationRequestHandler requestHandler;
	private final int DIALOG_HEIGHT = 700;
	private final int DIALOG_WIDTH = 300;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private BasicEditPanel mainPanel = null;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
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
	public FinalDiagnosisPropertyEditor(DesignerContext context,DiagramWorkspace wrkspc,ProcessBlockView blk) {
		this.block = blk;
		this.model = blk.getAuxiliaryData();
		this.key = NotificationKey.keyForAuxData(block.getId().toString());
		this.rb = ResourceBundle.getBundle("com.ils.blt.designer.designer");  // designer.properties
		this.requestHandler = new ApplicationRequestHandler();
		this.context = context;
        this.diagram = wrkspc.getActiveDiagram();
		this.corePanel = new CorePropertyPanel(this,block);
		this.log = LogMaker.getLogger(this);
		this.database = requestHandler.getProductionDatabase();
		this.provider = requestHandler.getProductionTagProvider();
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
        setUI();
		// Register for notifications
		log.infof("%s: adding notification listener %s",CLSS,key);
		notificationHandler.addNotificationChangeListener(key,CLSS,this);
		requestHandler.readAuxData(context.getProject().getId(),diagram.getResourceId(),block.getId().toString(), provider, database);
	}

	/**
	 * The super class takes care of making a central tabbed pane --- but
	 * we don't want it. Simply put our mainPanel as the content pane.
	 * Here we add the tabs ... there are no buttons
	 * 1) Core attributes
	 * 2) Python hook definitions.
	 */
	private void initialize() {
		setLayout(new MigLayout("top,flowy,ins 2,gapy 0:10:15","","[top]0[]"));
		add(corePanel,"grow,push");
   
		if(diagram.getState().equals(DiagramState.ACTIVE) ||
		   diagram.getState().equals(DiagramState.ISOLATED) ) {
			mainPanel = createMainPanel();
		} 
		else {
			mainPanel = createMainPanelNoData();
		}
		add(mainPanel,"grow,push");;
	}
	
	public void shutdown() {
		notificationHandler.removeNotificationChangeListener(key,CLSS);
		save();
	}
	
	private BasicEditPanel createMainPanel() {	
		// The internal panel has two panes
		// - one for the dual list box, the other for the remaining attributes
		//setLayout(new BorderLayout());
		mainPanel = new BasicEditPanel(this);
		mainPanel.setLayout(new MigLayout("ins 2,fill","[][]","[][growprio 60,150:150:2000][]"));
		
		mainPanel.addSeparator(mainPanel,"QuantOutputs");
		dual = new DualListBox();

		dual.addPropertyChangeListener(this);
		mainPanel.add(dual, "gapx 5 5,grow,wrap");
		mainPanel.add(createPropertiesPanel(),"grow,wrap");
		return mainPanel;
	}

	private BasicEditPanel createMainPanelNoData() {	
		//setLayout(new BorderLayout());
		mainPanel = new BasicEditPanel(this);
		mainPanel.setLayout(new MigLayout("ins 1,fill","[]","[growprio 60,150:150:2000][]"));
		mainPanel.addSeparator(mainPanel,"Editing restricted - Diagram disabled or no database");
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
		final String columnConstraints = "para[][]";
		final String layoutConstraints = "ins 2,gapy 1,gapx 5,fillx,filly";
		final String rowConstraints = "para [][][][][][][][][][][][]";
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

		panel.add(createLabel("FinalDiagnosis.Label"),"gaptop 2,aligny top");
		finalDiagnosisLabelField = createTextField("FinalDiagnosis.Label.Desc","");
		
		panel.add(finalDiagnosisLabelField,"growx,wrap");
		
		panel.add(createLabel("FinalDiagnosis.Comment"),"gaptop 2,aligny top");
		commentArea = createTextArea("FinalDiagnosis.Comment.Desc","");
		JScrollPane commentScrollPane = new JScrollPane(commentArea);
		commentScrollPane.setPreferredSize(COMMENT_AREA_SIZE);
		panel.add(commentScrollPane,"growx,growy,wrap");
		
		panel.add(createLabel("FinalDiagnosis.Explanation"),"gaptop 2,aligny top");
		explanationArea = createTextArea("FinalDiagnosis.Explanation.Desc","");
		JScrollPane explanationScrollPane = new JScrollPane(explanationArea);
		explanationScrollPane.setPreferredSize(EXPLANATIION_AREA_SIZE);
		panel.add(explanationScrollPane,"growx,growy,wrap");
		
		panel.add(createLabel("FinalDiagnosis.TextRecommendation"),"gaptop 2,aligny top");
		textRecommendationArea = createTextArea("FinalDiagnosis.TextRecommendation.Desc","");
		JScrollPane textRecommendationScrollPane = new JScrollPane(textRecommendationArea);
		textRecommendationScrollPane.setPreferredSize(TEXT_RECOMMENDATION_AREA_SIZE);
		panel.add(textRecommendationScrollPane,"growx,growy,wrap");
		
		panel.add(createLabel("FinalDiagnosis.CalcMethod"),"gaptop 2,aligny top");
		calculationMethodField = createTextField("FinalDiagnosis.CalcMethod.Desc","");
		panel.add(calculationMethodField,"growx,wrap");

		panel.add(createLabel("FinalDiagnosis.Priority"),"gaptop 2,aligny top");
		priorityField = createTextField("FinalDiagnosis.Priority.Desc","");
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(priorityField,"wrap");
		
		panel.add(createLabel("FinalDiagnosis.RefreshRate"),"gaptop 2,aligny top");
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
		
		List<String> q1 = model.getLists().get("OutputsInUse");
		if( q1==null ) q1 = new ArrayList<>();
		dual.setDestinationElements(q1);
		// The outputs are ALL possibilities. Subtract 
		// those already being used.
		List<String> q0 = model.getLists().get("QuantOutputs");
		if( q0!=null ) {
			for( String inUse:q1) {
				q0.remove(inUse);
			}
		}
		else {
			q0 = new ArrayList<>();
		}
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
	 * These properties are present in every block.
	 * class, label, state, statusText
	 */
	public class CorePropertyPanel extends BasicEditPanel {
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
			add(createNameEditButton(blk),"w :25:");
			add(new JLabel("Class"),"newline,skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(new JLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
		}
		public void updateCorePanel(int tab,ProcessBlockView blk) {
			
		}
	}
	

	// Copy the FinalDiagnosis auxiliary data back into the block's aux data
	private void save(){
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
		requestHandler.writeAuxData(context.getProject().getId(),diagram.getResourceId(),block.getId().toString(),model,provider, database);
		log.infof("%s.save: writing aux data",CLSS);
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
		// I'd like to right justify the labels, but this doesn't seem to do anything.
		JLabel label = new JLabel(rb.getString(bundle)+": ", JLabel.RIGHT);
		return label;
	}
	

	/**
	 * Create a button that navigates to the proper editor for
	 * a block's name.
	 */
	private JButton createNameEditButton(final ProcessBlockView blk) {
		JButton btn = new JButton();
		final String ICON_PATH  = "Block/icons/editor/pencil.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BlockEditConstants.BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn.setIcon(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BlockEditConstants.BUTTON_SIZE);
			btn.addActionListener(new ActionListener() {
				// Determine the correct panel, depending on the property type
				public void actionPerformed(ActionEvent e){
					//CorePropertyPanel.this.updateCorePanel(BlockEditConstants.NAME_EDIT_PANEL,blk);
					setSelectedPane(BlockEditConstants.NAME_EDIT_PANEL);
				}
			});
		}
		return btn;
	}

	// ============================================== PropertyChange listener ==========================================
	@Override
	public void propertyChange(PropertyChangeEvent event) {
		if (event.getPropertyName().equalsIgnoreCase(DualListBox.PROPERTY_CHANGE_UPDATE)) {
			save();
		}

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
