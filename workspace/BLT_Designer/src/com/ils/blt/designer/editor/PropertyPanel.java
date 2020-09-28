/**
 *   (c) 2014-2020  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.ils.blt.common.TimeUtility;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.HysteresisType;
import com.ils.blt.common.block.LimitType;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.SlopeCalculationOption;
import com.ils.blt.common.block.StatFunction;
import com.ils.blt.common.block.TransmissionScope;
import com.ils.blt.common.block.TrendDirection;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.sqltags.ClientTagManager;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.sqltags.model.Tag;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.model.TagProp;
import com.inductiveautomation.ignition.common.sqltags.model.event.TagChangeEvent;
import com.inductiveautomation.ignition.common.sqltags.model.event.TagChangeListener;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
import com.inductiveautomation.ignition.common.sqltags.model.types.ExpressionType;
import com.inductiveautomation.ignition.common.sqltags.parser.TagPathParser;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * This is a panel on the "home" panel page that edits exactly one property,
 * By default it contains:
 *    First line --
 *    	  title label
 *    Next line --
 *        valueBox     - text box with the current value
 *        edit button  - go to editor screen to change value
 *        binding btn  - go to separate screen to determine binding    
 */
public class PropertyPanel extends JPanel implements ChangeListener, FocusListener, KeyListener, NotificationChangeListener,TagChangeListener {
	private static final long serialVersionUID = 2264535784255009984L;
	private static final boolean DEBUG = false;
	private static SimpleDateFormat dateFormatter = new SimpleDateFormat(BlockConstants.TIMESTAMP_FORMAT);
	private final NotificationHandler notificationHandler = NotificationHandler.getInstance();
	private static UtilityFunctions fncs = new UtilityFunctions();
	// Use TAG as the "source" attribute when registering for Notifications from Gateway
	private final static String TAG = "PropertyPanel";
	private static final String columnConstraints = "";
	private static final String layoutConstraints = "ins 2,hidemode 2";
	private static final String rowConstraints = "";
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final DesignerContext context;
	private final JTextField bindingDisplayField;
	private final JButton editButton;
	private final JButton configurationButton;
	
	private JComboBox<String> valueComboBox;
	private final JTextField valueDisplayField;
	private final MainPanel parent;
	private final ProcessBlockView block;
	private final BlockProperty property;
	private TimeUnit currentTimeUnit;
	private final DiagramWorkspace workspace;

	
	public PropertyPanel(DesignerContext ctx, MainPanel main,ProcessBlockView blk,BlockProperty prop, DiagramWorkspace workspace) {
		log.debugf("%s.PropertyPanel: property %s (%s:%s) = %s",TAG,prop.getName(),prop.getType().toString(),prop.getBindingType().toString(),prop.getValue().toString());
		this.context = ctx;
		this.parent = main;
		this.block = blk;
		this.property = prop;
		this.workspace = workspace;

//		this.currentTimeUnit = TimeUnit.SECONDS;   // The "canonical" unit
		this.currentTimeUnit = TimeUnit.MINUTES;   // Force all to be in minutes, to avoid confusing behavior in UI
		property.addChangeListener(this);
	
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
		if( property.getType().equals(PropertyType.TIME) ) {
//			double propValue = fncs.coerceToDouble(prop.getValue());
//			currentTimeUnit = TimeUtility.unitForValue(propValue);
//			main.addSeparator(this,property.getName()+" ~ "+currentTimeUnit.name().toLowerCase());
			main.addSeparator(this,property.getName()+" ~ "+TimeUnit.MINUTES.name().toLowerCase());
		} 
		else if( property.getType().equals(PropertyType.TIME_SECONDS) ) {
			this.currentTimeUnit = TimeUnit.SECONDS;
			main.addSeparator(this,property.getName()+" ~ "+TimeUnit.SECONDS.name().toLowerCase());
		} 
		else if( property.getType().equals(PropertyType.TIME_MINUTES) ) {
			main.addSeparator(this,property.getName()+" ~ "+TimeUnit.MINUTES.name().toLowerCase());
		} 
		else {
			main.addSeparator(this,property.getName());
		}

		valueDisplayField = createValueDisplayField(property);  // May not be used ...
	
		// Handle the enumerated types
		boolean isEnumerated = isPropertyEnumerated(property);
		if( isEnumerated  ) {
			valueComboBox = createValueCombo(property);
			add(valueComboBox,"skip,growx,push");
		}
		else  {
			add(valueDisplayField,"skip,growx,push");
		}

		editButton = parent.createEditButton(property);
		configurationButton = parent.createConfigurationButton(property);
		bindingDisplayField = createBindingDisplayField(property);

		// Business Rules ...
		if( property.getType().equals(PropertyType.LIST) )   {
			add(editButton,"w :25:,wrap");
			editButton.setVisible(true);
		}
		else if( isEnumerated && property.getBindingType().equals(BindingType.ENGINE) )  {
			add(configurationButton,"w :25:,wrap");
			valueComboBox.setEditable(false);
			valueComboBox.setEnabled(false);
		}
		else if( isEnumerated ) {
			// We add a configuration button for the simple reason of attribute display
			add(configurationButton,"w :25:,wrap");
			valueComboBox.setEditable(true);
			valueComboBox.setEnabled(true);
		}                // Enumerated types are neither editable nor bindable
		else if( property.getBindingType().equals(BindingType.NONE) ||
				 property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				 property.getBindingType().equals(BindingType.ENGINE)     )  {
			add(configurationButton,"w :25:,wrap");
		}
		// For "hard-core" bindings, the value field is read-only
		else if(property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	  )  {
			valueDisplayField.setEditable(false);
			valueDisplayField.setEnabled(false);
			add(configurationButton,"w :25:,wrap");
			configurationButton.setVisible(false); 
		}
		
		// A possible second line. If the BindingType is TAG, display the binding
		// A fresh property may not have had a binding update for provider change.
		// Do it now..
		add(bindingDisplayField,"skip,growx,push");
		add(editButton,"w :25:,wrap");
		if( property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	) {
			String binding = main.getEditor().modifyPathForProvider(property.getBinding());
			property.setBinding(binding);
			bindingDisplayField.setText(binding);
			bindingDisplayField.setVisible(true);
			editButton.setVisible(true);
		}
		else {
			bindingDisplayField.setVisible(false);
			if(!property.getType().equals(PropertyType.LIST)) editButton.setVisible(false);
		}
		
		// Register for notifications
		// The "plain" (NONE) properties can be changed by python scripting
		if(property.getBindingType().equals(BindingType.ENGINE) || property.getBindingType().equals(BindingType.NONE)) {
			String key = NotificationKey.keyForProperty(block.getId().toString(), property.getName());
			log.debugf("%s.registerChangeListeners: adding %s for ENGINE",TAG,key);
			notificationHandler.addNotificationChangeListener(key,TAG,this);
		}
		else if( property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	) {
			String key = NotificationKey.keyForPropertyBinding(block.getId().toString(), property.getName());
			log.debugf("%s.registerChangeListeners: adding %s for %s",TAG,key,property.getBindingType().name());
			notificationHandler.addNotificationChangeListener(key,TAG,this);
			subscribeToTagPath(property.getBinding());
		}
	}
	// Un-subscribe to anything we're listening on ...
	public void unsubscribe() {
		if( property.getBindingType().equals(BindingType.ENGINE)|| property.getBindingType().equals(BindingType.NONE) ) {
			String key = NotificationKey.keyForProperty(block.getId().toString(), property.getName());
			notificationHandler.removeNotificationChangeListener(key,TAG);
		}
		else if( property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	) {
			String key = NotificationKey.keyForPropertyBinding(block.getId().toString(), property.getName());
			notificationHandler.removeNotificationChangeListener(key,TAG);
			unsubscribeToTagPath(property.getBinding());
		}
		property.removeChangeListener(this);
	}
	// Subscribe to a tag. This will fail if the tag path is unset or illegal.
	// The provider has been set in the panel constructor.
	private DataType subscribeToTagPath(String path) {
		DataType type = null;
		if( path==null || path.length()==0 ) return null;  // Fail silently for path not set
		if(DEBUG)log.infof("%s.subscribeToTagPath: - %s (%s)",TAG,property.getName(),path);
		 ClientTagManager tmgr = context.getTagManager();
		try {
			TagPath tp = TagPathParser.parse(path);
			Tag tag = tmgr.getTag(tp);
			type = tag.getDataType();
			tmgr.subscribe(tp, this);
		}
		catch(IOException ioe) {
			log.errorf("%s.subscribeToTagPath tag path parse error for %s (%s)",TAG,path,ioe.getMessage());
		}
		return type;
	}
	
	// Unsubscribe to a tag
	private void unsubscribeToTagPath(String path) {
		if( path==null || path.length()==0 ) return;  // Fail silently for path not set
		
		if(DEBUG)log.infof("%s.unsubscribeToTagPath: - %s (%s)",TAG,property.getName(),path);
		ClientTagManager tmgr = context.getTagManager();
		try {
			TagPath tp = TagPathParser.parse(path);
			tmgr.unsubscribe(tp, this);
		}
		catch(IOException ioe) {
			log.errorf("%s.unsubscribeToTagPath tag path parse error for %s (%s)",TAG,path,ioe.getMessage());
		}
	}
	
	// Update the panel UI for new property data. Called from Config panel via main panel.
	public void updatePanelUI() {
		String text = "";
		// For TIME we scale the value
		if( property.getType().equals(PropertyType.TIME) || property.getType().equals(PropertyType.TIME_SECONDS) || property.getType().equals(PropertyType.TIME_MINUTES) ) {
			double propValue = fncs.coerceToDouble(property.getValue());
			text = fncs.coerceToString(TimeUtility.valueForCanonicalValue(propValue,currentTimeUnit));
			log.debugf("%s.updatePanelUI: property %s,value= %s, display= %s (%s)",TAG,property.getName(),property.getValue().toString(),
													text,currentTimeUnit.name());
		}
		else {
			text = fncs.coerceToString(property.getValue());
			// For list we lop off the delimiter.
			if( DEBUG ) log.infof("%s.updatePanelUI: property %s, raw value= %s",TAG,property.getName(),text);
		}
		
		valueDisplayField.setText(text);
		if( property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	) {
			
			String msg = null;
			String tagPath = fncs.coerceToString(property.getBinding());
			// we should only do  this check if it affects the connection type
			Tag tag = null;
			Integer tagProp = null;
			if( BlockConstants.BLOCK_PROPERTY_TAG_PATH.equalsIgnoreCase(property.getName())) {
				ProcessDiagramView dview = workspace.getActiveDiagram();
				ClientTagManager tmgr = context.getTagManager();
				DataType typ = null;
				try {
					TagPath tp = TagPathParser.parse(tagPath);
					tag = tmgr.getTag(tp);
					tagProp = (Integer)tag.getAttribute(TagProp.ExpressionType).getValue();
					typ = tag.getDataType();
				} 
				catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				// block binding to expressions for output
				if (block.getClassName().equals(BlockConstants.BLOCK_CLASS_OUTPUT) && tagProp != ExpressionType.None.getIntValue()) {  // only update the tagpath property
					msg = "Unable to bind expression tag to output";
				} 
				else {
					msg = dview.isValidBindingChange(block,property,tagPath,typ,tagProp);
				}
			}
			if (msg == null) {
				// The display field has the old binding - use it to unsubscribe
				String oldPath = bindingDisplayField.getText();
				unsubscribeToTagPath(oldPath);
				
				bindingDisplayField.setText(tagPath);
				
				DataType type = subscribeToTagPath(tagPath);
				editButton.setVisible(true);
				bindingDisplayField.setVisible(true);
				valueDisplayField.setEnabled(false);
				valueDisplayField.setEditable(false);
				// we should only do  this check if it affects the connection type.
				if (property.getBinding()!=null ) {
					block.modifyConnectionForTagChange(property, type);
				}
			} else {
		        JOptionPane.showMessageDialog(null, msg, "Warning", JOptionPane.INFORMATION_MESSAGE);
			}
				
		} else {
			bindingDisplayField.setVisible(false);
			// List is the only type with a custom editor
			if( property.getType().equals(PropertyType.LIST) ) {
				editButton.setVisible(true);
				valueDisplayField.setEnabled(false);
				valueDisplayField.setEditable(false);
			}
			else if( property.getBindingType().equals(BindingType.ENGINE) ) {
				valueDisplayField.setEnabled(false);
				valueDisplayField.setEditable(false);
			}
			else {
				valueDisplayField.setEnabled(true);
				valueDisplayField.setEditable(true);
			}
		}
	}
	
	
	    // =============================== Component Creation Methods ================================
	
	
	/**
	 * Create a text box for the binding field. This is editable.
	 */
	private JTextField createBindingDisplayField(final BlockProperty prop) {	
		Object val = prop.getBinding();
		if(val==null) val = "";
		EditableTextField field = new EditableTextField(prop,val.toString());
		field.addFocusListener(this);
		field.addKeyListener(this);
		return field;
	}
	/**
	 * Create a text box for the binding field. This is editable.
	 */
	private JComboBox<String> createValueCombo(final BlockProperty prop) {	
		final JComboBox<String> valueCombo = new JComboBox<String>();
		if(prop.getBindingType().equals(BindingType.OPTION))      setOptionCombo(valueCombo,prop);
		else if( prop.getType().equals(PropertyType.BOOLEAN))     setBooleanCombo(valueCombo);
		else if( prop.getType().equals(PropertyType.COLOR))       setColorCombo(valueCombo);
		else if( prop.getType().equals(PropertyType.PROPERTY))    setPropertyCombo(valueCombo, getSignalDownStreamBlock());
		else if(prop.getType().equals(PropertyType.HYSTERESIS))   setHysteresisTypeCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.LIMIT))        setLimitTypeCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.SCOPE))	      setTransmissionScopeCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.SLOPEOPTION))  setSlopeCalculationOptionCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.STATISTICS))   setStatisticsCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.TRENDDIRECTION)) setTrendDirectionCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.TRUTHVALUE) )  setTruthValueCombo(valueCombo); 
		valueCombo.setEditable(true);
		valueCombo.repaint();
		this.invalidate();
		if( prop.getValue()!=null ) {
			final String selection = prop.getValue().toString().toUpperCase();
			log.tracef("%s.createValueCombo: %s=%s",TAG,prop.getName(),selection);
			SwingUtilities.invokeLater( new Runnable() {
				public void run() {
					valueCombo.setSelectedItem(selection);
				}
			});
			valueCombo.getModel().setSelectedItem(selection);
			//log.tracef("%s.createValueCombo: selection now=%s",TAG,valueCombo.getModel().getSelectedItem().toString());
		} 
		valueCombo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                String selection = valueCombo.getSelectedItem().toString();
                if( !prop.getValue().toString().equalsIgnoreCase(selection)) {
					prop.setValue(selection);
                	parent.saveDiagramCLean();   // Update property immediately
                }
            }
        });

		return valueCombo;
	}
	/**
	 * Populate a combo box for true/false 
	 */
	private void setBooleanCombo(JComboBox<String> box) {
		box.removeAllItems();
		box.addItem(Boolean.TRUE.toString().toUpperCase());
		box.addItem(Boolean.FALSE.toString().toUpperCase());
	}

	/**
	 * Populate a combo box for colors 
	 */
	private void setColorCombo(JComboBox<String> box) {
		box.removeAllItems();
		box.addItem("TRANSPARENT");
		box.addItem("RED");
		box.addItem("GREEN");
		box.addItem("BLUE");
		box.addItem("WHITE");
		box.addItem("YELLOW");
		box.addItem("GRAY");
		box.addItem("LIGHT_GRAY");
		box.addItem("DARK_GRAY");
		box.addItem("ORANGE");
		box.addItem("MAGENTA");
		box.addItem("PINK");
		box.addItem("CYAN");
	}
	
	/**
	 * Follow the signal connection and get the downstream block
	 */
	private ProcessBlockView getSignalDownStreamBlock() {
		ProcessBlockView ret = null;
		ProcessDiagramView pdv = workspace.getActiveDiagram();
		Collection<Connection> connections = pdv.getConnections();
		for (Connection connection:connections) {
			Block origin = connection.getOrigin().getBlock();
			Block terminus = connection.getTerminus().getBlock();
			if (origin.equals(block) && origin instanceof ProcessBlockView ) {
				ProcessBlockView found = (ProcessBlockView) terminus;
				for (ProcessAnchorDescriptor pad:found.getAnchors()) {
					if (pad.getConnectionType().equals(ConnectionType.SIGNAL)) {
						ret = found;
						break;
					}
				}
			}
			if (ret != null) {  // Got it.  Stop looking
				break;
			}
		}
		return ret;
	}

	/**
	 * Populate a combo box of property names from the downstream block (signal connected)
	 */
	private void setPropertyCombo(JComboBox<String> box, ProcessBlockView downstream) {
		box.removeAllItems();
		if (downstream != null) {
			for (BlockProperty prop: downstream.getProperties()) {
				box.addItem(prop.getName().toUpperCase());
			}
		} 
		else {
			box.addItem("Not available - Block not connected or diagram unsaved");
		}
	}
	
	/**
	 * Populate a combo box for OPTION type. For these properties
	 * the option list is a comma-separated set of values in the binding.
	 */
	private void setOptionCombo(JComboBox<String> box, BlockProperty prop) {
		box.removeAllItems();
		String list = prop.getBinding();
		String[] args = list.split(",");
		for(String arg : args) {
			box.addItem(arg);
		}
	}
	/**
	 * Populate a combo box for hysteresis type
	 */
	private void setHysteresisTypeCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(HysteresisType type : HysteresisType.values()) {
			box.addItem(type.name());
		}
	}
	/**
	 * Populate a combo box for limit type
	 */
	private void setLimitTypeCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(LimitType scope : LimitType.values()) {
			box.addItem(scope.name());
		}
	}
	/**
	 * Populate a combo box for trend slope calculation option
	 */
	private void setSlopeCalculationOptionCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(SlopeCalculationOption opt : SlopeCalculationOption.values()) {
			box.addItem(opt.name());
		}
	}
	/**
	 * Populate a combo box for statistics functions
	 */
	private void setStatisticsCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(StatFunction type : StatFunction.values()) {
			box.addItem(type.name());
		}
	}
	/**
	 * Populate a combo box for transmission scope
	 */
	private void setTransmissionScopeCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(TransmissionScope scope : TransmissionScope.values()) {
			box.addItem(scope.name());
		}
	}
	/**
	 * Populate a combo box for trend direction
	 */
	private void setTrendDirectionCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(TrendDirection direction : TrendDirection.values()) {
			box.addItem(direction.name());
		}
	}
	/**
	 * Populate a combo box for limit type
	 */
	private void setTruthValueCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(TruthValue tv : TruthValue.values()) {
			box.addItem(tv.name());
		}
	}
	/**
	 * Create a text box for the value field.
	 */
	private JTextField createValueDisplayField(final BlockProperty prop) {	
		Object val = fncs.coerceToString(prop.getValue());
		if(val==null) val = "";
		JTextField field = null;
		if( prop.getType().equals(PropertyType.TIME) || prop.getType().equals(PropertyType.TIME_SECONDS) || prop.getType().equals(PropertyType.TIME_MINUTES) ) {
			// Scale value for time unit.
			double interval = fncs.coerceToDouble(property.getValue());
			val = fncs.coerceToString(TimeUtility.valueForCanonicalValue(interval,currentTimeUnit));
			log.tracef("%s.createValueDisplayField: property %s,value= %s, display= %s (%s)",TAG,property.getName(),property.getValue().toString(),
					val,currentTimeUnit.name());
		}
		// A date is intrinsically non-editable.
		if( prop.getType().equals(PropertyType.DATE)) {
			val = dateFormatter.format(new Date(fncs.coerceToLong(prop.getValue())));
			field = new JTextField(val.toString());
			field.setEditable(false);
			field.setEnabled(false);
		}
		else if( prop.isEditable() && !prop.getType().equals(PropertyType.LIST)) {
			field = new EditableTextField(prop,val.toString());
			field.addFocusListener(this);
			field.addKeyListener(this);
		}
		else {
			field = new JTextField(val.toString());
			field.setEditable(false);
			field.setEnabled(false);
		}
		return field;
	}
	// Enumerated types use a combo box instead of a text field
	private boolean isPropertyEnumerated(BlockProperty prop) {
			return (prop.getBindingType().equals(BindingType.OPTION)   ||
				    prop.getType().equals(PropertyType.BOOLEAN)        ||          
				    prop.getType().equals(PropertyType.HYSTERESIS)     ||  
				    prop.getType().equals(PropertyType.STATISTICS)	   ||
				    prop.getType().equals(PropertyType.LIMIT)          ||        
				    prop.getType().equals(PropertyType.SCOPE)          ||
				    prop.getType().equals(PropertyType.COLOR)          ||
				    prop.getType().equals(PropertyType.PROPERTY)       ||
				    prop.getType().equals(PropertyType.SLOPEOPTION)    ||
				    prop.getType().equals(PropertyType.TRENDDIRECTION) ||
				    prop.getType().equals(PropertyType.TRUTHVALUE)          ); 
	}
	
	/**
	 * A component field that remembers the associated block property and
	 * allows us to access the current text value of the field.
	 */
	public interface EditableField {
		public BlockProperty getProperty();
		public String getText();
	}
	
	/**
	 * A text field that remembers the associated block property.
	 */
	private class EditableTextField extends JTextField implements EditableField{
		private static final long serialVersionUID = 1L;
		private final BlockProperty fieldProperty;
		public EditableTextField(BlockProperty prop,String val) {
			super(val);
			fieldProperty = prop;
			setEditable(true);
			setEnabled(true);
		}
		public BlockProperty getProperty() { return fieldProperty; }
	}

	// =========================================== Focus Listener ====================================
	@Override
	public void focusGained(FocusEvent e) {}
	/**
	 * This method is fired if we type a TAB, thereby leaving the field. The method is also
	 * called if we select a component outside this field.
	 */
	@Override
	public void focusLost(FocusEvent e) {
		if( e.getSource() instanceof EditableTextField ) {
			log.debugf("%s.focusLost: %s", TAG,e.getSource().getClass().getName());
			EditableField field = (EditableField)e.getSource();
			updatePropertyForField(field,false);
		}
	}

	// =========================================== Key Listener ====================================
	@Override
	public void keyTyped(KeyEvent e) {}

	/**
	 * This method is fired when we type ENTER, among other things. In reality we ultimately get a loss of focus
	 * event sometime later when we navigate away from the field. 
	 */
	@Override
	public void keyPressed(KeyEvent e) {
		if( e.getSource() instanceof EditableTextField)  {
			//log.tracef("%s.keyPressed: %s = %d, %d", TAG,((EditableTextField)e.getSource()).getProperty().getName(),e.getKeyCode(),KeyEvent.VK_ENTER);
			if( e.getKeyCode()==KeyEvent.VK_ENTER ) {
				EditableField field = (EditableField)e.getSource();
				updatePropertyForField(field,true);    // Force propagation of the change
			}
		}
	}
	@Override
	public void keyReleased(KeyEvent e) {}
	
	// If the force flag is on, propagate the event even if the value has not changed. Use "force" with a 
	// carriage return in the field, but not with a loss of focus.
	private void updatePropertyForField(EditableField field,boolean force) {
		BlockProperty prop = field.getProperty();
		if( DEBUG ) log.infof("%s.updatePropertyForField: %s (%s:%s)", TAG,prop.getName(),prop.getType().name(),prop.getBindingType().name());
		// If there is a value change, then update the property (or binding)
		if( prop.getBindingType().equals(BindingType.NONE)) {
			Object fieldValue = field.getText();
			if( force || !fieldValue.equals(prop.getValue().toString())) {
				// Coerce to the correct data type
				if( prop.getType().equals(PropertyType.BOOLEAN ))     fieldValue = new Boolean(fncs.coerceToBoolean(fieldValue));
				else if( prop.getType().equals(PropertyType.DOUBLE )) fieldValue = new Double(fncs.coerceToDouble(fieldValue));
				else if( prop.getType().equals(PropertyType.INTEGER ))fieldValue = new Integer(fncs.coerceToInteger(fieldValue));
				else if( prop.getType().equals(PropertyType.DATE ))   fieldValue = dateFormatter.format(new Date(fncs.coerceToLong(fieldValue)));
				if( prop.getType().equals(PropertyType.TIME) || prop.getType().equals(PropertyType.TIME_SECONDS) || prop.getType().equals(PropertyType.TIME_MINUTES) ) {
					// Scale field value for time unit. Get back to seconds.
					double interval = fncs.coerceToDouble(fieldValue);
					fieldValue = new Double(TimeUtility.canonicalValueForValue(interval,currentTimeUnit));
					log.tracef("%s.updatePropertyForField: property %s,old= %s, new= %s, displayed= %s (%s)",TAG,prop.getName(),prop.getValue().toString(),
							fieldValue.toString(),field.getText(),currentTimeUnit.name());
				}
				prop.setValue(fieldValue);
				parent.saveDiagramCLean();    // Update property directly, immediately
			}
			else {
				log.tracef("%s.updatePropertyForField: No Change was %s, is %s", TAG,prop.getValue().toString(),fieldValue);
			}
		}
		else {
			if( !field.getText().equals(prop.getBinding()) ) {
				unsubscribeToTagPath(prop.getBinding());
				String tagPath = parent.getEditor().modifyPathForProvider(field.getText());
				if( DEBUG ) log.infof("%s.updatePropertyForField: Adjusting %s to %s", TAG,prop.getBinding(),tagPath);
				prop.setBinding(tagPath);
				subscribeToTagPath(tagPath);
				parent.saveDiagramCLean();		
			}
		}
	}
	// ======================================= Notification Change Listener ===================================
	@Override
	public void bindingChange(String binding) {
		if(DEBUG )log.infof("%s.bindingChange: - %s new binding (%s)",TAG,property.getName(),binding);
		//property.setValue(value.getValue());  // Block should have its own subscription to value changes.
		SwingUtilities.invokeLater( new Runnable() {
			public void run() {
				unsubscribeToTagPath(bindingDisplayField.getText());
				bindingDisplayField.setText(binding);
				property.setBinding(binding);
				subscribeToTagPath(binding);
			}
		});
	}
	@Override
	public void diagramStateChange(long resId, String state) {}
	// We get this when another entity changes a property. We just need to re-display.
	@Override
	public void nameChange(String name) {
		
	}
	/**
	 * The source of the event is a property. 
	 * Ignore if the binding has not changed.
	 */
	@Override
	public void stateChanged(ChangeEvent e) {
		BlockProperty source = (BlockProperty)e.getSource();
		if( source.getBinding()!=null && !source.getBinding().equals(property.getBinding())) {
			if(DEBUG) log.infof("%s.stateChanged: - %s -> %s",TAG,property.getBinding(),source.getBinding());
			updatePanelUI();	
		}
	}
	

	@Override
	public void valueChange(final QualifiedValue value) {
		log.debugf("%s.valueChange: - %s new value (%s)",TAG,property.getName(),value.getValue().toString());
		//property.setValue(value.getValue());  // Block should have its own subscription to value changes.
		SwingUtilities.invokeLater( new Runnable() {
			public void run() {
				String text = value.getValue().toString();
				if(property.getType().equals(PropertyType.TIME)) {
					// Scale value for time unit.
					double interval = fncs.coerceToDouble(text);
					text = fncs.coerceToString(TimeUtility.valueForCanonicalValue(interval,currentTimeUnit));
				}
				else if(property.getType().equals(PropertyType.TIME_MINUTES)) {
					// Scale value for time unit.
					double interval = fncs.coerceToDouble(text);
					text = fncs.coerceToString(TimeUtility.valueForCanonicalValue(interval,TimeUnit.MINUTES));
				}
				else if(property.getType().equals(PropertyType.TIME_SECONDS)) {
					// Scale value for time unit.
					double interval = fncs.coerceToDouble(text);
					text = fncs.coerceToString(TimeUtility.valueForCanonicalValue(interval,TimeUnit.SECONDS));
				}
				else if(property.getType().equals(PropertyType.DATE) ) {
					text = dateFormatter.format(new Date(fncs.coerceToLong(text)));
				}
				// If we set components willy-nilly, we end up in an update loop.
				if( isPropertyEnumerated(property)) {
					if( !valueComboBox.getSelectedItem().toString().equalsIgnoreCase(text)) {
						valueComboBox.setSelectedItem(text.toUpperCase());
					}	
				}
				else if(!valueDisplayField.getText().equals(text)) {
					valueDisplayField.setText(text);
				}
			}
		});
	}
	@Override
	public void watermarkChange(String mark) {}
	// =========================================== Tag Change Listener ===================================
	// Set this to null as we're interested in all tag properties
	@Override
	public TagProp getTagProperty() {
		return null;
	}
	// The display contains tag value, quality and timestamp
	@Override
	public void tagChanged(TagChangeEvent event) {
		final Tag tag = event.getTag();
		if( tag!=null && tag.getValue()!=null ) {
			log.tracef("%s.tagChanged: - %s new value from %s (%s)",TAG,property.getName(),tag.getName(),tag.getValue().toString());
			SwingUtilities.invokeLater( new Runnable() {
				public void run() {
					String text = String.format("%s  %s  %s", 
							(tag.getValue().getValue()==null?"null":tag.getValue().getValue().toString()),
							(tag.getValue().getQuality() ==null?"null":tag.getValue().getQuality().toString()),
							                   dateFormatter.format(tag.getValue().getTimestamp()));
					valueDisplayField.setText(text);
				}
			});
		}
		else {
			// Tag or path is null
			log.warnf("%s.tagChanged: Unknown tag (%s)",TAG,(tag==null?"null":tag.getName()));
		}
	}
}