/**
 *   (c) 2014  ILS Automation. All rights reserved.
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
import java.util.concurrent.TimeUnit;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.TimeUtility;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.HysteresisType;
import com.ils.blt.common.block.LimitType;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TransmissionScope;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.NotificationHandler;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.sqltags.ClientTagManager;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.sqltags.model.Tag;
import com.inductiveautomation.ignition.common.sqltags.model.TagPath;
import com.inductiveautomation.ignition.common.sqltags.model.TagProp;
import com.inductiveautomation.ignition.common.sqltags.model.event.TagChangeEvent;
import com.inductiveautomation.ignition.common.sqltags.model.event.TagChangeListener;
import com.inductiveautomation.ignition.common.sqltags.parser.TagPathParser;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

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
	private static SimpleDateFormat dateFormatter = new SimpleDateFormat(BlockConstants.TIMESTAMP_FORMAT);
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
	
	public PropertyPanel(DesignerContext ctx, MainPanel main,ProcessBlockView blk,BlockProperty prop) {
		log.debugf("%s.PropertyPanel: property %s (%s:%s) = %s",TAG,prop.getName(),prop.getType().toString(),prop.getBindingType().toString(),prop.getValue().toString());
		this.context = ctx;
		this.parent = main;
		this.block = blk;
		this.property = prop;
		this.currentTimeUnit = TimeUnit.SECONDS;   // The "canonical" unit
		property.addChangeListener(this);
	
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
		if( property.getType().equals(PropertyType.TIME) ) {
			double propValue = fncs.coerceToDouble(prop.getValue());
			currentTimeUnit = TimeUtility.unitForValue(propValue);
			main.addSeparator(this,property.getName()+" ~ "+currentTimeUnit.name().toLowerCase());
		}
		else {
			main.addSeparator(this,property.getName());
		}

		valueDisplayField = createValueDisplayField(property);  // May not be used ...
		boolean isEnumerated = false;

		// Handle the enumerated types
		if( 
			property.getBindingType().equals(BindingType.OPTION)  ||
			property.getType().equals(PropertyType.BOOLEAN)       ||          
			property.getType().equals(PropertyType.HYSTERESIS)     ||   
			property.getType().equals(PropertyType.LIMIT)          ||        
			property.getType().equals(PropertyType.SCOPE)          ||	      
			property.getType().equals(PropertyType.TRUTHVALUE)          ) {

			isEnumerated = true;
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
		
		// A possible second line.
		// If the BindingType is TAG, display the binding
		add(bindingDisplayField,"skip,growx,push");
		add(editButton,"w :25:,wrap");
		if( property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	) {
			bindingDisplayField.setVisible(true);
			editButton.setVisible(true);
		}
		else {
			bindingDisplayField.setVisible(false);
			if(!property.getType().equals(PropertyType.LIST)) editButton.setVisible(false);
		}
		
		// Register for notifications
		if(property.getBindingType().equals(BindingType.ENGINE) ) {
			NotificationHandler handler = NotificationHandler.getInstance();
			String key = NotificationKey.keyForProperty(block.getId().toString(), property.getName());
			log.infof("%s.registerChangeListeners: adding %s",TAG,key);
			handler.addNotificationChangeListener(key,TAG,this);
		}
		else if( property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	) {
			subscribeToTagPath(property.getBinding());
		}
	}
	// Un-subscribe to anything we're listening on ...
	public void unsubscribe() {
		if( property.getBindingType().equals(BindingType.ENGINE) ) {
			NotificationHandler handler = NotificationHandler.getInstance();
			String key = NotificationKey.keyForProperty(block.getId().toString(), property.getName());
			handler.removeNotificationChangeListener(key,TAG);
		}
		else if( property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	) {
			unsubscribeToTagPath(property.getBinding());
		}
		property.removeChangeListener(this);
	}
	// Subscribe to a tag. This will fail if the tag path is unset of illegal.
	private void subscribeToTagPath(String path) {
		if( path==null || path.length()==0 ) return;  // Fail silently for path not set
		
		// If tag path isn't in canonical form, make it that way by prepending provider
		String providerName = providerNameFromPath(path);
		if( providerName.length()==0) {
			providerName = context.getDefaultSQLTagsProviderName();
			path = String.format("[%s]%s",providerName,path);
		}
		log.debugf("%s.subscribeToTagPath: - %s (%s)",TAG,property.getName(),path);
		ClientTagManager tmgr = context.getTagManager();
		try {
			TagPath tp = TagPathParser.parse(path);
			tmgr.subscribe(tp, this);
		}
		catch(IOException ioe) {
			log.errorf("%s.unsubscribe tag path parse error for %s (%s)",TAG,path,ioe.getMessage());
		}
	}
	
	// Unsubscribe to a tag
	private void unsubscribeToTagPath(String path) {
		if( path==null || path.length()==0 ) return;  // Fail silently for path not set
		
		log.debugf("%s.unsubscribeToTagPath: - %s (%s)",TAG,property.getName(),path);
		ClientTagManager tmgr = context.getTagManager();
		try {
			TagPath tp = TagPathParser.parse(path);
			tmgr.unsubscribe(tp, this);
		}
		catch(IOException ioe) {
			log.errorf("%s.unsubscribe tag path parse error for %s (%s)",TAG,path,ioe.getMessage());
		}
	}
	private String providerNameFromPath(String tagPath) {
		String provider = "";
		if( tagPath.startsWith("[") ) {
			int index = tagPath.indexOf(']');
			if( index>0 ) {
				provider = tagPath.substring(1,index);
			}
		}
		return provider;
	}
	
	// Update the panel UI for new property data. Called from Config panel via main panel.
	public void update() {
		String text = "";
		// For TIME we scale the value
		if( property.getType().equals(PropertyType.TIME) ) {
			double propValue = fncs.coerceToDouble(property.getValue());
			text = fncs.coerceToString(TimeUtility.valueForCanonicalValue(propValue,currentTimeUnit));
			log.debugf("%s.update: property %s,value= %s, display= %s (%s)",TAG,property.getName(),property.getValue().toString(),
													text,currentTimeUnit.name());
		}
		else {
			text = fncs.coerceToString(property.getValue());
			// For list we lop off the delimiter.
			log.debugf("%s.updateForProperty: property %s, raw value= %s",TAG,property.getName(),text);
		}
		
		valueDisplayField.setText(text);
		if( property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	) {
			// The display field has the old binding - use it to unsubscribe
			String oldPath = bindingDisplayField.getText();
			unsubscribeToTagPath(oldPath);
			
			bindingDisplayField.setText(fncs.coerceToString(property.getBinding()));
			subscribeToTagPath(property.getBinding());
			editButton.setVisible(true);
			bindingDisplayField.setVisible(true);
			valueDisplayField.setEnabled(false);
			valueDisplayField.setEditable(false);
		}
		else {
			bindingDisplayField.setVisible(false);
			// List is the only type with a custom editor
			if( !property.getType().equals(PropertyType.LIST) ) {
				editButton.setVisible(false);
				bindingDisplayField.setVisible(true);
				valueDisplayField.setEnabled(false);
				valueDisplayField.setEditable(false);
			}
			if( !property.getBindingType().equals(BindingType.ENGINE) ) {
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
		else if(prop.getType().equals(PropertyType.HYSTERESIS))   setHysteresisTypeCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.LIMIT))        setLimitTypeCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.SCOPE))	      setTransmissionScopeCombo(valueCombo);
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
                	parent.handlePropertyChange(prop);    // Update property immediately
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
	 * Populate a combo box for transmission scope
	 */
	private void setTransmissionScopeCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(TransmissionScope scope : TransmissionScope.values()) {
			box.addItem(scope.name());
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
		if(prop.getType().equals(PropertyType.TIME)) {
			// Scale value for time unit.
			double interval = fncs.coerceToDouble(property.getValue());
			val = fncs.coerceToString(TimeUtility.valueForCanonicalValue(interval,currentTimeUnit));
			log.tracef("%s.createValueDisplayField: property %s,value= %s, display= %s (%s)",TAG,property.getName(),property.getValue().toString(),
					val,currentTimeUnit.name());
		}
		if( prop.isEditable() && !prop.getType().equals(PropertyType.LIST)) {
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
		log.debugf("%s.updatePropertyForField: %s (%s:%s)", TAG,prop.getName(),prop.getType().name(),prop.getBindingType().name());
		// If there is a value change, then update the property (or binding)
		if( prop.getBindingType().equals(BindingType.NONE)) {
			Object fieldValue = field.getText();
			if( force || !fieldValue.equals(prop.getValue().toString())) {
				// Coerce to the correct data type
				if( prop.getType().equals(PropertyType.BOOLEAN ))     fieldValue = new Boolean(fncs.coerceToBoolean(fieldValue));
				else if( prop.getType().equals(PropertyType.DOUBLE )) fieldValue = new Double(fncs.coerceToDouble(fieldValue));
				else if( prop.getType().equals(PropertyType.INTEGER ))fieldValue = new Integer(fncs.coerceToInteger(fieldValue));
				else if(prop.getType().equals(PropertyType.TIME)) {
					// Scale field value for time unit. Get back to seconds.
					double interval = fncs.coerceToDouble(fieldValue);
					fieldValue = new Double(TimeUtility.canonicalValueForValue(interval,currentTimeUnit));
					log.tracef("%s.updatePropertyForField: property %s,old= %s, new= %s, displayed= %s (%s)",TAG,prop.getName(),prop.getValue().toString(),
							fieldValue.toString(),field.getText(),currentTimeUnit.name());
				}
				prop.setValue(fieldValue);
				parent.handlePropertyChange(prop);    // Update property directly, immediately
			}
			else {
				log.tracef("%s.updatePropertyForField: No Change was %s, is %s", TAG,prop.getValue().toString(),fieldValue);
			}
		}
		else {
			if( !field.getText().equals(prop.getBinding()) ) {
				unsubscribeToTagPath(prop.getBinding());
				prop.setBinding(field.getText());
				subscribeToTagPath(prop.getBinding());
				parent.handlePropertyChange(prop);		
			}
		}
	}
	// ======================================= Notification Change Listener ===================================
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
				valueDisplayField.setText(text);
			}
		});
		
	}
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
	
	// =========================================== Change Listener ===================================
	// We get this when another entity changes a property. We just need to re-display.
	@Override
	public void stateChanged(ChangeEvent e) {
		//log.infof("%s.stateChanged: - %s",TAG,property.getName());
		update();	
	}
}