/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.component.beaninfos;

import java.awt.Dimension;
import java.beans.IntrospectionException;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JTextField;

import com.ils.blt.client.component.PrefuseViewerComponent;
import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.factorypmi.designer.property.customizers.DynamicPropertyProviderCustomizer;
import com.inductiveautomation.vision.api.designer.beans.CommonBeanInfo;
import com.inductiveautomation.vision.api.designer.beans.CustomizerDescriptor;
import com.inductiveautomation.vision.api.designer.beans.InPlaceEditHandler;
/**
 * Define the properties that are common to all BasicBlockComponents.
 * We expect this class to be extended for each different block type.
 * It may be necessary for some blocks to remove properties defined
 * here that are common to most, but not all block types.
 */
public class BasicBlockBeanInfo extends CommonBeanInfo {
	private final static String PREFIX = BLTProperties.BUNDLE_PREFIX+".Workspace.Menu.";
	private Dimension preferredSize = null;
	
   /**
    * Constructor: The superclass constructor takes an array of relevant custom
    *               descriptors. The DynamicPropertyProviderCustomizer.VALUE_DESCRIPTOR
    *               is added here.
    * @param c class of the execution block
    */
	public BasicBlockBeanInfo(Class<?> c) {
		super(c, new CustomizerDescriptor[] { DynamicPropertyProviderCustomizer.VALUE_DESCRIPTOR });
	}
	/**
	 * Constructor with customizers. Arguments are passed directly to the superclass.
	 */
	public BasicBlockBeanInfo(Class<?> c, CustomizerDescriptor ... descs) {
		super(c,descs);
	}
	
	/**
	 * Add property descriptors for all the BasicBlockComponent properties.
	 * Sub-classes should extend this method to add more properties as appropriate.
	 */
	@Override
	protected void initProperties() throws IntrospectionException {
		//super.initProperties();  // Throws exception
		
		//removeProp("opaque");
		//removeProp("border");
		
		//addProp("blockType","Block Type","Role that this block performs in the execution model", CAT_COMMON, PREFERRED_MASK|BOUND_MASK);
		//addProp("heading","Heading","Title text on the component", CAT_COMMON, PREFERRED_MASK|BOUND_MASK);
		//addProp("subHeading","SubHeading","Explanatory text on the component", CAT_COMMON, PREFERRED_MASK|BOUND_MASK);
		//addProp("connections","Next Blocks","Comma-separated list of objects immediately downstream", CAT_COMMON, PREFERRED_MASK);
		//addEnumProp("encapsulationEnabled","Allow Subworkspace","True if encapsulation is allowed for this block",CAT_COMMON,
		//		new int[] {0,1}, new String[] {"FALSE","TRUE"}, PREFERRED_MASK);
		//addEnumProp("startBlock","Starts Model","True if this block serves as an entry point into the model",CAT_COMMON,
		//		new int[] {0,1}, new String[] {"FALSE","TRUE"}, PREFERRED_MASK);
	}
	
	/**
	 * Define which events are listed in the left panel of the script editor. We want only the 
	 * property change and our new gateway block execution events.
	 */
	@Override
	protected void initEventSets() throws IntrospectionException {
		// NOTE: The names refer to event and listener class names, respectively
		addEventSet(JComponent.class, "propertyChange", PropertyChangeListener.class, "propertyChange");
	}
	
	/**
	 * Initialize the bean descriptors.
	 */
	@Override
	protected void initDesc() {
	    
	    // When an execution status change arrives, run the following method. 
	    // See PMIButtonBeanInfo as an example.
		/*
	    try {
	        Method executionComplete = ExecutionStatusListener.class.getMethod("executionComplete", new Class[] { ExecutionStatusEvent.class });
	        getBeanDescriptor().setValue(CommonBeanInfo.DOUBLE_CLICK_HANDLER, new OpenEventScriptingHandler(executionComplete));
	    }
	    catch (Exception e) {
	      e.printStackTrace();
	    }
	    */
	    // Display a custom popup menu with a right-click.
		getBeanDescriptor().setValue(CommonBeanInfo.RIGHT_CLICK_HANDLER, new BlockComponentInitializer() );	
	 
		// Allow editing of the block name  in-place with a double-click. 
		// Note: Edit-click and double-click seem to be the same gesture.
		getBeanDescriptor().setValue(CommonBeanInfo.EDIT_CLICK_HANDLER, new InPlaceEditHandler() {
	      protected int getHorizontalAlignment(JComponent component) {
	         return JTextField.RIGHT;
	      }
	      protected void setText(JComponent component, String text)  {
	    	  ((PrefuseViewerComponent)component).setName(text);
	      }
	      protected String getText(JComponent component) {
	        return ((PrefuseViewerComponent)component).getName();
	      }
	    });
	
	}
	
	// Needed to satisfy introspection ...
	public boolean isPreferredSize() { return false; }  // Works
	public void setPreferredSize(Dimension dim) { this.preferredSize = dim; }
}
