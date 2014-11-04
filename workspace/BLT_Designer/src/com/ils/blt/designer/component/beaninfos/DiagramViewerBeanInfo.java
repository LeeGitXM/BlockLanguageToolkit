/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.component.beaninfos;

import java.awt.Image;
import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.PropertyChangeListener;
import java.beans.SimpleBeanInfo;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JTextField;

import com.ils.blt.client.component.PrefuseViewerComponent;
import com.ils.blt.client.component.DiagramViewer;
import com.ils.blt.client.component.RecommendationMap;
import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.factorypmi.designer.property.customizers.DynamicPropertyProviderCustomizer;
import com.inductiveautomation.factorypmi.designer.property.customizers.StyleCustomizer;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.vision.api.designer.beans.CommonBeanInfo;
import com.inductiveautomation.vision.api.designer.beans.CustomizerDescriptor;
import com.inductiveautomation.vision.api.designer.beans.InPlaceEditHandler;


/**
 * Define properties accessible in the designer properties editor for the RecommendationMap.
 * Also set the icon.
 * 
 * Caution: Empirical evidence shows that when inheriting from an extended class of CommonBeanInfo,
 *          the ability to find a palette icon is broken. 
 */
public class DiagramViewerBeanInfo extends CommonBeanInfo {
	private static final String TAG = "DiagramViewerBeanInfo";
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Has block properties
	private static final LoggerEx logger = LogUtil.getLogger(DiagramViewerBeanInfo.class.getPackage().getName());
	
	/**
	 * Constructor: Create a beaninfo object for the CallbackBlock. The
	 *              superclass fills in common properties and customizers.
	 */
	public DiagramViewerBeanInfo() {
		super(DiagramViewer.class, new CustomizerDescriptor[] {
				DynamicPropertyProviderCustomizer.VALUE_DESCRIPTOR,
				StyleCustomizer.VALUE_DESCRIPTOR});
	}

	@Override
	protected void initProperties() throws IntrospectionException {
		// Adds common properties
		super.initProperties();
		
		// Remove properties which aren't used in our component
		removeProp("opaque");
		
		addBoundProp(DiagramViewer.BLOCKS_PROPERTY, "Blocks", "A list of blocks and their positions", 
                					CAT_DATA,PREFERRED_MASK | BOUND_MASK | EXPERT_MASK);
		addBoundProp(DiagramViewer.CONNECTIONS_PROPERTY, "Connections", "A list of connections and end points", 
                					CAT_DATA,PREFERRED_MASK | BOUND_MASK | EXPERT_MASK);

	}
	
	/**
	 * Add the bean descriptors to set name, display name and short description.
	 * These strings are derived from designer.properties.
	 */
	@Override
	protected void initDesc() {
		logger.tracef("%s.initDesc",TAG);
		getBeanDescriptor().setName(BundleUtil.get().getString(PREFIX+".DiagramViewer.Name"));
		getBeanDescriptor().setDisplayName(BundleUtil.get().getString(PREFIX+".DiagramViewer.Display"));       // Tooltip-title
		getBeanDescriptor().setShortDescription(BundleUtil.get().getString(PREFIX+".DiagramViewer.Desc"));     // Tooltip-description
		super.initDesc();
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

	
	@Override
	public Image getIcon(int kind) {
		logger.tracef("%s: getIcon of type %d",TAG,kind);
		switch (kind) {
		case BeanInfo.ICON_COLOR_16x16:
		case BeanInfo.ICON_MONO_16x16:
			return new ImageIcon(getClass().getResource("/images/diagram_view_16.png")).getImage();
		case SimpleBeanInfo.ICON_COLOR_32x32:
		case SimpleBeanInfo.ICON_MONO_32x32:
		default:
			return new ImageIcon(getClass().getResource("/images/diagram_view_32.png")).getImage();
		}
	}
	
	/**
	 * Define which events are listed in the left panel of the script editor. We want only the 
	 * property change and our new gateway block execution events.
	 */
	@Override
	protected void initEventSets() throws IntrospectionException {
		// NOTE: The names refer to event and listener class names, respectively
		//       By default we get propertyChange plus all the mouse and mouse motion events - mapped to scripts
		super.initEventSets();
		//addEventSet(JComponent.class, "propertyChange", PropertyChangeListener.class, "propertyChange");
	}
	
}
