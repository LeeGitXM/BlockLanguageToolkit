/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.component.beaninfos;

import java.awt.Image;
import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.SimpleBeanInfo;

import javax.swing.ImageIcon;

import com.ils.blt.client.component.DiagramAnalyzerComponent;
import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.factorypmi.designer.property.customizers.DynamicPropertyProviderCustomizer;
import com.inductiveautomation.factorypmi.designer.property.customizers.StyleCustomizer;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.vision.api.designer.beans.CommonBeanInfo;
import com.inductiveautomation.vision.api.designer.beans.CustomizerDescriptor;


/**
 * Define properties accessible in the designer properties editor for the CallbackBlock.
 * Also set the icon.
 */
public class DiagramAnalyzerComponentBeanInfo extends CommonBeanInfo {
	private static final String TAG = "DiagramAnalyzerComponentBeanInfo";
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Has block properties
	private static final LoggerEx logger = LogUtil.getLogger(DiagramAnalyzerComponentBeanInfo.class.getPackage().getName());
	
	/**
	 * Constructor: Create a beaninfo object for the CallbackBlock. The
	 *              superclass fills in common properties and customizers.
	 */
	public DiagramAnalyzerComponentBeanInfo() {
		super(DiagramAnalyzerComponent.class, new CustomizerDescriptor[] {
				DynamicPropertyProviderCustomizer.VALUE_DESCRIPTOR,
				StyleCustomizer.VALUE_DESCRIPTOR});
		logger.infof("%s:CONSTRUCTOR",TAG);
	}

	@Override
	protected void initProperties() throws IntrospectionException {
		// Adds common properties
		logger.infof("%s:INITPROPRTIES",TAG);
		super.initProperties();
	}
	
	/**
	 * Add the bean descriptors to set name, display name and short description.
	 * These strings are derived from designer.properties.
	 */
	@Override
	protected void initDesc() {
		logger.infof("%s.initDesc",TAG);
		getBeanDescriptor().setName(BundleUtil.get().getString(PREFIX+".DiagramAnalyzer.Name"));
		getBeanDescriptor().setDisplayName(BundleUtil.get().getString(PREFIX+".DiagramAnalyzer.Display"));       // Tooltip-title
		getBeanDescriptor().setShortDescription(BundleUtil.get().getString(PREFIX+".DiagramAnalyzer.Desc"));     // Tooltip-description
		super.initDesc();
	}

	
	@Override
	public Image getIcon(int kind) {
		String imagePath="";
		logger.infof("%s: getIcon of type %d",TAG,kind);
		switch (kind) {
		case BeanInfo.ICON_COLOR_16x16:
		case BeanInfo.ICON_MONO_16x16:
			imagePath = "/images/diagram_analyzer_16.png";
			break;
		case SimpleBeanInfo.ICON_COLOR_32x32:
		case SimpleBeanInfo.ICON_MONO_32x32:
		default:
			imagePath = "/images/diagram_analyzer_32.png";
		}
		Image img = new ImageIcon(getClass().getResource(imagePath)).getImage();
		return img;
	}
}
