/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.component.beaninfos;

import java.awt.Image;
import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.SimpleBeanInfo;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;


/**
 * Define properties accessible in the designer properties editor for the CallbackBlock.
 * Also set the icon.
 */
public class DiagramAnalyzerComponentBeanInfo extends BasicBlockBeanInfo {
	private static final String TAG = "DiagramAnalyzerComponentBeanInfo";
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Has block properties
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
	/**
	 * Constructor: Create a beaninfo object for the CallbackBlock. The
	 *              superclass fills in common properties and customizers.
	 */
	public DiagramAnalyzerComponentBeanInfo() {
		super(DiagramAnalyzerComponentBeanInfo.class);
		log.infof("%s:CONSTRUCTOR",TAG);
	}

	@Override
	protected void initProperties() throws IntrospectionException {
		// Adds common properties
		super.initProperties();
	}
	
	/**
	 * Add the bean descriptors to set name, display name and short description.
	 * These strings are derived from designer.properties.
	 */
	@Override
	protected void initDesc() {
		getBeanDescriptor().setName(BundleUtil.get().getString(PREFIX+".DiagramAnalyzer.Name"));
		getBeanDescriptor().setDisplayName(BundleUtil.get().getString(PREFIX+".DiagramAnalyzer.Display"));       // Tooltip-title
		getBeanDescriptor().setShortDescription(BundleUtil.get().getString(PREFIX+".DiagramAnalyzer.Desc"));     // Tooltip-description
		super.initDesc();
	}

	
	@Override
	public Image getIcon(int kind) {
		String imagePath="";
		log.infof("%s: getIcon of type %d",TAG,kind);
		switch (kind) {
		case BeanInfo.ICON_COLOR_16x16:
		case BeanInfo.ICON_MONO_16x16:
			imagePath = "/images/diagram_analyzer_16.png";
		case SimpleBeanInfo.ICON_COLOR_32x32:
		case SimpleBeanInfo.ICON_MONO_32x32:
		default:
			imagePath = "/images/diagram_analyzer_32.png";
		}
		Image img = new ImageIcon(getClass().getResource(imagePath)).getImage();
		return img;
	}
}
