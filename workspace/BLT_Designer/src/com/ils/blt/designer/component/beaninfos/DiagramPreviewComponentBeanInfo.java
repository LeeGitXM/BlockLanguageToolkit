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
import com.inductiveautomation.factorypmi.designer.beaninfo.BasicContainerBeanInfo;

/**
 * Define properties accessible in the designer properties editor for the CallbackBlock.
 * Also set the icon.
 */
public class DiagramPreviewComponentBeanInfo extends BasicBlockBeanInfo {
	private static final String TAG = "DiagramPreviewComponentBeanInfo";
	private ResourceBundle bundle = null;

	
	/**
	 * Constructor: Create a beaninfo object for the CallbackBlock. The
	 *              superclass fills in common properties and customizers.
	 */
	public DiagramPreviewComponentBeanInfo() {
		super(DiagramPreviewComponentBeanInfo.class);
		
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
		bundle = ResourceBundle.getBundle(BLTProperties.BLOCK_RESOURCE_PATH);
		getBeanDescriptor().setName(getString("Name"));
		getBeanDescriptor().setDisplayName(getString("Display"));       // Tooltip-title
		getBeanDescriptor().setShortDescription(getString("Desc"));     // Tooltip-description
		super.initDesc();
	}

	/**
	 * Use the bundle utility to lookup the subject string in designer.properties.
	 * @param string bundle key
	 * @return result from the properties file.
	 */
	private String getString(String string) {
		if( bundle==null) log.error(TAG+" Error null bundle");
		return bundle.getString("DiagramPreview."+string);
	}
	@Override
	public Image getIcon(int kind) {
		String imagePath="";
		log.info(TAG+"getIcon of type "+kind);
		switch (kind) {
		case BeanInfo.ICON_COLOR_16x16:
		case BeanInfo.ICON_MONO_16x16:
			imagePath = "images/diagram_previw_16.png";
		case SimpleBeanInfo.ICON_COLOR_32x32:
		case SimpleBeanInfo.ICON_MONO_32x32:
			imagePath = "/images/diagram_preview_32.png";
		}
		Image img = new ImageIcon(getClass().getResource(imagePath)).getImage();
		return img;
	}
}
