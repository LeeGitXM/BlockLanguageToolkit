/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.component.beaninfos;

import java.awt.Image;
import java.beans.BeanInfo;
import java.beans.IntrospectionException;

import javax.swing.ImageIcon;

import com.ils.blt.common.BLTProperties;
import com.ils.common.component.recmap.RecMapConstants;
import com.ils.common.component.recmap.RecommendationMap;
import com.inductiveautomation.factorypmi.designer.property.customizers.DynamicPropertyProviderCustomizer;
import com.inductiveautomation.factorypmi.designer.property.customizers.StyleCustomizer;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.vision.api.designer.beans.CommonBeanInfo;
import com.inductiveautomation.vision.api.designer.beans.CustomizerDescriptor;


/**
 * Define properties accessible in the designer properties editor for the RecommendationMap.
 * Also set the icon.
 */
public class RecommendationMapBeanInfo extends CommonBeanInfo {
	private static final String TAG = "RecommendationMapBeanInfo";
	private static final String PREFIX = BLTProperties.BLOCK_PREFIX;  // Has block properties
	private static final LoggerEx logger = LogUtil.getLogger(RecommendationMapBeanInfo.class.getPackage().getName());
	
	/**
	 * Constructor: Create a beaninfo object for the RecommendationMap. The
	 *              superclass fills in common properties and customizers.
	 */
	public RecommendationMapBeanInfo() {
		super(RecommendationMap.class, new CustomizerDescriptor[] {
				DynamicPropertyProviderCustomizer.VALUE_DESCRIPTOR,
				StyleCustomizer.VALUE_DESCRIPTOR});
	}

	@Override
	protected void initProperties() throws IntrospectionException {
		// Adds common properties
		super.initProperties();
		
		addBoundProp(RecMapConstants.CONNECTIONS_PROPERTY, "Connections", "A map of final diagnosis to outputs", 
                CAT_DATA,PREFERRED_MASK | BOUND_MASK | EXPERT_MASK);
		addBoundProp(RecMapConstants.DIAGNOSES_PROPERTY, "Diagnoses", "A list of final diagnosis values", 
					                                       CAT_DATA,PREFERRED_MASK | BOUND_MASK | EXPERT_MASK);
		addBoundProp(RecMapConstants.OUTPUTS_PROPERTY, "Outputs", "A list of outputs for which recommendations apply", 
                                                           CAT_DATA,PREFERRED_MASK | BOUND_MASK | EXPERT_MASK);
		addBoundProp(RecMapConstants.RECOMMENDATIONS_PROPERTY, "Recommendations", "An editable list of recommendations", 
                                                           CAT_DATA,PREFERRED_MASK | BOUND_MASK | EXPERT_MASK);
		
	}
	
	/**
	 * Add the bean descriptors to set name, display name and short description.
	 * These strings are derived from designer.properties.
	 */
	@Override
	protected void initDesc() {
		logger.tracef("%s.initDesc",TAG);
		getBeanDescriptor().setName(BundleUtil.get().getString(PREFIX+".RecommendationMap.Name"));
		getBeanDescriptor().setDisplayName(BundleUtil.get().getString(PREFIX+".RecommendationMap.Display"));       // Tooltip-title
		getBeanDescriptor().setShortDescription(BundleUtil.get().getString(PREFIX+".RecommendationMap.Desc"));     // Tooltip-description
		super.initDesc();
	}

	
	@Override
	public Image getIcon(int kind) {
		logger.tracef("%s: getIcon of type %d",TAG,kind);
		switch (kind) {
		case BeanInfo.ICON_COLOR_16x16:
		case BeanInfo.ICON_MONO_16x16:
			return new ImageIcon(getClass().getResource("/images/recommendation_map_16.png")).getImage();
		case BeanInfo.ICON_COLOR_32x32:
		case BeanInfo.ICON_MONO_32x32:
		default:
			return new ImageIcon(getClass().getResource("/images/recommendation_map_32.png")).getImage();
		}
	}
}
