/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BundleUtil;

/**
 *  Use Prefuse to display a map of Recommendations to Outputs. The map
 *  allows interaction to update the recommendation priorities.
 */
public class DiagramViewerComponent extends AbstractDiagramComponent {
	private static final long serialVersionUID = 4408313516136446100L;
	private static String PREFIX = BLTProperties.CUSTOM_PREFIX;              // For bundle identification
	

	public DiagramViewerComponent() {
		
		setName(BundleUtil.get().getString(PREFIX+".DiagramViewer.Name"));
		setHeading(BundleUtil.get().getString(PREFIX+".DiagramViewer.Display"));
		this.setOpaque(true);
		this.setBorder(border);
	}

	@Override
	public boolean isSquare() {return true; }
	@Override
	public void setHeading(String text) { heading = text; }

}
