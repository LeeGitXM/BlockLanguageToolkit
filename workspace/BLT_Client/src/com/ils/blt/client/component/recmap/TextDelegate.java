/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import prefuse.visual.VisualItem;

/**
 * Delegates are provide label rendering information for 
 * specific node types.
 */
public interface TextDelegate {
	/**
     * @return delimited text for the body of the label block
     */
    public String getBodyText(VisualItem item);
	/**
     * @return delimited text for the header of the label block
     */
    public String getHeaderText(VisualItem item);
    /**
     * @return a tooltip for the VisualItem
     */
    public String getTooltipText(VisualItem item);
} 
