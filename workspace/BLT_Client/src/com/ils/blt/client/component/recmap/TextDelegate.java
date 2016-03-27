/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import java.util.Properties;

import prefuse.visual.VisualItem;

/**
 * Delegates are provide label rendering information for 
 * specific node types.
 */
public interface TextDelegate {
	/**
     * @return delimited text for the body of the block
     */
    public String getBodyText(VisualItem item,Properties properties);
    /**
     * @return single-line text that is the header of the block
     */
    public String getHeaderText(VisualItem item,Properties properties);

    /**
     * @return a tooltip for the VisualItem
     */
    public String getTooltipText(VisualItem item,Properties properties);
} 
