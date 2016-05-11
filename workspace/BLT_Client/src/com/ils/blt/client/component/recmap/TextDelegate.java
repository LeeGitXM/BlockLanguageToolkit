/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import javax.swing.JPopupMenu;

import prefuse.visual.VisualItem;

/**
 * Delegates are provide label rendering information for 
 * specific node types. The delegate also handles the popup menu
 * choices. A property map is supplied to the delegate in its constructor.
 */
public interface TextDelegate  {
	/**
     * @return delimited text for the body of the block
     */
    public String getBodyText(VisualItem item);
    /**
     * @return single-line text that is the header of the block
     */
    public String getHeaderText(VisualItem item);

    /**
     * @return a tooltip for the VisualItem
     */
    public String getTooltipText(VisualItem item);
    
    /**
     * Add and handle popup menu selections.
     * @param menu the popup
     */
    public void addMenuItems(VisualItem item,JPopupMenu menu);
} 
