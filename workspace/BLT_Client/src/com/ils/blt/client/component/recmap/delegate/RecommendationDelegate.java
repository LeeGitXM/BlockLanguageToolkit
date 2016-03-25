/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap.delegate;

import com.ils.blt.client.component.recmap.RecMapConstants;
import com.ils.blt.client.component.recmap.TextDelegate;

import prefuse.visual.VisualItem;

/**
 * Render the block that holds recommendations.
 */
public class RecommendationDelegate implements TextDelegate {

    /**
     */
    public RecommendationDelegate() {
    }
   
    /**
     * Returns the text to draw. Subclasses can override this class to
     * perform custom text selection.
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    protected String getText(VisualItem item) {
        String s = null;
        s = item.getString(RecMapConstants.NAME); 
        return s;
    }

	@Override
	public String getBodyText(VisualItem item) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getHeaderText(VisualItem item) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getTooltipText(VisualItem item) {
		return "Recommendaation";
	}
} 
