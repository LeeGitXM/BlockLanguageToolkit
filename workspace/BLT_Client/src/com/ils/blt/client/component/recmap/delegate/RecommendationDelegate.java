/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap.delegate;

import java.util.Properties;

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
     * Returns the text to draw in the block header.
     * The name is simply "Recommendation"
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    @Override
    public String getHeaderText(VisualItem item,Properties properties) {
        return "Recommendation";
    }
    /**
     * Returns the text to draw. multiple properties separated
     * by a blank lines.
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    @Override
    public String getBodyText(VisualItem item,Properties properties) {
        StringBuilder sb = new StringBuilder();
        String auto = "0.0";
		if( properties!=null && properties.getProperty(RecMapConstants.AUTO)!=null ) {
			auto = properties.getProperty(RecMapConstants.AUTO);
		}
		sb.append(RecMapConstants.AUTO);
		sb.append(": ");
		sb.append(auto);
        sb.append("\n");
        return sb.toString();
    }

	@Override
	public String getTooltipText(VisualItem item,Properties properties) {
		return "Recommendaation";
	}
} 
