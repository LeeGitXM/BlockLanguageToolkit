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
		return getHtml(item,properties);
	}
	
	private String getHtml(VisualItem item,Properties properties) {
		String name = "Recommendation";  // There is no name in the dataset
		String auto = "0.0";             // Whatever this means
		if( properties!=null && properties.getProperty(RecMapConstants.AUTO)!=null ) {
			String prop = properties.getProperty(RecMapConstants.AUTO);
			if( prop!=null) auto = String.valueOf(prop);
		}
		String html = 
			"<html>" + 
				"<div style=\"background:rgb(172,185,190);border-style:solid;border-width:3px 0px 0px 0px;border-color:rgb(250,250,250)\">" +
					"<center><h3>"+name+"</h3></center>" +
				"</div>" +
				"<div>" +
				"<table>" +
				"<tr>" +
				"<td>Auto:</td><td>"+auto+"</td>"+
				"</tr>" +
				"</table>" +
				"</div>" +
			"</html>";
		return html;
	}
} 
