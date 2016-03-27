/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap.delegate;

import java.util.Properties;

import com.ils.blt.client.component.recmap.RecMapConstants;
import com.ils.blt.client.component.recmap.TextDelegate;

import prefuse.visual.VisualItem;

/**
 * Render the block that holds Quant Output attributes.
 */
public class OutputDelegate implements TextDelegate {
	

    /**
     */
    public OutputDelegate() {
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
        String currentSetpoint = "0.0";
		if( properties!=null && properties.getProperty(RecMapConstants.CURRENT)!=null ) {
			currentSetpoint = properties.getProperty(RecMapConstants.CURRENT);
		}
		sb.append(currentSetpoint);
        sb.append("\n");
        return sb.toString();
    }
    /**
     * Returns the text that appears in the block header. 
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    @Override
    public String getHeaderText(VisualItem item,Properties properties) {
        StringBuilder sb = new StringBuilder();
        sb.append(RecMapConstants.NAME);
        sb.append(": ");
        sb.append(item.getString(RecMapConstants.NAME)); 
        return sb.toString();
    }
	@Override
	public String getTooltipText(VisualItem item,Properties properties) {
		return "<html><h1>Output Delegate</h1></html>";
	}
} 
