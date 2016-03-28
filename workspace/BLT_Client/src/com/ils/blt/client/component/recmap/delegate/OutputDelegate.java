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
		sb.append(RecMapConstants.CURRENT);
		sb.append(": ");
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
        sb.append(item.getString(RecMapConstants.NAME)); 
        return sb.toString();
    }
    
	@Override
	public String getTooltipText(VisualItem item,Properties properties) {
		return getHtml(item,properties);
	}
	
	private String getHtml(VisualItem item,Properties properties) {
		String name = "QUANT OUTPUT";
		if( item.canGetString(RecMapConstants.NAME) ) {
			name = item.getString(RecMapConstants.NAME);
		}
		
		String currentSetpoint = "0.0";
		if( properties!=null && properties.getProperty(RecMapConstants.CURRENT)!=null ) {
			String prop = properties.getProperty(RecMapConstants.CURRENT);
			if( prop!=null) currentSetpoint = String.valueOf(prop);
		}
		String finalSetpoint = "0.0";
		if( properties!=null && properties.getProperty(RecMapConstants.FINAL)!=null ) {
			String prop = properties.getProperty(RecMapConstants.FINAL);
			if( prop!=null) finalSetpoint = String.valueOf(prop);
		}
		String target = "0.0";
		if( properties!=null && properties.getProperty(RecMapConstants.TARGET)!=null ) {
			String prop = properties.getProperty(RecMapConstants.TARGET);
			if( prop!=null) target = String.valueOf(prop);
		}
		String recommendation = "0.0";
		if( properties!=null && properties.getProperty(RecMapConstants.RECOMMENDATION)!=null ) {
			String prop = properties.getProperty(RecMapConstants.RECOMMENDATION);
			if( prop!=null) recommendation = String.valueOf(prop);
		}
		String html = 
			"<html>" + 
				"<div style=\"background:rgb(195,207,235);border-style:solid;border-width:3px 0px 0px 0px;border-color:rgb(250,250,250)\">" +
					"<center><h3>"+name+"</h3></center>" +
				"</div>" +
				"<div>" +
				"<table>" +
				"<tr>" +
				"<td>Current Setpoint:</td><td>"+currentSetpoint+"</td>"+
				"</tr>" +
				"<tr>" +
				"<td>Final Setpoint:</td><td>"+finalSetpoint+"</td>"+
				"</tr>" +
				"<tr>" +
				"<td>Target:</td><td>"+target+"</td>"+
				"</tr>" +
				"<tr>" +
				"<td>Recommendation:</td><td>"+recommendation+"</td>"+
				"</tr>" +
				"</table>" +
				"</div>" +
			"</html>";
		return html;
	}
} 
