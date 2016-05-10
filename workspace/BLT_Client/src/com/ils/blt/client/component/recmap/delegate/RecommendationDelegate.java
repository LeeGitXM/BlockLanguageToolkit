/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap.delegate;

import java.awt.event.ActionEvent;
import java.util.Map;
import java.util.Properties;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import com.ils.blt.client.BLTClientHook;
import com.ils.blt.client.component.recmap.RecMapConstants;
import com.ils.blt.client.component.recmap.TextDelegate;
import com.inductiveautomation.ignition.common.script.ScriptManager;

import prefuse.visual.VisualItem;

/**
 * Render the block that holds recommendations.
 */
public class RecommendationDelegate implements TextDelegate {
	private final Map<Integer,Properties> propertyMap;
	private final ScriptManager scriptManager;

    public RecommendationDelegate(Map<Integer,Properties> propMap) {
    	this.propertyMap = propMap;
    	scriptManager = BLTClientHook.getScriptManager();
    }
    /**
     * Returns the text to draw in the block header.
     * The name is simply "Recommendation"
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    @Override
    public String getHeaderText(VisualItem item) {
        return "Recommendation";
    }
    /**
     * Returns the text to draw. multiple properties separated
     * by a blank lines.
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    @Override
    public String getBodyText(VisualItem item) {
    	int row = item.getInt(RecMapConstants.ROW);
		Properties properties = propertyMap.get(new Integer(row));
        StringBuilder sb = new StringBuilder();
        String auto = "0.0";
        String manual = "0.0";
        boolean isAuto = false;
        
		if( properties!=null && properties.getProperty(RecMapConstants.IS_AUTO)!=null ) {
			isAuto = properties.getProperty(RecMapConstants.IS_AUTO).equalsIgnoreCase("AUTO");
		}
		
		if( properties!=null && properties.getProperty(RecMapConstants.AUTO)!=null ) {
			auto = properties.getProperty(RecMapConstants.AUTO);
		}
		if( properties!=null && properties.getProperty(RecMapConstants.MANUAL)!=null ) {
			manual = properties.getProperty(RecMapConstants.MANUAL);
		}
		if( isAuto ) {
			sb.append(RecMapConstants.AUTO);
			sb.append(": ");
			sb.append(auto);
			sb.append("\n");
		}
		else {
			sb.append(RecMapConstants.MANUAL);
			sb.append(": ");
			sb.append(manual);
			sb.append(" (");
			sb.append(RecMapConstants.AUTO);
			sb.append(": ");
			sb.append(auto);
			sb.append(")\n");
		}
        return sb.toString();
    }

	@Override
	public String getTooltipText(VisualItem item) {
		int row = item.getInt(RecMapConstants.ROW);
		Properties properties = propertyMap.get(new Integer(row));
		return getHtml(item,properties);
	}
	
	private String getHtml(VisualItem item,Properties properties) {
		String name = "Recommendation";  // There is no name in the dataset
		String auto = "0.0";             
		String manual = "0.0";             
		String autoOrManual = "";
		if( properties!=null && properties.getProperty(RecMapConstants.IS_AUTO)!=null ) {
			autoOrManual = properties.getProperty(RecMapConstants.IS_AUTO).toUpperCase();
		}
		if( properties!=null && properties.getProperty(RecMapConstants.AUTO)!=null ) {
			String prop = properties.getProperty(RecMapConstants.AUTO);
			if( prop!=null) auto = String.valueOf(prop);
		}
		if( properties!=null && properties.getProperty(RecMapConstants.MANUAL)!=null ) {
			String prop = properties.getProperty(RecMapConstants.MANUAL);
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
				"<td>Manual:</td><td>"+manual+"</td>"+
				"<td>Auto:</td><td>"+auto+"</td>"+
				"</tr>" +
				"<tr>" +
				"<td>"+autoOrManual+"</td>"+
				"</tr>" +
				"</table>" +
				"</div>" +
			"</html>";
		return html;
	}
	
	@Override
	public void addMenuItems(VisualItem item,JPopupMenu menu) {
		JMenuItem menuItem;
		menuItem = new JMenuItem("Output");
	    menuItem.addActionListener(this);
	    menu.add(menuItem);
	}
	
	// ========================================== Action Listener ============================================
	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub
		
	}
} 
