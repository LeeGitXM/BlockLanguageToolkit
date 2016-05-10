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
//public class DiagnosisRenderer extends TableLabelRenderer {
	
public class DiagnosisDelegate implements TextDelegate {
	private final Map<Integer,Properties> propertyMap;
	private final ScriptManager scriptManager;

    public DiagnosisDelegate(Map<Integer,Properties> propMap) {
    	this.propertyMap = propMap;
    	scriptManager = BLTClientHook.getScriptManager();
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
        String problem = "";
		if( properties!=null && properties.getProperty(RecMapConstants.PROBLEM)!=null ) {
			problem = properties.getProperty(RecMapConstants.PROBLEM);
		}
        String multiplier = "0.0";
		if( properties!=null && properties.getProperty(RecMapConstants.MULTIPLIER)!=null ) {
			multiplier = properties.getProperty(RecMapConstants.MULTIPLIER);
		}
		sb.append(RecMapConstants.PROBLEM);
		sb.append(": ");
		sb.append(problem);
        sb.append("\n");
		sb.append(RecMapConstants.MULTIPLIER);
		sb.append(": ");
		sb.append(multiplier);
        sb.append("\n");
        return sb.toString();
    }
    /**
     * Returns the text that appears in the block header. 
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    @Override
    public String getHeaderText(VisualItem item) {
        StringBuilder sb = new StringBuilder();
        sb.append(item.getString(RecMapConstants.NAME)); 
        return sb.toString();
    }

	@Override
	public String getTooltipText(VisualItem item) {
		int row = item.getInt(RecMapConstants.ROW);
		Properties properties = propertyMap.get(new Integer(row));
		return getHtml(item,properties);
	}
	
	private String getHtml(VisualItem item,Properties properties) {
		String name = "DIAGNOSIS";
		if( item.canGetString(RecMapConstants.NAME) ) {
			name = item.getString(RecMapConstants.NAME);
		}
		String problem = "PROBLEM";
		if( properties!=null && properties.getProperty(RecMapConstants.PROBLEM)!=null ) {
			problem = properties.getProperty(RecMapConstants.PROBLEM);
		}
		String multiplier = "1.0";
		if( properties!=null && properties.getProperty(RecMapConstants.MULTIPLIER)!=null ) {
			String prop = properties.getProperty(RecMapConstants.MULTIPLIER);
			if( prop!=null) multiplier = String.valueOf(prop);
		}
		String html = 
			"<html>" + 
				"<div style=\"background:rgb(204,215,235);border-style:solid;border-width:3px 0px 0px 0px;border-color:rgb(250,250,250)\">" +
					"<center><h3>"+name+"</h3></center>" +
				"</div>" +
				"<div>" +
				"<table>" +
				"<tr>" +
				"<td>Problem:</td><td>"+problem+"</td>"+
				"</tr>" +
				"<tr>" +
				"<td>Multiplier:</td><td>"+multiplier+"</td>"+
				"</tr>" +
				"</table>" +
				"</div>" +
			"</html>";
		return html;
	}
	
	@Override
	public void addMenuItems(VisualItem item,JPopupMenu menu) {
		int row = item.getInt(RecMapConstants.ROW);
		Properties properties = propertyMap.get(new Integer(row));
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
