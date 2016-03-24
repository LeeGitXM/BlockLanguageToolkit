/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import java.awt.Color;

import prefuse.action.assignment.ColorAction;
import prefuse.util.ColorLib;
import prefuse.visual.VisualItem;
import prefuse.visual.tuple.TableEdgeItem;

/**
 * Render an edge with colors depending on whether or not the edge is active.
 * The active color is a dark green,
 * the inactive color a dark gray
 */
public class EdgeColorAction extends ColorAction {

    public EdgeColorAction(String group) {
        super(group, VisualItem.STROKECOLOR);
    }
    
    @Override
    public int getColor(VisualItem item) {
    	int color = ColorLib.rgb(220,220,220);  // Gray
        if ( item instanceof TableEdgeItem && item.canGetBoolean(RecMapConstants.ACTIVE) ) {
        	boolean active = item.getBoolean(RecMapConstants.ACTIVE);
        	if( active ) color= ColorLib.rgb(27,88,27);
        	else color= ColorLib.rgb(180,180,180);
        }
        return color;
    }
    
    // There should be no default case. Return yellow
    @Override
    public int getDefaultColor() {
    	return  Color.YELLOW.getRGB();
    }
}  
