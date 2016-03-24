/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.blt.client.component.recmap;

import prefuse.render.EdgeRenderer;
import prefuse.visual.VisualItem;

/**
 * Render an edge with different thicknesses depending
 * on whether or not the edge is active.
 */
public class RecMapEdgeRenderer extends EdgeRenderer {

	public RecMapEdgeRenderer(int lineType) {
		super(lineType);
	}
	
	@Override
    protected double getLineWidth(VisualItem item) {
		double width = 2.0;
        if ( item.canGetBoolean(RecMapConstants.ACTIVE) ) {
        	boolean active = item.getBoolean(RecMapConstants.ACTIVE);
        	if( active ) width = 8.0;           
        }
        return width;
    }
} 
