/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.common.component.recmap;

import java.awt.Shape;

import prefuse.Constants;
import prefuse.render.EdgeRenderer;
import prefuse.visual.EdgeItem;
import prefuse.visual.VisualItem;

/**
 * Render an edge with different thicknesses depending
 * on whether or not the edge is active.
 */
public class RecMapEdgeRenderer extends EdgeRenderer {

	public RecMapEdgeRenderer(int lineType) {
		super(lineType);
	}
	
	/**
	 * This is a much simplified version of the superior method. 
	 * It handles straight lines only. The standard method left a 
	 * gap before the downstream block ... this version appears
	 * to work just fine.
	 */
	@Override
	protected Shape getRawShape(VisualItem item) {
		EdgeItem edge = (EdgeItem)item;
		VisualItem item1 = edge.getSourceItem();
		VisualItem item2 = edge.getTargetItem();
		
		getAlignedPoint(m_tmpPoints[0],item1.getBounds(),Constants.RIGHT,Constants.CENTER);
		getAlignedPoint(m_tmpPoints[1],item2.getBounds(),Constants.LEFT,Constants.CENTER);
		Shape shape = null;
		double n1x  =  m_tmpPoints[0].getX();
		double n1y  =  m_tmpPoints[0].getY();
		double n2x  =  m_tmpPoints[1].getX(); 
		double n2y  =  m_tmpPoints[1].getY();
		
		m_curWidth = (float)(m_width*getLineWidth(item));
		m_line.setLine(n1x, n1y, n2x, n2y);

		shape = m_line;
		return shape;	
	}
	
	@Override
    protected double getLineWidth(VisualItem item) {
		double width = 1.0;
        if ( item.canGetBoolean(RecMapConstants.ACTIVE) ) {
        	boolean active = item.getBoolean(RecMapConstants.ACTIVE);
        	if( active ) width = 7.0;           
        }
        return width;
    }
} 
