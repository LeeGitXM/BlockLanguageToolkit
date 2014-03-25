package com.ils.blt.designer.workspace.ui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;

import com.ils.blt.designer.workspace.ProcessBlockView;


/**
 * Create a block that depicts a and or or junction.
 * It looks like a stubby entry block.
 */
public class JunctionUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 6634400470545202522L;
	private static final int DEFAULT_HEIGHT = 50;
	private static final int DEFAULT_WIDTH  = 40;
	
	public JunctionUIView(ProcessBlockView view) {
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		setOpaque(false);
		initAnchorPoints();
		
	}
	

	// We punt on a crescent. Draw 
	@Override
	protected void paintComponent(Graphics _g) {
		// Calling the super method effects an "erase".
		Graphics2D g = (Graphics2D) _g;

		// Preserve the original transform to roll back to at the end
		AffineTransform originalTx = g.getTransform();

		// Turn on anti-aliasing
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
	
		// Calculate the inner area
		Dimension sz = getPreferredSize();
		Rectangle ifb = new Rectangle(INSET,INSET,sz.width-2*INSET,sz.height-2*INSET);   // x,y,width,height

		// Now translate so that 0,0 is is at the inner origin
		g.translate(ifb.x, ifb.y);

		// Create a polygon that is within the component boundaries
		int[] xvertices = new int[] {0,ifb.width/2,ifb.width,ifb.width,ifb.width/2,0,0};
		int[] yvertices = new int[] {0,0,ifb.height/3,2*ifb.height/3,ifb.height,ifb.height,0};
		Polygon fi = new Polygon(xvertices,yvertices,5);
		g.setColor(getBackground());
		g.fillPolygon(fi);
		
		// Outline the frame
		float outlineWidth = 1.0f;
		Stroke stroke = new BasicStroke(outlineWidth,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		g.setPaint(Color.BLACK);
		g.draw(fi);

		// Reverse any transforms we made
		g.setTransform(originalTx);
		drawAnchors(g);
	}

}
