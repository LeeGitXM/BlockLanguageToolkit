package com.ils.blt.designer.workspace.ui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;

import javax.swing.SwingUtilities;

import com.ils.blt.designer.workspace.ProcessBlockView;


/**
 * Create a block that depicts a tag reader.
 */
public class EntryUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 6644400470545202522L;
	private static final int DEFAULT_HEIGHT = 60;
	private static final int DEFAULT_WIDTH  = 80;

	
	public EntryUIView(ProcessBlockView view) {
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		setOpaque(false);
		initAnchorPoints();
		
	}

	// Draw a rectangle with pointed end
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
		Rectangle ifb = new Rectangle();   // Interior, frame and border
		ifb = SwingUtilities.calculateInnerArea(this,ifb);

		// Now translate so that 0,0 is is at the inner origin
		g.translate(ifb.x, ifb.y);
		// Now leave space for stubs
		ifb.x += INSET;
		ifb.y += INSET;
		ifb.width  -= 2*INSET;
		ifb.height -= 2*INSET;

		// Create a polygon that is within the component boundaries
		int[] xvertices = new int[] {ifb.x,ifb.x+(3*ifb.width/4),ifb.x+ifb.width,ifb.x+(3*ifb.width/4),ifb.x };
		int[] yvertices = new int[] {ifb.y+(3*ifb.height/4),ifb.y+(3*ifb.height/4),ifb.y+(ifb.height/2),ifb.y+(ifb.height/4),ifb.y+(ifb.height/4)};
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
		drawBadges(g);
	}

}
