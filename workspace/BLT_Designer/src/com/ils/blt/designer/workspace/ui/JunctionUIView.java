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
 * Create a block that depicts a junction.
 * It looks like a tiny square block.
 */
public class JunctionUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 6635500470545202522L;
	private static final int DEFAULT_HEIGHT = 50;
	private static final int DEFAULT_WIDTH  = 50;
	private static final int JUNCTION_BORDER_WIDTH = 2;
	
	public JunctionUIView(ProcessBlockView view) {
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		setOpaque(false);
		initAnchorPoints();
		
	}
	

	// Draw a small square with a thin border
	@Override
	protected void paintComponent(Graphics _g) {
		// Calling the super method effects an "erase".
		Graphics2D g = (Graphics2D) _g;

		// Preserve the original transform to roll back to at the end
		AffineTransform originalTx = g.getTransform();
		Color originalBackground = g.getBackground();

		// Turn on anti-aliasing
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
		// Setup for outlining
		float outlineWidth = 1.0f;
		Stroke stroke = new BasicStroke(outlineWidth,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		g.setPaint(Color.BLACK);
		
		// Calculate the inner area
		Rectangle ifb = new Rectangle();   // Interior, frame and border
		ifb = SwingUtilities.calculateInnerArea(this,ifb);
		// Now translate so that 0,0 is is at the inner origin
		g.translate(ifb.x, ifb.y);
		// Now leave space for stubs and border
		int inset = INSET;
		ifb.x += inset;
		ifb.y += inset;
		ifb.width  -= 2*(inset);
		ifb.height -= 2*(inset);
		// Create a square for the border that is within the insets. 
		// Use the upper left for light shading, the lower right for dark
		int[] xulvertices = new int[] {ifb.x,            ifb.x,ifb.x+ifb.width,ifb.x };
		int[] yulvertices = new int[] {ifb.y+ifb.height, ifb.y,ifb.y,ifb.y+ifb.height};
		Polygon fi = new Polygon(xulvertices,yulvertices,4);
		g.setColor(BORDER_LIGHT_COLOR);
		g.fillPolygon(fi);
		g.draw(fi);
		
		int[] xlrvertices = new int[] {ifb.x,ifb.x+ifb.width,  ifb.x+ifb.width,ifb.x };
		int[] ylrvertices = new int[] {ifb.y+ifb.height, ifb.y,ifb.y+ifb.height,ifb.y+ifb.height};
		fi = new Polygon(xlrvertices,ylrvertices,4);
		g.setColor(BORDER_DARK_COLOR);
		g.fillPolygon(fi);
		g.draw(fi);

		ifb.x += JUNCTION_BORDER_WIDTH;
		ifb.y += JUNCTION_BORDER_WIDTH;
		ifb.width  -= 2*(JUNCTION_BORDER_WIDTH);
		ifb.height -= 2*(JUNCTION_BORDER_WIDTH);
		// Create a square that is within the border boundaries
		int[] xvertices = new int[] {ifb.x,ifb.x+ifb.width,ifb.x+ifb.width,ifb.x };
		int[] yvertices = new int[] {ifb.y,ifb.y,ifb.y+ifb.height,ifb.y+ifb.height};
		fi = new Polygon(xvertices,yvertices,4);
		g.setColor(new Color(block.getBackground()));
		g.fillPolygon(fi);
		// Outline the inner square
		g.setPaint(INSET_COLOR);
		g.draw(fi);

		// Reverse any transforms we made
		g.setTransform(originalTx);
		g.setBackground(originalBackground);
		drawAnchors(g,0,0);
	}

}
