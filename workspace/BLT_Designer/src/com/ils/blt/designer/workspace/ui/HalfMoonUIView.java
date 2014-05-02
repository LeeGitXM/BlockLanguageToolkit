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
 * Create a circular "button" with a predefined 48x48 graphic. The first input anchor
 * creates an anchor point on the left. The first output anchor point creates an 
 * anchor point on the top.
 */
public class HalfMoonUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 2180868310475735865L;
	private static final int DEFAULT_HEIGHT = 80;
	private static final int DEFAULT_WIDTH  = 80;
	
	public HalfMoonUIView(ProcessBlockView view) {
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
		Color originalBackground = g.getBackground();

		// Turn on anti-aliasing
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);

		// For drawing outlines
		float outlineWidth = 1.0f;
		Stroke stroke = new BasicStroke(outlineWidth,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		g.setPaint(Color.BLACK);
		
		// Calculate the inner area - the inner area includes the border
		Dimension sz = getPreferredSize();
		int inset = INSET;
		int width = sz.width-2*inset;
		int height = sz.height-2*inset;
		// Now translate so that 0,0 is is at the inner origin (inside insets in both directions)
		g.translate(inset, inset);

		// Create a polygon for the upper left  
		int[] ulxvertices = new int[] {0,0,2*width/3,width,0};
		int[] ulyvertices = new int[] {height,0,0,height/4,height};
		Polygon fi = new Polygon(ulxvertices,ulyvertices,4);
		g.setColor(BORDER_LIGHT_COLOR);
		g.fillPolygon(fi);
		g.draw(fi);   // Outline
		
		// Create polygons for the lower right. 
		int[] lrxvertices = new int[] {0,width,width,2*width/3,0};
		int[] lryvertices = new int[] {height, height/4,3*height/4,height,height};
		fi = new Polygon(lrxvertices,lryvertices,5);
		g.setColor(BORDER_DARK_COLOR);
		g.fillPolygon(fi);
		g.draw(fi);   // Outline

		
		// Draw the inner area.
		inset = INSET+BORDER_WIDTH;
		width = sz.width-2*inset;
		height = sz.height-2*inset;
		// Now translate so that 0,0 is is at the inner origin (inside the inset and border)
		g.translate(BORDER_WIDTH, BORDER_WIDTH);
		// Create a polygon that is inside the border
		int[]xvertices = new int[] {0,2*width/3,width,width,2*width/3,0,0};
		int[]yvertices = new int[] {0,0,height/4,3*height/4,height,height,0};
		fi = new Polygon(xvertices,yvertices,7);
		g.setColor(new Color(block.getBackground()));
		g.fillPolygon(fi);
		g.draw(fi);
		
		// Reverse any transforms we made
		g.setTransform(originalTx);
		g.setBackground(originalBackground);
		drawAnchors(g);
		drawBadges(g);
		drawEmbeddedText(g);
	}

}
