package com.ils.blt.designer.workspace.ui;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;

import javax.swing.SwingUtilities;

import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;

@SuppressWarnings("serial")
public class SquareUIView extends AbstractUIView implements BlockViewUI {
	private static final int EXCESS_INSET = 3;
	
	public SquareUIView(ProcessBlockView view) {
		super(view);
		setOpaque(false);
		setPreferredSize(new Dimension(100,100));
		initAnchorPoints();	
	}


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
		ifb.x += INSET+EXCESS_INSET;
		ifb.y += INSET+EXCESS_INSET;
		ifb.width  -= 2*(INSET+EXCESS_INSET);
		ifb.height -= 2*(INSET+EXCESS_INSET);

		// Create a square that is within the component boundaries
		int[] xvertices = new int[] {ifb.x,ifb.x+ifb.width,ifb.x+ifb.width,ifb.x };
		int[] yvertices = new int[] {ifb.y,ifb.y,ifb.y+ifb.height,ifb.y+ifb.height};
		Polygon fi = new Polygon(xvertices,yvertices,4);
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
		drawEmbeddedIcon(g);
		drawEmbeddedText(g);
	}

}
