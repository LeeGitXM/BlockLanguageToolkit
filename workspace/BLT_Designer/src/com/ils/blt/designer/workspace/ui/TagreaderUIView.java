package com.ils.blt.designer.workspace.ui;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;
import java.util.List;

import javax.swing.SwingUtilities;

import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;


/**
 * Create a block that depicts a tag reader.
 */
public class TagreaderUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 6644400470545202522L;
	private final static int BORDER_WIDTH=10;
	private final static int STUB_BUFFER=15;        // Save room all around for the stubs
	private final static float HEIGHT_FACTOR=0.5f;
	private final static Color BORDER_SHADOW_COLOR = Color.DARK_GRAY;
	private final static Color BORDER_HIGHLIGHT_COLOR = Color.magenta;   // Want maroon
	private final static Color HEADING_COLOR = Color.BLACK;
	
	public TagreaderUIView(ProcessBlockView view) {
		super(view);
		setOpaque(false);
		setPreferredSize(new Dimension(100,100));
		initAnchorPoints();
		
	}

	@Override
	public void install(BlockComponent panel) {
		panel.setLayout(new BorderLayout());
		panel.add(this,BorderLayout.CENTER);
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
		ifb.x += STUB_BUFFER;
		ifb.y += STUB_BUFFER;
		ifb.width  -= 2*STUB_BUFFER;
		ifb.height -= 2*STUB_BUFFER;

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

		/*
		// Create a Triangle within the bounds of rectangle fi
		int width = fi.width;
		int height = fi.height;
		int[] xvertices = new int[] {fi.x,fi.x+width,fi.x+(width/2) };
		int[] yvertices = new int[] {fi.y+height,fi.y+height,fi.y};

		Polygon fip = new Polygon(xvertices,yvertices,3);  // Triangle
		g.setColor(getForeground());
		g.fillPolygon(fip);

		// Create a circle that is the interior; surrounds the text.
		float centerx = ifb.width / 2.0f;
		float centery = (ifb.height / 2.0f); // Nominal
		centery = centery + (centery*(1.0f-HEIGHT_FACTOR));  // Lower a bit
		float radius =  (fi.height / 2.0f)/2.0f; 
		Ellipse2D.Float interior = new Ellipse2D.Float(centerx-radius,centery-radius,2*radius,2*radius);

		g.setColor(getBackground());
		g.fillOval((int)interior.x, (int)interior.y, (int)interior.width,(int)interior.height);

		// Set the font for drawing the Heading
		g.setFont(getFont());
		FontMetrics fm = g.getFontMetrics();
		// Calculate the x,y for the String's baseline in order to center it
		String text = getBlock().getLabel();
		if( text!=null ) {
			int stringWidth = fm.stringWidth(text);
			int xpos = (ifb.width - stringWidth) / 2;
			int ypos = (int)(ifb.getHeight() / 2f); // Nominal
			ypos = ypos +(int)(ypos*(1.0f-HEIGHT_FACTOR));
			paintTextAt(g, text, xpos, ypos, HEADING_COLOR);
		}
		
		// Paint the borders
		float borderWidth = BORDER_WIDTH;
		Stroke stroke = new BasicStroke(borderWidth,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		g.setPaint(BORDER_SHADOW_COLOR);
		g.drawLine(ifb.x, ifb.y+ifb.height, ifb.x+ifb.width, ifb.y+ifb.height);
		g.drawLine(ifb.x+((ifb.x+ifb.width)/2), ifb.y, ifb.x+ifb.width, ifb.y+ifb.height);
		g.setPaint(BORDER_HIGHLIGHT_COLOR);
		g.drawLine(ifb.x, ifb.y+ifb.height, ifb.x+(ifb.width/2), ifb.y);

		// Outline the frame
		float outlineWidth = 1.0f;
		stroke = new BasicStroke(outlineWidth,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		g.setPaint(Color.BLACK);
		g.draw(fip);
		g.draw(interior);
*/
		// Reverse any transforms we made
		g.setTransform(originalTx);
		drawAnchors(g);
	}

}
