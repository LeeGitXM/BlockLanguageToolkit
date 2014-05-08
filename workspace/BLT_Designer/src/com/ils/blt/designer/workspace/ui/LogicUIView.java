package com.ils.blt.designer.workspace.ui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;

import javax.swing.SwingUtilities;

import com.ils.blt.designer.workspace.ProcessBlockView;


/**
 * Draw various styles of logic blocks according to (roughly) "google logic gate symbols". 
 */
public class LogicUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 2180868310475735865L;
	public static final String SUBSTYLE_AND = "AND";
	public static final String SUBSTYLE_NOT = "NOT";
	public static final String SUBSTYLE_OR = "OR";
	private static final int DEFAULT_HEIGHT = 80;
	private static final int DEFAULT_WIDTH  = 80;
	private final String substyle;
	
	public LogicUIView(ProcessBlockView view,String ss) {
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		this.substyle = ss;
		setOpaque(false);
		initAnchorPoints();
	}
	

	// Create a shape according to the sub-style.
	// Shift down and right - paint border dark color
	// Shift up and left - fill, then outline with border light
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

		// Calculate the inner area
		Rectangle ifb = new Rectangle();   // Interior, frame and border
		ifb = SwingUtilities.calculateInnerArea(this,ifb);
		// Now translate so that 0,0 is is at the inner origin
		g.translate(ifb.x, ifb.y);
		// Now translate so that 0,0 is inside the insets
		g.translate(INSET, INSET);

		int width  = ifb.width - 2*(INSET+BORDER_WIDTH);    // Actual width of shape
		int height =ifb.height - 2*(INSET+BORDER_WIDTH);    // Actual height of shape
		
		Shape shape = null;
		if( substyle.equalsIgnoreCase(SUBSTYLE_NOT)) {
			shape = computeNotShape(width,height);
		}
		else if( substyle.equalsIgnoreCase(SUBSTYLE_OR)) {
			shape = computeOrShape(width,height);
		}
		// Default is AND
		else {
			shape = computeAndShape(width,height);
		}

		// Fill dark shadow, one border-width down and to right.
		g.translate(BORDER_WIDTH,BORDER_WIDTH);

		g.setColor(BORDER_LIGHT_COLOR);
		g.fill(shape);

		// Re-adjust to the actual space
		g.translate(-BORDER_WIDTH,-BORDER_WIDTH);

		g.setColor(new Color(block.getBackground()));
		g.fill(shape);
		// Outline the block
		Stroke stroke = new BasicStroke(OUTLINE_WIDTH,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		g.setPaint(BORDER_DARK_COLOR);
		g.draw(shape);

		
		// Reverse any transforms we made
		g.setTransform(originalTx);
		g.setBackground(originalBackground);
		
		drawAnchors(g,-BORDER_WIDTH,-BORDER_WIDTH);
		drawEmbeddedIcon(g);
		int xoffset = -BORDER_WIDTH;
		if( substyle.equals(SUBSTYLE_NOT) ) xoffset -= width/5;
		drawEmbeddedText(g,xoffset,-BORDER_WIDTH);
		drawBadges(g);
	}

	//  ===================================== Calculate the shapes ===================================
	/**
	 * The "and" shape is a square with the right side bulged out into a 1/2 circle.
	 * @param width
	 * @param height
	 * @return
	 */
	private Shape computeAndShape(int width,int height) {
		final double STRAIGHT_FRACTION = 0.6;
		GeneralPath shape = new GeneralPath();
		shape.moveTo(0, 0);
		shape.lineTo(STRAIGHT_FRACTION*width,0);
		shape.quadTo(1.6*width,height/2,STRAIGHT_FRACTION*width, height);
		shape.lineTo(0, height);
		shape.closePath();
		return shape;
	}
	/**
	 * The "not" shape is a triangle with a small circle at its point.
	 * @param width
	 * @param height
	 * @return
	 */
	private Shape computeNotShape(int width,int height) {
		final double TRIANGLE_FRACTION = 0.8;
		GeneralPath triangle = new GeneralPath();
		triangle.moveTo(0, 0);
		triangle.lineTo(TRIANGLE_FRACTION*width,height/2.);
		triangle.lineTo(0, height);
		triangle.closePath();
		
		double diameter = width*(1-TRIANGLE_FRACTION);
		Ellipse2D.Double circle = new Ellipse2D.Double(width*TRIANGLE_FRACTION,(height-diameter)/2,diameter,diameter);
		
		Area area = new Area(triangle);
		area.add(new Area(circle));
		return area;
	}

	/**
	 * The "or" shape like the "and", except that the left side is indented.
	 * @param width
	 * @param height
	 * @return
	 */
	private Shape computeOrShape(int width,int height) {
		final double INDENT_FRACTION = 0.2;
		final double STRAIGHT_FRACTION = 0.6;
		GeneralPath shape = new GeneralPath();
		shape.moveTo(0, 0);
		shape.lineTo(STRAIGHT_FRACTION*width,0);
		shape.quadTo(1.6*width,height/2,STRAIGHT_FRACTION*width, height);
		shape.lineTo(0, height);
		shape.quadTo(INDENT_FRACTION*width,height/2,0, 0);
		shape.closePath();
		return shape;
	}

}
