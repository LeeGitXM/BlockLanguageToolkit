package com.ils.blt.designer.workspace.ui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;

import javax.swing.SwingUtilities;

import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;


/**
 * Create a block that depicts input or output from a tag. This is basically a 
 * square block with an arrow. The arrow colors are preset and depend on whether
 * this is a reader or a writer.
 */
public class ArrowUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 6644400470545202522L;
	private static final int DEFAULT_HEIGHT = 60;
	private static final int DEFAULT_WIDTH  = 80;
	private final static double STEM_WIDTH = 0.6;   // Fraction of width for stem of arrow
	private final static double STEM_HEIGHT = 0.5;  // Fraction of height

	
	public ArrowUIView(ProcessBlockView view) {
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		setOpaque(true);
		initAnchorPoints();
		
	}
	
	/**
	 *  The arrow allows only a single input and/or a single output. Place them
	 *  on the left and right, respectively.
	 */
	@Override
	protected void initAnchorPoints() {
		Dimension sz = getPreferredSize();
		int inSegmentCount = 0;
		int inputIndex = 0;
		int outSegmentCount= 0;
		int outputIndex= 0;
		
		// Count inputs and outputs - just to be safe. There should be a max
		// of one each.
		for(ProcessAnchorDescriptor desc:block.getAnchors()) {
			if(desc.getType()==	AnchorType.Origin ) inSegmentCount++;
			else if(desc.getType()==AnchorType.Terminus ) outSegmentCount++;
		}
		outSegmentCount++;   // Now equals the number of segments on a side
		inSegmentCount++;
		int inset = INSET;   // Align with arrow without border
		int ht = sz.height - BORDER_WIDTH;   // Effective height without border
		
		for(ProcessAnchorDescriptor desc:block.getAnchors()) {
			// Left side terminus
			if( desc.getType()==AnchorType.Terminus  ) {
				outputIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Terminus,
						desc.getConnectionType(),
						new Point(inset,outputIndex*ht/outSegmentCount),
						new Point(-LEADER_LENGTH,outputIndex*ht/outSegmentCount),
						new Rectangle(0,outputIndex*ht/outSegmentCount-inset,2*inset,2*inset+3), // Hotspot
						desc.isMultiple(),
						desc.getAnnotation());   
				getAnchorPoints().add(ap);
				
			}
			// Right-side origin
			else if(desc.getType()==AnchorType.Origin ) {
				inputIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(sz.width-inset,inputIndex*ht/inSegmentCount),
						new Point(sz.width+LEADER_LENGTH,inputIndex*ht/inSegmentCount),
						new Rectangle(sz.width-2*inset,inputIndex*ht/inSegmentCount-inset,2*inset,2*inset+3),
						desc.isMultiple(),
						desc.getAnnotation());
				getAnchorPoints().add(ap);
			}
		}
	}

	// Paint a right-pointing arrow. 
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
		
		int width  = ifb.width - 2*(INSET+BORDER_WIDTH);    // Actual width of  arrow
		int height =ifb.height - 2*(INSET+BORDER_WIDTH);    // Actual height of arrow
		// Actual stem dimensions do not include border
		int stemHeight = (int)(height * STEM_HEIGHT);
		int stemWidth = (int)(width * STEM_WIDTH);

		// Fill light shadow, one border-width down.
		g.translate(0,BORDER_WIDTH);
		
		int[] xbordervertices = new int[] {0,                     0,                    stemWidth,            stemWidth,width,stemWidth,stemWidth,0 };
		int[] ybordervertices = new int[] {(height+stemHeight)/2,(height-stemHeight)/2,(height-stemHeight)/2,0,height/2,height,(height+stemHeight)/2,(height+stemHeight)/2};
		Polygon fi = new Polygon(xbordervertices,ybordervertices,8);
		g.setColor(BORDER_LIGHT_COLOR);
		g.fillPolygon(fi);
		
		// Now add a few spots of dark fill color
		int[] xdarkvertices = new int[] {0, BORDER_WIDTH, stemWidth+BORDER_WIDTH,stemWidth,0};
		int[] ydarkvertices = new int[] {(height+stemHeight)/2,(height+stemHeight)/2-BORDER_WIDTH,(height+stemHeight)/2-BORDER_WIDTH,(height+stemHeight)/2,(height+stemHeight)/2};
		Polygon dark = new Polygon(xdarkvertices,ydarkvertices,5);
		g.setColor(BORDER_DARK_COLOR);
		g.fillPolygon(dark);
		
		// Re-adjust to the actual space
		g.translate(BORDER_WIDTH,-BORDER_WIDTH);

		int rgb = block.getBackground();
		if( block.isDirty()  ) rgb -= BLOCK_DIRTY_SHADING;
		g.setColor(new Color(rgb));
		g.fillPolygon(fi);
		// Outline the arrow
		Stroke stroke = new BasicStroke(OUTLINE_WIDTH,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		g.setPaint(BORDER_DARK_COLOR);
		g.draw(fi);
		// Darken the lower right of the arrow
		stroke = new BasicStroke(2*OUTLINE_WIDTH,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		GeneralPath path = new GeneralPath();
		path.moveTo(stemWidth, height);
		path.lineTo(width,height/2);
		g.draw(path);
		
		// Reverse any transforms we made
		g.setTransform(originalTx);
		g.setBackground(originalBackground);
		drawAnchors(g,-BORDER_WIDTH/2,-BORDER_WIDTH/2);
		drawEmbeddedIcon(g);
		drawEmbeddedText(g,0,0);
		drawBadges(g);
	}


}
