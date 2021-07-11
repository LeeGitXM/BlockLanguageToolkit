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

import javax.swing.SwingUtilities;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.ProcessBlockView;


/**
 * Create a rectangular display similar to a "readout". The block has a large number of properties
 * related to formatting of the output.
 * 
 * There are no anchor points.
 */
public class AttributeUIView extends AbstractBlockUIView implements BlockViewUI {
	private static final long serialVersionUID = 2160868310475735865L; 
	private final ProcessBlockView referenceBlock;
	private static final int DEFAULT_HEIGHT = 40;
	private static final int DEFAULT_WIDTH = 80;
	private final String propertyName;
	private int height;
	private int width ;
	private int offsetx;
	private int offsety;
	private final UtilityFunctions fncs;
	private BlockProperty valueProperty = null;  /// This is the watched property on the reference block
	
	public AttributeUIView(ProcessBlockView view) {
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		this.referenceBlock = null;
		this.propertyName = null;
		this.fncs = new UtilityFunctions();
		setOpaque(false);
		
		valueProperty = findValueProperty();
	}
	
	// The value property is the named property of the referenced block.
	private BlockProperty findValueProperty() {
		BlockProperty vp = null;
		for( BlockProperty bp:block.getProperties()) {
			if( bp.getName()!=null && bp.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_VALUE)) {
				vp = bp;
				//log.infof("ReadoutViewUI(%d): found value property",bp.hashCode());
			}
		}
		return vp;
	}
	
	
	@Override
	protected void paintComponent(Graphics _g) {
		// Calling the super method effects an "erase".
		Graphics2D g = (Graphics2D) _g;
		//log.infof("ReadoutUIView.paintComponent %s ...(%d:%s)",getBlock().getName(),valueProperty.hashCode(),fncs.coerceToString(valueProperty.getValue()) );
		// Preserve the original transform to roll back to at the end
		AffineTransform originalTx = g.getTransform();
		Color originalBackground = g.getBackground();

		// Turn on anti-aliasing
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
		// Setup for outlining
		float outlineWidth = 1.0f;
		Stroke stroke = new BasicStroke(outlineWidth,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		//g.setStroke(stroke);
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
		// Create a rectangle for the border that is within the insets. 
		// Use the upper left for light shading, the lower right for dark
		int[] xulvertices = new int[] {ifb.x,            ifb.x,ifb.x+ifb.width,ifb.x };
		int[] yulvertices = new int[] {ifb.y+ifb.height, ifb.y,ifb.y,ifb.y+ifb.height};
		Polygon fi = new Polygon(xulvertices,yulvertices,4);
		g.setColor(BORDER_LIGHT_COLOR);
		g.fillPolygon(fi);
		g.draw(fi);
		
		// This is a triangle (sort-of), the lower-right half. 
		int[] xlrvertices = new int[] {ifb.x,  ifb.x+BORDER_WIDTH,ifb.x+ifb.width-BORDER_WIDTH,           ifb.x+ifb.width,  ifb.x+ifb.width,ifb.x };
		int[] ylrvertices = new int[] {ifb.y+ifb.height,ifb.y+ifb.height-BORDER_WIDTH,ifb.y+BORDER_WIDTH, ifb.y,ifb.y+ifb.height,ifb.y+ifb.height};
		fi = new Polygon(xlrvertices,ylrvertices,6);
		g.setColor(BORDER_DARK_COLOR);
		g.fillPolygon(fi);
		g.draw(fi);

		ifb.x += BORDER_WIDTH;
		ifb.y += BORDER_WIDTH;
		ifb.width  -= 2*(BORDER_WIDTH);
		ifb.height -= 2*(BORDER_WIDTH);
		// Create a rectangle that is within the border boundaries
		int[] xvertices = new int[] {ifb.x, ifb.x+ifb.width,ifb.x+ifb.width,ifb.x };
		int[] yvertices = new int[] {ifb.y, ifb.y,ifb.y+ifb.height,ifb.y+ifb.height};
		fi = new Polygon(xvertices,yvertices,4);
		int rgb = block.getBackground();
		Color background = new Color(rgb);
		if(block.isDirty()) background = background.darker();
		g.setColor(background); 
		g.fillPolygon(fi);
		// Outline the inner square
		g.setPaint(INSET_COLOR);
		g.draw(fi);

		// Reverse any transforms we made
		g.setTransform(originalTx);
		g.setBackground(originalBackground);
		drawAnchors(g,0,-BORDER_WIDTH/2);
		// Update embedded text - the block formats the output
		// We are guaranteed that the property value is a qualified value.
		String value  = "no value property";
		if( valueProperty!=null ) {
			value = fncs.coerceToString(valueProperty.getValue());   // Just to be safe
		}
		
		// NOTE* No longer assume 100px width.  TimeReadout is wider.  The old setting of 8 for small was unreadable
		// Set the font size based on the string length.
		// Assumes 100px block width
		int fontSize = 12;  // Small
		if( value.length()<7 ) fontSize = 14;
		else if( value.length()<13 ) fontSize = 12;
		
		block.setEmbeddedFontSize(fontSize);
		block.setEmbeddedLabel(value);

		drawBadges(g);
		drawEmbeddedText(g,0,0);

	}
}
