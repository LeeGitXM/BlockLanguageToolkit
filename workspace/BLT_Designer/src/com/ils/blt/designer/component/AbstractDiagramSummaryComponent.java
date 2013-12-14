/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.component;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.Rectangle2D;

import javax.swing.border.BevelBorder;
import javax.swing.border.Border;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.vision.api.client.components.model.AbstractVisionComponent;
/**
 * This is a superclass for our custom Vision components.
 */
public abstract class AbstractDiagramSummaryComponent extends AbstractVisionComponent {

	private static final long serialVersionUID = 6091339174784663157L;
	
	// Specify default block dimensions
	protected static final int DEFAULT_BLOCK_HEIGHT = 80;
	protected static final int DEFAULT_FRAME_HEIGHT  = 6;   
	protected static final int DEFAULT_FRAME_WIDTH   = 7;            // Width of the cyan border
	protected static final int DEFAULT_RECTANGLE_BLOCK_WIDTH = 140;  // Was 120
	protected static final int DEFAULT_SQUARE_BLOCK_WIDTH = 80;
	protected static final int DEFAULT_BAR_HEIGHT = 15;              // Solid area for parallel, join
	
	protected static final int DEFAULT_ROUNDING_ARC_HEIGHT = 5;
	protected static final int DEFAULT_ROUNDING_ARC_WIDTH = 5;
	
	protected static final Color ENTRY_BAR_COLOR = new Color(30,85,225);   // For parallel
	protected static final Color EXIT_BAR_COLOR  = new Color(220,0,0);   // For join
	protected static final Color BORDER_HIGHLIGHT_COLOR = new Color(250,250,250);
	protected static final Color BORDER_SHADOW_COLOR = new Color(10,10,10);
	protected static final int   BORDER_WIDTH = 4;       // For components where we draw the border
	protected static int         STUB_HEIGHT  = 8;       // Connection stub height
	protected static int         STUB_WIDTH   = 4;       // Connection stub height
	
	 /** The background color is the filler color of the interior of most blocks. */
	protected static final Color DEFAULT_BACKGROUND_COLOR = new Color(250,250,250);

	 /** The foreground color is the filler color of the border of most blocks. */
	protected static final Color DEFAULT_FRAME_COLOR = Color.CYAN;
	protected static final Font DEFAULT_HEADING_FONT = new Font("Dialog", Font.PLAIN, 24); 
	protected static final Font DEFAULT_SUBHEADING_FONT = new Font("Dialog", Font.PLAIN, 18); 
	
	protected final LoggerEx log;

	
	
	protected Color frameColor = DEFAULT_FRAME_COLOR;
	protected String heading = "";
	protected String subheading = "";
	protected Border border = null;
	
	/**
	 * Normal form of the constructor.
	 */
	public AbstractDiagramSummaryComponent() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		setOpaque(false);

		
		// For Rectangular blocks,this is the border.
		border = new BevelBorder(BevelBorder.RAISED,BORDER_HIGHLIGHT_COLOR,BORDER_SHADOW_COLOR);
		
		setPreferredSize(new Dimension(
				  (isSquare()?DEFAULT_SQUARE_BLOCK_WIDTH:DEFAULT_RECTANGLE_BLOCK_WIDTH),
				  DEFAULT_BLOCK_HEIGHT));
		setBackground(DEFAULT_BACKGROUND_COLOR);
		setForeground(DEFAULT_FRAME_COLOR);
		setFont(DEFAULT_HEADING_FONT);
	}

	
	public String getHeading() { return heading; }
	public Color getHeadingColor() { return Color.BLACK; }
	public String getSubHeading() { return subheading; }
	public Color  getSubHeadingColor() { return Color.darkGray; }
	
	/**
	 * There are two general block styles: rectangular and square.
	 * @return whether or not to use the general dimensions for a square block.
	 */
	abstract public boolean isSquare();
	
	public void setHeading(String text) { heading=text; }
	public void setSubHeading(String text) { subheading=text; }
	
	protected void paintTextAt(Graphics2D g, String text, float xpos, float ypos, Color fill) {
		Font font = g.getFont();
		FontRenderContext frc = g.getFontRenderContext();
		GlyphVector vector = font.createGlyphVector(frc, text);
		Rectangle2D bounds = vector.getVisualBounds();
		// ypos is the center of the font. Adjust up by 1/2
		ypos+= bounds.getHeight()/2f;

		Shape textShape = vector.getOutline(xpos, ypos);
		g.setColor(fill);
		g.fill(textShape);
	}
}
