package com.ils.blt.designer.workspace.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.Rectangle2D;

import com.ils.blt.designer.workspace.BlockAttributeView;
import com.ils.blt.designer.workspace.ProcessBlockView;


/**
 * Create a rectangular display similar to a "readout", or a G2 attribute-display. The
 * subject BlockAttributeView is listening on a specified property of a different block.
 * The view has a large number of properties related to formatting of the output.
 * 
 * There are no anchor points.
 */
public class AttributeUIView extends AbstractBlockUIView implements BlockViewUI {
	private static final long serialVersionUID = 2160868310475735865L; 
	private static final int DEFAULT_HEIGHT = 40;
	private static final int DEFAULT_WIDTH = 80;
	private BlockAttributeView bav = null;
	
	
	// Once we have the view, get the block that is being viewed
	public AttributeUIView(ProcessBlockView view) {
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		if( view instanceof BlockAttributeView ) {
			bav = (BlockAttributeView)view;
		}
		setOpaque(false);
	}
	
	@Override
	protected void paintComponent(Graphics _g) {
		// Calling the super method effects an "erase".
		Graphics2D g = (Graphics2D) _g;
		//log.infof("AttributeUIView.paintComponent %s ...(%d:%s)",getBlock().getName(),valueProperty.hashCode(),fncs.coerceToString(valueProperty.getValue()) );
		// Preserve the original transform to roll back to at the end
		Color background = colorForString(bav.getBackgroundColor());
		if( !bav.getBackgroundColor().equalsIgnoreCase("TRANSPARENT") ) {
			float[] cc = background.getComponents(null);
			background = new Color(cc[0],cc[1],cc[2],0.5f);
		}
		Color foreground = colorForString(bav.getForegroundColor());

		// Turn on anti-aliasing
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
		g.setPaint(foreground);
		
 
		int height = bav.getPreferredHeight();
		int width = bav.getPreferredWidth();
		g.setColor(background);
		g.fillRect(0, 0, width,height);
		
		String value = bav.getValue();
		int fontSize = bav.getFontSize(); 
		
		block.setEmbeddedFontSize(fontSize);
		block.setEmbeddedLabel(value);
		drawEmbeddedText(g,0,0,foreground);
	}
	
	// Draw the text that is part of the rendered box. Recognize \n or \\n as newlines.
	// Left adjust
	protected void drawEmbeddedText(Graphics2D g,int offsetx,int offsety,Color fill) {
		String text = block.getEmbeddedLabel();
		if( text == null || text.length()==0 ) return;
		Dimension sz = getPreferredSize();
		String[] lines = text.split("\n");
		if( lines.length==1 ) lines = text.split("\\n");
		int lineCount = lines.length;
		int dy = 3*block.getEmbeddedFontSize()/4;
		int y = sz.height/2 - (lineCount-1)*dy/2;
		for( String line: lines) {
			paintTextAt(g,line,offsetx,y+offsety,fill,block.getEmbeddedFontSize());
			y+=dy;
		}
	}
	/**
	 * Utility method to paint a text string - left aligned.
	 * @param g
	 * @param text
	 * @param xpos center of the text
	 * @param ypos center of the text
	 * @param fill color of the text
	 */
	protected void paintTextAt(Graphics2D g, String text, float xpos, float ypos, Color fill,int fontSize) {
		Font font = g.getFont();
		font = font.deriveFont((float)fontSize);  // This is, presumably the correct way
		FontRenderContext frc = g.getFontRenderContext();
		GlyphVector vector = font.createGlyphVector(frc, text);
		Rectangle2D bounds = vector.getVisualBounds();
		// xpos, ypos are centers. Adjust to upper left.
		ypos+= bounds.getHeight()/2f;
		//xpos-= bounds.getWidth()/2f;

		Shape textShape = vector.getOutline(xpos, ypos);
		g.setColor(fill);
		g.fill(textShape);
	}
	
	private Color colorForString(String clr) {
		Color color = new Color(255,255,255,0); // TRANSPARENT
		if(clr.equalsIgnoreCase("BLACK")) 		color = Color.BLACK;
		else if(clr.equalsIgnoreCase("RED")) 	color = Color.RED;
		else if(clr.equalsIgnoreCase("GREEN")) 	color = Color.GREEN;
		else if(clr.equalsIgnoreCase("BLUE"))	color = Color.BLUE;
		else if(clr.equalsIgnoreCase("WHITE"))	color = Color.WHITE;
		else if(clr.equalsIgnoreCase("YELLOW"))	color = Color.YELLOW;
		else if(clr.equalsIgnoreCase("GRAY"))	color = Color.GRAY;
		else if(clr.equalsIgnoreCase("LIGHT_GRAY"))	color = Color.LIGHT_GRAY;
		else if(clr.equalsIgnoreCase("DARK_GRAY"))	color = Color.DARK_GRAY;
		else if(clr.equalsIgnoreCase("ORANGE"))	color = Color.ORANGE;
		else if(clr.equalsIgnoreCase("MAGENTA"))color = Color.MAGENTA;
		else if(clr.equalsIgnoreCase("PINK"))	color = Color.PINK;
		else if(clr.equalsIgnoreCase("CYAN"))	color = Color.CYAN;
		return color;
	}
}
