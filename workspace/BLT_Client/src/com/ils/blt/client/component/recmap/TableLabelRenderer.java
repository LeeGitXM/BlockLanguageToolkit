/**
 *   (c) 2016 ILS Automation. All rights reserved. 
 */
package com.ils.blt.client.component.recmap;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RectangularShape;
import java.awt.geom.RoundRectangle2D;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import prefuse.Constants;
import prefuse.render.LabelRenderer;
import prefuse.util.ColorLib;
import prefuse.util.FontLib;
import prefuse.util.GraphicsLib;
import prefuse.util.StringLib;
import prefuse.visual.VisualItem;
import prefuse.visual.tuple.TableNodeItem;


/**
 * A table-label is a multi-line label which displays text in separate
 * rows. The top-most row, the header, is shaded.
 * 
 * The value in the top row is the name of the block. Subsequent rows 
 * are populated from a list of name-value pairs, the attributes.
 */
public class TableLabelRenderer extends LabelRenderer {
	private static final String CLSS = "TableLabelRenderer";
	private final static Color BACKGROUND = new Color(250,250,250);   // Cream
	private final LoggerEx log;
	private final Map<Integer,TextDelegate> delegates;
	private final RecMapDataModel model;
	private String m_header = "";
	private double itemHeight = 1.;
	private double itemWidth = 1.;
	/** Transform used to scale and position header (not visible from base class) */
    private AffineTransform m_headertransform = new AffineTransform();
	
    /**
     */
    public TableLabelRenderer(RecMapDataModel mdl) {
    	this.log = LogUtil.getLogger(getClass().getPackage().getName());
    	this.delegates = new HashMap<>();
    	this.model = mdl;
    	m_imageMargin = 2;
    	m_horizBorder = 2;
    	m_vertBorder = 2;
    	setRenderType(RENDER_TYPE_DRAW_AND_FILL);
    	setManageBounds(true);   // False doesn't work
    }
    
    public void setDelegate(int kind,TextDelegate delegate) {
		delegates.put(new Integer(kind),delegate);
	}
	
    /**
     * @see prefuse.render.Renderer#render(java.awt.Graphics2D, prefuse.visual.VisualItem)
     */
    @Override
    public void render(Graphics2D g, VisualItem item) {
    	//log.infof("%s.render ....",CLSS);
    	TextDelegate delegate = delegateFromItem(item);
        if( delegate!=null ) {
        	Properties properties = propertiesFromItem(item);
        	RectangularShape shape = (RectangularShape)getShape(item);
		    if (shape != null) {
		         // fill the header shape (1/2 height), if requested
		         int type = getRenderType(item);
		         if ( type==RENDER_TYPE_FILL || type==RENDER_TYPE_DRAW_AND_FILL ) {
		        	 // First paint the entire block ivory
		        	 int saveColor = item.getFillColor();
		        	 item.setFillColor(BACKGROUND.getRGB());
		        	 GraphicsLib.paint(g, item, shape, getStroke(item), RENDER_TYPE_DRAW_AND_FILL);
		        	 
		        	 item.setFillColor(saveColor);
		        	 RectangularShape halfhigh = new Rectangle2D.Double(shape.getMinX(),
		        			 					shape.getMinY(),
		        			 					shape.getWidth(),shape.getHeight()/2.);
		             GraphicsLib.paint(g, item, halfhigh, getStroke(item), RENDER_TYPE_DRAW_AND_FILL);
		         }
		         

		         boolean useInt = 1.5 > Math.max(g.getTransform().getScaleX(),
		                                         g.getTransform().getScaleY());
		         
		         double x = shape.getMinX() + m_horizBorder;
		         double y = shape.getMinY() + m_vertBorder;
		  
		         // First render the header. Header is 1/2 the height, centered
		         String text = delegate.getHeaderText(item, properties);
		         m_font = item.getFont();
		         m_font = scaleFontForText(m_font,text,itemWidth,itemHeight/2);
		         m_hTextAlign = Constants.CENTER;
		         int textColor = item.getTextColor();
		         if ( text != null && ColorLib.alpha(textColor) > 0 ) {
		             g.setPaint(ColorLib.getColor(textColor));
		             g.setFont(m_font);
		             FontMetrics fm = DEFAULT_GRAPHICS.getFontMetrics(m_font);

		             // compute available width
		             double hw = m_textDim.width;
		             if( hw>itemWidth) hw = itemWidth;
		             
		             // compute available height
		             double hh = m_textDim.height;
		             if( hh>itemHeight) hh = itemHeight;
		             
		             // compute starting y-coordinate - align to center of top 1/2
		             y += hh/2;
		             
		             // render  text - center horizontally
		             int lh = fm.getHeight(); // the line height

		             drawString(g, fm, text, useInt, x, y, hw);
		         }
		     

			
		         // render body text - we're counting on one line, but this has code to handle multiple.
		         text = delegate.getBodyText(item, properties);
		         m_hTextAlign = Constants.LEFT;
		         if ( text != null && ColorLib.alpha(textColor) > 0 ) {
		             g.setPaint(ColorLib.getColor(textColor));
		             g.setFont(m_font);
		             FontMetrics fm = DEFAULT_GRAPHICS.getFontMetrics(m_font);

		             // compute available width
		             double tw = m_textDim.width;
		             if( tw>itemWidth) tw = itemWidth;
		             
		             // compute available height
		             double th = m_textDim.height;
		             if( th>itemHeight) th = itemHeight;
		             
		             // compute starting y-coordinate - align to center of bottom 1/2
		             y = shape.getMinY() + m_vertBorder;
		             y += shape.getHeight()/2;    // Now at midpoint
		             y += th/2;
		             
		             // render each line of text - left align
		             int lh = fm.getHeight(); // the line height

		             drawString(g, fm, text, useInt, x, y, tw);
		         }
			}
		}
    }
    

    /**
     * The parent method returns a variable size that depends on the lenght of the text.
     * This version returns a fixed size box and then relies on modification of the
     * font to fit the text into the box.
     */
    @Override
    protected Shape getRawShape(VisualItem item) {
        
        // use bounding box dimensions. 
		double w=itemWidth  + 2*m_horizBorder; 
        double h=itemHeight + 2*m_vertBorder;
        
        // get the top-left point, using the current alignment settings
        getAlignedPoint(m_pt, item, w, h, m_xAlign, m_yAlign);
        
        if ( m_bbox instanceof RoundRectangle2D ) {
            RoundRectangle2D rr = (RoundRectangle2D)m_bbox;
            rr.setRoundRect(m_pt.getX(), m_pt.getY(), w, h,
                            m_arcWidth, m_arcHeight);
        } 
        else {
            m_bbox.setFrame(m_pt.getX(), m_pt.getY(), w, h);
        }
        return (RectangularShape)m_bbox;
    }
    
    /**
     * Draws the specified shape into the provided Graphics context, using
     * stroke and fill color values from the specified VisualItem. This method
     * can be called by subclasses in custom rendering routines. 
     */
    @Override
    protected void drawShape(Graphics2D g, VisualItem item, Shape shape) {
        GraphicsLib.paint(g, item, shape, getStroke(item), getRenderType(item));
    }
    
    

    /**
     * We should not be using this method. Throw exception
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    protected String getText(VisualItem item) {
        throw new IllegalArgumentException("getText not applicable to this class");
    }

    public void setMaximumHeight(double h) { this.itemHeight = h; }
    public void setMaximumWidth(double w) { this.itemWidth = w; }
    
    /**
     * @return a font (no smaller than 6) that will fit the text in the area allotted.
     *         This function only downsizes the font. The original setting should
     *         define the largest size desired.
     */
    private Font scaleFontForText(Font font,String text,double w,double h) {
        // Get the metrics for the default font size
    	FontMetrics fm = DEFAULT_GRAPHICS.getFontMetrics(font);
    	int stringWidth = fm.stringWidth(text);
    	int stringHeight= fm.getHeight();
    	
    	double ratio = w/stringWidth;
    	if( ratio > h/stringHeight ) ratio = h/stringHeight;
        // scale the font if needed
        if ( ratio < 1 ) {
        	int fontSize = (int)(ratio*font.getSize());
        	if( fontSize<6 ) fontSize = 6;
            font = FontLib.getFont(font.getName(), font.getStyle(),fontSize);
        }
        return font;
    }
    
    private TextDelegate delegateFromItem(VisualItem item) {
    	TextDelegate delegate = null;
    	if( item instanceof TableNodeItem ) {
    		int kind = item.getInt(RecMapConstants.KIND);
    		delegate = delegates.get(new Integer(kind));
    	}
    	return delegate;
    }
    
    // Stolen directly from LabelRenderer (it was private final)
    private void drawString(Graphics2D g, FontMetrics fm, String text,boolean useInt, double x, double y, double w)  {
        // compute the x-coordinate
        double tx;
        switch ( m_hTextAlign ) {
        case Constants.LEFT:
            tx = x;
            break;
        case Constants.RIGHT:
            tx = x + w - fm.stringWidth(text);
            break;
        case Constants.CENTER:
            tx = x + (w - fm.stringWidth(text)) / 2;
            break;
        default:
            throw new IllegalStateException(
                    "Unrecognized text alignment setting.");
        }
        // use integer precision unless zoomed-in
        // results in more stable drawing
        if ( useInt ) {
            g.drawString(text, (int)tx, (int)y);
        } else {
            g.drawString(text, (float)tx, (float)y);
        }
    }
    private Properties propertiesFromItem(VisualItem item) {
    	Properties properties = null;
    	if( item instanceof TableNodeItem ) {
    		int row = item.getInt(RecMapConstants.ROW);
    		properties = model.getAttributes(row);
    	}
    	return properties;
    }
    			
} 
