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
	private double m_maxheight = 1.;
	private double m_maxwidth = 1.;
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
        	RectangularShape shape = getShape(item,delegate,properties);
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
		         String text = m_header;
		         m_hTextAlign = Constants.CENTER;
		         int textColor = item.getTextColor();
		         if ( text != null && ColorLib.alpha(textColor) > 0 ) {
		             g.setPaint(ColorLib.getColor(textColor));
		             g.setFont(m_font);
		             FontMetrics fm = DEFAULT_GRAPHICS.getFontMetrics(m_font);

		             // compute available width
		             double hw = m_textDim.width;
		             if( hw>m_maxwidth) hw = m_maxwidth;
		             
		             // compute available height
		             double hh = m_textDim.height;
		             if( hh>m_maxheight) hh = m_maxheight;
		             
		             // compute starting y-coordinate - align to center of top 1/2
		             y += hh/2;
		             
		             // render each line of text - center horizontally
		             int lh = fm.getHeight(); // the line height
		             int start = 0, end = text.indexOf(m_delim);
		             for ( ; end >= 0; y += lh ) {
		                 drawString(g, fm, text.substring(start, end), useInt, x, y, hw);
		                 start = end+1;
		                 end = text.indexOf(m_delim, start);   
		             }
		             drawString(g, fm, text.substring(start), useInt, x, y, hw);
		         }
		     

			
		         // render body text - we're counting on one line, but this has code to handle multiple.
		         text = m_text;
		         m_hTextAlign = Constants.LEFT;
		         if ( text != null && ColorLib.alpha(textColor) > 0 ) {
		             g.setPaint(ColorLib.getColor(textColor));
		             g.setFont(m_font);
		             FontMetrics fm = DEFAULT_GRAPHICS.getFontMetrics(m_font);

		             // compute available width
		             double tw = m_textDim.width;
		             if( tw>m_maxwidth) tw = m_maxwidth;
		             
		             // compute available height
		             double th = m_textDim.height;
		             if( th>m_maxheight) th = m_maxheight;
		             
		             // compute starting y-coordinate - align to center of bottom 1/2
		             y = shape.getMinY() + m_vertBorder;
		             y += shape.getHeight()/2;    // Now at midpoint
		             y += th/2;
		             
		             // render each line of text - left align
		             int lh = fm.getHeight(); // the line height
		             int start = 0, end = text.indexOf(m_delim);
		             for ( ; end >= 0; y += lh ) {
		                 drawString(g, fm, text.substring(start, end), useInt, x, y, tw);
		                 start = end+1;
		                 end = text.indexOf(m_delim, start);   
		             }
		             drawString(g, fm, text.substring(start), useInt, x, y, tw);
		         }
			}
		}
    }
    
    /**
     * Handle the case where this is called internally.
     */
    @Override
    public Shape getShape(VisualItem item) {
    	log.infof("%s.getShape ....",CLSS);
    	Shape shape = null;
    	TextDelegate delegate = delegateFromItem(item);
        if( delegate!=null ) {
        	Properties properties = propertiesFromItem(item);
        	shape = getShape(item,delegate,properties);
		}
		return shape;
    }
    
    /**
     * Returns the shape describing the boundary of an item. The shape's
     * coordinates should be in absolute (item-space) coordinates.
     * Similar to a call with the same name inAbstractShapeRenderer, except
     * that we pass along the delegate and properties.
     * @param item the item for which to get the Shape
     */
    private RectangularShape getShape(VisualItem item,TextDelegate delegate,Properties properties) {
        AffineTransform at = getTransform(item);
        RectangularShape rawShape = getRawShape(item,delegate,properties);
        return (at==null || rawShape==null ? rawShape : (RectangularShape)at.createTransformedShape(rawShape));
    }

    /**
     * Similar to a call with the same name in LabelRenderer, except that we 
     * have a header string instead of an image. The code is shamelessly 
     * plagiarized from LabelRenderer.
     * @see prefuse.render.LabelRenderer#getRawShape(prefuse.visual.VisualItem)
     */
    private RectangularShape getRawShape(VisualItem item,TextDelegate delegate,Properties properties) {
        m_header = delegate.getHeaderText(item, properties);
        m_text = delegate.getBodyText(item, properties);
        double size = item.getSize();
        
        // get header dimensions
        double hw=0, hh=0;
        if ( m_header != null ) {
        	m_header = computeTextDimensions(item, m_header, size);
            hh = m_textDim.height;
            hw = m_textDim.width;     
        }
        
        // get text dimensions
        int tw=0, th=0;
        if ( m_text != null ) {
            m_text = computeTextDimensions(item, m_text, size);
            th = m_textDim.height;
            tw = m_textDim.width;   
        }
        
        // get bounding box dimensions. The header is always on top.
        double w=0, h=0;
        w = Math.max(tw, size*hw) + size*2*m_horizBorder;
        h = th + size*(hh + 2*m_vertBorder + (th>0 && hh>0 ? m_imageMargin : 0));
      
        // get the top-left point, using the current alignment settings
        getAlignedPoint(m_pt, item, w, h, m_xAlign, m_yAlign);
        
        if ( m_bbox instanceof RoundRectangle2D ) {
            RoundRectangle2D rr = (RoundRectangle2D)m_bbox;
            rr.setRoundRect(m_pt.getX(), m_pt.getY(), w, h,
                            size*m_arcWidth, size*m_arcHeight);
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
    /**
     * @see prefuse.render.Renderer#setBounds(prefuse.visual.VisualItem)
     */
    @Override
    public void setBounds(VisualItem item) {
        if ( !m_manageBounds ) return;   // Things don't go well
        TextDelegate delegate = delegateFromItem(item);
        if( delegate!=null ) {
        	Properties properties = propertiesFromItem(item);
            Shape shape = getShape(item,delegate,properties);
            if ( shape == null ) {
                item.setBounds(item.getX(), item.getY(), 0, 0);
            } 
            else {
                GraphicsLib.setBounds(item, shape, getStroke(item));
            }
        }
    }
    public void setMaximumHeight(double h) { this.m_maxheight = h; }
    public void setMaximumWidth(double w) { this.m_maxwidth = w; }
    
    // Stolen from LabelRenderer where it is a private method.
    // This potential shortens the text. As a side effect, it sets 
    // class members that hold dimensions and font sizes. 
    private String computeTextDimensions(VisualItem item, String text,double size) {
        // put item font in temp member variable
        m_font = item.getFont();
        // scale the font as needed
        if ( size != 1 ) {
            m_font = FontLib.getFont(m_font.getName(), m_font.getStyle(),
                                     size*m_font.getSize());
        }
        
        FontMetrics fm = DEFAULT_GRAPHICS.getFontMetrics(m_font);
        StringBuffer str = null;
        
        // compute the number of lines and the maximum width
        int nlines = 1, w = 0, start = 0, end = text.indexOf(m_delim);
        m_textDim.width = 0;
        String line;
        for ( ; end >= 0; ++nlines ) {
            w = fm.stringWidth(line=text.substring(start,end));
            // abbreviate line as needed
            if ( m_maxTextWidth > -1 && w > m_maxTextWidth ) {
                if ( str == null )
                    str = new StringBuffer(text.substring(0,start));
                str.append(StringLib.abbreviate(line, fm, m_maxTextWidth));
                str.append(m_delim);
                w = m_maxTextWidth;
            } else if ( str != null ) {
                str.append(line).append(m_delim);
            }
            // update maximum width and substring indices
            m_textDim.width = Math.max(m_textDim.width, w);
            start = end+1;
            end = text.indexOf(m_delim, start);
        }
        w = fm.stringWidth(line=text.substring(start));
        // abbreviate line as needed
        if ( m_maxTextWidth > -1 && w > m_maxTextWidth ) {
            if ( str == null )
                str = new StringBuffer(text.substring(0,start));
            str.append(StringLib.abbreviate(line, fm, m_maxTextWidth));
            w = m_maxTextWidth;
        } else if ( str != null ) {
            str.append(line);
        }
        // update maximum width
        m_textDim.width = Math.max(m_textDim.width, w);
        
        // compute the text height
        m_textDim.height = fm.getHeight() * nlines;
        
        return str==null ? text : str.toString();
    }
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
