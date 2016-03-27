/**
 *   (c) 2016 ILS Automation. All rights reserved. 
 */
package com.ils.blt.client.component.recmap;

import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.RoundRectangle2D;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

import prefuse.Constants;
import prefuse.render.LabelRenderer;
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
	private final LoggerEx log;
	private final Map<Integer,TextDelegate> delegates;
	private final RecMapDataModel model;
	private String m_header = "";
	
    /**
     */
    public TableLabelRenderer(RecMapDataModel mdl) {
    	this.log = LogUtil.getLogger(getClass().getPackage().getName());
    	this.delegates = new HashMap<>();
    	this.model = mdl;
    	m_imageMargin = 2;
    	m_horizBorder = 2;
    	m_vertBorder = 2;
    }
    
    public void setDelegate(int kind,TextDelegate delegate) {
		delegates.put(new Integer(kind),delegate);
	}
	
    /**
     * @see prefuse.render.Renderer#render(java.awt.Graphics2D, prefuse.visual.VisualItem)
     */
    @Override
    public void render(Graphics2D g, VisualItem item) {
    	log.infof("%s.render ....",CLSS);
		if( item instanceof TableNodeItem ) {
			int kind = item.getInt(RecMapConstants.KIND);
			int row = item.getInt(RecMapConstants.ROW);
			TextDelegate delegate = delegates.get(new Integer(kind));
			if( delegate!=null) {
				Properties properties = model.getAttributes(row);
				Shape shape = getShape(item,delegate,properties);
		        if (shape != null)
		            drawShape(g, item, shape);
			}
		}
        
    }
    
    /**
     * Returns the shape describing the boundary of an item. The shape's
     * coordinates should be in absolute (item-space) coordinates.
     * Similar to a call with the same name inAbstractShapeRenderer, except
     * that we pass along the delegate and properties.
     * @param item the item for which to get the Shape
     */
    private Shape getShape(VisualItem item,TextDelegate delegate,Properties properties) {
        AffineTransform at = getTransform(item);
        Shape rawShape = getRawShape(item,delegate,properties);
        return (at==null || rawShape==null ? rawShape : at.createTransformedShape(rawShape));
    }
    
    /**
     * Similar to a call with the same name in LabelRenderer, except that we 
     * have a header string instead of an image. The code is shamelessly 
     * plagiarized from LabelRenderer.
     * @see prefuse.render.LabelRenderer#getRawShape(prefuse.visual.VisualItem)
     */
    private Shape getRawShape(VisualItem item,TextDelegate delegate,Properties properties) {
        m_header = delegate.getHeaderText(item, properties);
        m_text = delegate.getBodyText(item, properties);
        double size = item.getSize();
        
        // get header dimensions
        double iw=0, ih=0;
        if ( m_header != null ) {
        	m_header = computeTextDimensions(item, m_header, size);
            ih = m_textDim.height;
            iw = m_textDim.width;     
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
        w = Math.max(tw, size*iw) + size*2*m_horizBorder;
        h = th + size*(ih + 2*m_vertBorder + (th>0 && ih>0 ? m_imageMargin : 0));
      
        // get the top-left point, using the current alignment settings
        getAlignedPoint(m_pt, item, w, h, m_xAlign, m_yAlign);
        
        if ( m_bbox instanceof RoundRectangle2D ) {
            RoundRectangle2D rr = (RoundRectangle2D)m_bbox;
            rr.setRoundRect(m_pt.getX(), m_pt.getY(), w, h,
                            size*m_arcWidth, size*m_arcHeight);
        } else {
            m_bbox.setFrame(m_pt.getX(), m_pt.getY(), w, h);
        }
        return m_bbox;
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
     * Returns the text to draw. Subclasses can override this class to
     * perform custom text selection.
     * @param item the item to represent as a <code>String</code>
     * @return a <code>String</code> to draw
     */
    protected String getText(VisualItem item) {
        String s = null;
        if ( item.canGetString(RecMapConstants.KIND) ) {
        	int kind = item.getInt(RecMapConstants.KIND);
        	// If this is the "link", then use the VALUE
        	s = item.getString(RecMapConstants.NAME); 
            return s;           
        }
        return s;
    }
    
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

} 
