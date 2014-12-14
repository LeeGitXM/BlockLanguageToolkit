package com.ils.blt.client.component.recmap;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RectangularShape;
import java.awt.geom.RoundRectangle2D;

import prefuse.Constants;
import prefuse.render.ImageFactory;
import prefuse.render.LabelRenderer;
import prefuse.util.ColorLib;
import prefuse.util.FontLib;
import prefuse.util.GraphicsLib;
import prefuse.util.StringLib;
import prefuse.visual.VisualItem;


/**
 * This is a label renderer that selects the table column to be rendered
 * based on the column type of the node. This is very specific to the
 * three column layout.
 */
public class ThreeColumnLabelRenderer extends LabelRenderer {

    protected ImageFactory m_images = null;
    protected String m_delim = "\n";
    
    protected String m_labelName = "label";
    protected String m_imageName = null;
    
    protected int m_xAlign = Constants.CENTER;
    protected int m_yAlign = Constants.CENTER;
    protected int m_hTextAlign = Constants.CENTER;
    protected int m_vTextAlign = Constants.CENTER;
    protected int m_hImageAlign = Constants.CENTER;
    protected int m_vImageAlign = Constants.CENTER;
    protected int m_imagePos = Constants.LEFT;
    
    protected int m_horizBorder = 2;
    protected int m_vertBorder  = 0;
    protected int m_imageMargin = 2;
    protected int m_arcWidth    = 0;
    protected int m_arcHeight   = 0;

    protected int m_maxTextWidth = -1;
    
    /** Transform used to scale and position images */
    AffineTransform m_transform = new AffineTransform();
    
    /** The holder for the currently computed bounding box */
    protected RectangularShape m_bbox  = new Rectangle2D.Double();
    protected Point2D m_pt = new Point2D.Double(); // temp point
    protected Font    m_font; // temp font holder
    protected String    m_text; // label text
    protected Dimension m_textDim = new Dimension(); // text width / height

    /**
     * Create a new LabelRenderer. The column "KIND"
     * determines which of "NAME" or "VALUE" to display.
     */
    public ThreeColumnLabelRenderer() {
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
        	if( kind==RecMapConstants.LINK_KIND) {
        		s = item.getString(RecMapConstants.VALUE); 
        	}
        	else {
        		s = item.getString(RecMapConstants.NAME); 
        	}
            return s;           
        }
        return s;
    }

    
    
} // end of class LabelRenderer
