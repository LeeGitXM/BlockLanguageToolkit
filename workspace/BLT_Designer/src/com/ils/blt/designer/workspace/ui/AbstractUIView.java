package com.ils.blt.designer.workspace.ui;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JComponent;

import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.WorkspaceConstants;
import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;


/**
 * This serves as a base class for our custom collection of block 2D renders.
 * The main service this class provides is to draw anchor points at specified
 * location and orientation.
 * 
 */
@SuppressWarnings("serial")
public abstract class AbstractUIView extends JComponent implements BlockViewUI {
	private static final String TAG = "AbstractUIView";
	protected final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	protected final ProcessBlockView block;
	private final List<AnchorPoint> anchorPoints;  // Entries are BasicAnchorPoint
	protected BlockComponent blockComponent = null;
	protected static int ANCHOR_ANNOTATION_TEXT_SIZE = 9;
	protected final static int BADGE_HEIGHT = 20;
	protected final static int BADGE_WIDTH = 20;
	protected final static int BORDER_WIDTH = 3;
	protected final static Color BORDER_DARK_COLOR = Color.darkGray;
	protected final static Color BORDER_LIGHT_COLOR = new Color(230,230,230); // Light gray
	protected final static Color INSET_COLOR = new Color(210,210,210);        // A little darker gray
	protected final static int INSET = 6;
	protected final static int LEADER_LENGTH = 10;
	protected final static int SIGNAL_LEADER_LENGTH = 8;        // Shorter for signals
	protected final static Color OUTLINE_COLOR = Color.BLACK;   // For stub
	protected final static float OUTLINE_WIDTH = 1.0f;          // For stub
	protected final static Color TEXT_COLOR = Color.BLACK;      // For embedded label
	
	/**
	 * Use default height and widths supplied by subclass when values not assigned by view.
	 * @param view
	 * @param defaultWidth
	 * @param defaultHeight
	 */
	public AbstractUIView(ProcessBlockView view,int defaultWidth,int defaultHeight) {
		this.block = view;
		setOpaque(false);
		int preferredHeight = view.getPreferredHeight();
		if( preferredHeight<=0 ) preferredHeight = defaultHeight;
		int preferredWidth = view.getPreferredWidth();
		if( preferredWidth<=0 ) preferredWidth = defaultWidth;
		setPreferredSize(new Dimension(preferredWidth,preferredHeight)); 
		anchorPoints = new ArrayList<AnchorPoint>();
	}

	/**
	 *  Create anchor points from the anchor descriptions. This default implementation
	 *  places at most one input on the left and one output on the right. It assumes the main UI has
	 *  insets of INSET on all 4 sides. The anchor "leader" should be at least 10 pixels outside the component.
	 *  The hotspot is twice the size of the visible square. 
	 *  
	 *  The anchor locations are:
	 *  1) Signal connections at 1/4, 3/4 along the top - input to the left
	 *  2) Other inputs centered along the left
	 *  3) Information output 3/4 of way along the bottom
	 *  4) Other outputs centered along the right. 
	 *  
	 *  A custom method is required for other patterns.
	 *  Note: This is NOT called from the constructor of the base class.
	 *        Call from the constructor of each sub-class.
	 *  Note: While we do support signal connections, the preferred method
	 *        is to enable signal receipt or transmission, then draw "badge"
	 *        markers to show so.
	 */
	protected void initAnchorPoints() {
		Dimension sz = getPreferredSize();
		int inputCount = 0;
		int inputIndex = 0;
		int outputCount= 0;
		int outputIndex= 0;
		
		// Count inputs and outputs
		for(ProcessAnchorDescriptor desc:block.getAnchors()) {
			if(desc.getConnectionType()==ConnectionType.SIGNAL) continue;
			if(desc.getConnectionType()==ConnectionType.TEXT &&
			   desc.getType()==	AnchorType.Terminus) continue;
			
			if(desc.getType()==	AnchorType.Origin ) inputCount++;
			else if(desc.getType()==AnchorType.Terminus ) outputCount++;
		}
		outputCount++;   // Now equals the number of segments on a side
		inputCount++;
		int inset = INSET-BORDER_WIDTH;
		
		for(ProcessAnchorDescriptor desc:block.getAnchors()) {
			// Top left signal
			if(desc.getConnectionType()==ConnectionType.SIGNAL && desc.getType()==AnchorType.Terminus ) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Terminus,
						desc.getConnectionType(),
						new Point(inset+(sz.width-2*inset)/4,inset+1),
						new Point(inset+(sz.width-2*inset)/4,-SIGNAL_LEADER_LENGTH),
						new Rectangle((sz.width-2*inset)/4,0,2*inset,2*inset),
						desc.getAnnotation());   // x,y,width,height. Hotspot shape.
				ap.setSide(AnchorSide.TOP);
				getAnchorPoints().add(ap);
			}
			// Top right signal
			else if(desc.getConnectionType()==ConnectionType.SIGNAL && desc.getType()==AnchorType.Origin ) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(inset+3*(sz.width-2*inset)/4,inset+1),
						new Point(inset+3*(sz.width-2*inset)/4,-LEADER_LENGTH),
						new Rectangle(3*(sz.width-2*inset)/4,0,2*inset,2*inset),
						desc.getAnnotation()); 
				ap.setSide(AnchorSide.TOP);
				getAnchorPoints().add(ap);
			}
			// Bottom right text
			else if(desc.getConnectionType()==ConnectionType.TEXT && desc.getType()==AnchorType.Origin ) {
				inputIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(inset+3*(sz.width-2*inset)/4,sz.height-inset),
						new Point(inset+3*(sz.width-2*inset)/4,sz.height+LEADER_LENGTH),
						new Rectangle(3*(sz.width-2*inset)/4,sz.height-2*inset,2*inset,2*inset),
						desc.getAnnotation());   // Hotspot shape.
				ap.setSide(AnchorSide.BOTTOM);
				getAnchorPoints().add(ap);
			}
			// Left side terminus - here we use the default behavior for side.
			else if( desc.getType()==AnchorType.Terminus  ) {
				outputIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Terminus,
						desc.getConnectionType(),
						new Point(inset,outputIndex*sz.height/outputCount),
						new Point(-LEADER_LENGTH,outputIndex*sz.height/outputCount),
						new Rectangle(0,outputIndex*sz.height/outputCount-inset,2*inset,2*inset),
						desc.getAnnotation());   // Hotspot shape.
				getAnchorPoints().add(ap);
				
			}
			// Right-side origin - also default behavior for side
			else if(desc.getType()==AnchorType.Origin ) {
				inputIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(sz.width-inset,inputIndex*sz.height/inputCount-1),
						new Point(sz.width+LEADER_LENGTH,inputIndex*sz.height/inputCount-1),
						new Rectangle(sz.width-2*inset,inputIndex*sz.height/inputCount-inset,2*inset,2*inset-1),
						desc.getAnnotation());
				getAnchorPoints().add(ap);
	
			}
		}
	}
	
	protected ProcessBlockView getBlock() { return this.block; }
	public BlockComponent getBlockComponent() { return this.blockComponent; }
	@Override
	public List<AnchorPoint> getAnchorPoints() { return this.anchorPoints; }
	
	@Override
	public void install(BlockComponent panel) {
		panel.setLayout(new BorderLayout());
		panel.add(this,BorderLayout.CENTER);
		blockComponent = panel;
	}
	 

	@Override
	protected abstract void paintComponent(Graphics _g);
	
	
	protected void drawAnchors(Graphics2D g,int xoffset,int yoffset) {
		// Preserve the original transform to roll back to at the end
		AffineTransform originalTx = g.getTransform();
		// Handle any offset of the block within the outer boundary
		g.translate(xoffset,yoffset);
		// Loop through the anchor points and draw squares for ports
		for( AnchorPoint ap:anchorPoints) {
			BasicAnchorPoint bap = (BasicAnchorPoint)ap;
			AnchorSide side = bap.getSide();
			int anchorWidth = anchorWidthForConnectionType(bap.getConnectionType());
			int anchorLength= INSET;  // Draw to the boundary
			Point loc = bap.getAnchor();   // Center of the anchor point
			// Paint the rectangle
			if( bap.getConnectionType()==ConnectionType.DATA) g.setColor(getBackground());
			else g.setColor(fillColorForConnectionType(bap.getConnectionType()));
			int x = 0;
			int y = 0;
			if( side==AnchorSide.TOP || side==AnchorSide.BOTTOM ) {
				// Up and down
				x = loc.x-anchorWidth/2;
				y = loc.y-anchorLength/2;
				g.fillRect(x, y, anchorWidth,anchorLength);
			}
			else  {
				x = loc.x-anchorLength/2;
				y = loc.y-anchorWidth/2;
				if( bap.getConnectionType()==ConnectionType.TRUTHVALUE) y+=2;  // Account for skinny connection
				g.fillRect(x, y, anchorLength,anchorWidth);
			}
			

			// A signal doesn't need an outline
			if( bap.getConnectionType()!=ConnectionType.SIGNAL ) {
				Stroke stroke = new BasicStroke(OUTLINE_WIDTH,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
				g.setStroke(stroke);
				g.setPaint(OUTLINE_COLOR);
				// Now paint the border on 2 sides -- always
				if( side==AnchorSide.TOP || side==AnchorSide.BOTTOM ) {
					g.drawLine(x+anchorWidth,y, x+anchorWidth, y+anchorLength);
					g.drawLine(x,y, x, y+anchorLength+1);
				}
				else {
					g.drawLine(x,y, x+anchorLength+1, y);
					g.drawLine(x,y+anchorWidth, x+anchorLength, y+anchorWidth);
				}
			}
			
			// Finally draw the annotation, if defined
			String annotation = bap.getAnnotation();
			if( annotation!=null && annotation.length()>0 ) {
				if( side==AnchorSide.TOP  ) {
					x = loc.x;
					y = loc.y+3*anchorLength/2;
				}
				else if( side==AnchorSide.BOTTOM ) {
					x = loc.x;
					y = loc.y-3*anchorLength;
				}
				else if( side==AnchorSide.LEFT  ) {
					x = loc.x+2*anchorWidth;
					y = loc.y;
				}
				else if( side==AnchorSide.RIGHT ) {
					x = loc.x-2*anchorWidth;
					y = loc.y;
				}
				paintTextAt(g,annotation,x,y,Color.BLACK,ANCHOR_ANNOTATION_TEXT_SIZE);
			}
			// As a debugging aid - highlight the hotspot
			if( log.isDebugEnabled() ) {
				g.setPaint(Color.MAGENTA);
				Shape hotspot = bap.getHotSpot();
				g.fill(hotspot);
			}
		}
		// Reverse any transforms we made
		g.setTransform(originalTx);
	}
	
	/**
	 *  Draw "badge" icons on top of the main rendering to indicate various block properties.
	 *  The embedded icons are not user-settable.
	 *  
	 *  The badge locations are:
	 *  1) Transmit enabled 3/4 along the top
	 *  2) Receive enabled 1/4 along the top. 
	 *  
	 */
	protected void drawBadges(Graphics2D g) {
		Dimension sz = getPreferredSize();

		// Receive
		if(block.isReceiveEnabled()) {
			// x,y,width,height
			Rectangle bounds = new Rectangle((sz.width-2*INSET)/4-INSET,0,BADGE_WIDTH,BADGE_HEIGHT);
			String path = "Block/icons/badges/receiver.png";
			paintBadge(g,path,bounds);
		}
		// Transmit
		if(block.isTransmitEnabled()) {
			Rectangle bounds = new Rectangle(3*(sz.width-2*INSET)/4,0,BADGE_WIDTH,BADGE_HEIGHT);
			String path = "Block/icons/badges/transmitter.png";
			paintBadge(g,path,bounds);
		}
	}
	
	protected void drawEmbeddedIcon(Graphics2D g) {
		String iconPath = block.getEmbeddedIcon();
		if( iconPath == null || iconPath.length()==0 ) return;
	
		Dimension imageSize = new Dimension(2*getPreferredSize().width/3-2*INSET,2*getPreferredSize().height/3-2*INSET);
		Image img = ImageLoader.getInstance().loadImage(iconPath,imageSize);
		ImageIcon icon = null;
		if( img !=null) icon = new ImageIcon(img);
		if( icon!=null ) {
			int x = (getPreferredSize().width - imageSize.width)/2;
			int y = (getPreferredSize().height - imageSize.height)/2;
			if( x>0 && y>0 ) {
				icon.paintIcon(getBlockComponent(), g, x, y);
			}
		}
		else {
			log.warnf("%s: drawEmbeddedIcon Missing icon at %s for %s",TAG,iconPath,block.getName());
		}
	}
	
	// Draw the text that is part of the rendered box. Recognize \n or \\n as newlines.
	// Pad with spaces so that we center
	// The yborder is all we care about for the moment. As for x, the lines are always centered.
	protected void drawEmbeddedText(Graphics2D g,int borderx,int bordery) {
		String text = block.getEmbeddedLabel();
		if( text == null || text.length()==0 ) return;
		Dimension sz = getPreferredSize();
		String[] lines = text.split("\n");
		if( lines.length==1 ) lines = text.split("\\n");
		int lineCount = lines.length;
		int dy = block.getEmbeddedFontSize()*2;
		int y = sz.height/2 - (lineCount*dy/2);
		for( String line: lines) {
			paintTextAt(g,line,sz.width/2,y,Color.BLACK,block.getEmbeddedFontSize());
			y+=dy;
		}
	}
	
	private int anchorWidthForConnectionType(ConnectionType type) {
		int size = WorkspaceConstants.CONNECTION_WIDTH_SIGNAL;   // Thinnest
		if( type==ConnectionType.TRUTHVALUE ) size = WorkspaceConstants.CONNECTION_WIDTH_TRUTHVALUE;
		else if( type==ConnectionType.DATA  ) size = WorkspaceConstants.CONNECTION_WIDTH_DATA;
		else if( type==ConnectionType.TEXT  ) size = WorkspaceConstants.CONNECTION_WIDTH_INFORMATION;
		else if( type==ConnectionType.ANY  ) size = WorkspaceConstants.CONNECTION_WIDTH_INFORMATION;
		return size;
	}
	
	private Color fillColorForConnectionType(ConnectionType type) {
		Color color = WorkspaceConstants.CONNECTION_BACKGROUND;   // Black
		if( type==ConnectionType.TRUTHVALUE ) color = WorkspaceConstants.CONNECTION_FILL_TRUTHVALUE;
		else if( type==ConnectionType.DATA  ) color = WorkspaceConstants.CONNECTION_FILL_DATA;
		else if( type==ConnectionType.TEXT  ) color = WorkspaceConstants.CONNECTION_FILL_INFORMATION;
		else if( type==ConnectionType.SIGNAL) color = WorkspaceConstants.CONNECTION_FILL_SIGNAL;
		else if( type==ConnectionType.ANY  ) color = WorkspaceConstants.CONNECTION_FILL_INFORMATION;
		return color;
	}
	
	private void paintBadge(Graphics2D g,String iconPath,Rectangle bounds) {
		if( iconPath == null || iconPath.length()==0 ) return;
	
		Dimension imageSize = new Dimension(bounds.width,bounds.height);
		Image img = ImageLoader.getInstance().loadImage(iconPath,imageSize);
		ImageIcon icon = null;
		if( img !=null) icon = new ImageIcon(img);
		if( icon!=null ) {
			icon.paintIcon(getBlockComponent(), g, bounds.x, bounds.y);
		}
		else {
			log.warnf("%s.paintBadge Missing icon at %s for %s",TAG,iconPath,block.getName());
		}
	}
	
	/**
	 * Utility method to paint a text string.
	 * @param g
	 * @param text
	 * @param xpos center of the text
	 * @param ypos center of the text
	 * @param fill color of the text
	 */
	private void paintTextAt(Graphics2D g, String text, float xpos, float ypos, Color fill,int fontSize) {
		Font font = g.getFont();
		font = font.deriveFont(fontSize);
		FontRenderContext frc = g.getFontRenderContext();
		GlyphVector vector = font.createGlyphVector(frc, text);
		Rectangle2D bounds = vector.getVisualBounds();
		// xpos, ypos are centers. Adjust to upper left.
		ypos+= bounds.getHeight()/2f;
		xpos-= bounds.getWidth()/2f;

		Shape textShape = vector.getOutline(xpos, ypos);
		g.setColor(fill);
		g.fill(textShape);
	}

}
