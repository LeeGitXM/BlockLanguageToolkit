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
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.WorkspaceConstants;
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
 * 
 */
@SuppressWarnings("serial")
public abstract class AbstractUIView extends JComponent 
									 implements BlockViewUI,ChangeListener {
	private static final String TAG = "AbstractUIView";
	protected final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	protected ProcessBlockView block = null;
	private final List<AnchorPoint> anchorPoints;  // Entries are BasicAnchorPoint
	protected BlockComponent blockComponent = null;
	protected static int ANCHOR_ANNOTATION_TEXT_SIZE = 9;
	protected final static int BADGE_HEIGHT = 20;
	public static final int   BLOCK_DIRTY_SHADING = 0x303030;    //Subtract from background color to darken
	protected final static int BADGE_WIDTH = 20;
	protected final static int BORDER_WIDTH = 3;
	protected final static Color BORDER_DARK_COLOR = Color.darkGray;
	protected final static Color BORDER_LIGHT_COLOR = new Color(250,250,250); // Light gray
	protected final static Color INSET_COLOR = new Color(210,210,210);        // A little darker gray
	protected final static int INSET = 6;
	protected final static int LEADER_LENGTH = 10;
	// If leader is less than 10, we get straight lines
	protected final static int SIGNAL_LEADER_LENGTH = 10;       // Would like shorter for signals
	protected final static Color OUTLINE_COLOR = Color.BLACK;   // For stub
	protected final static float OUTLINE_WIDTH = 1.0f;          // For stub
	protected final static Color TEXT_COLOR = Color.BLACK;      // For embedded label
	protected int hiddenIndex = -1;   // Allow for anchor to be hidden
	
	/**
	 * Use default height and widths supplied by subclass when values not assigned by view.
	 * @param view
	 * @param defaultWidth
	 * @param defaultHeight
	 */
	public AbstractUIView(ProcessBlockView view,int defaultWidth,int defaultHeight) {
		this.block = view;
		this.block.addChangeListener(this);
		setOpaque(true);
		int preferredHeight = view.getPreferredHeight();
		if( preferredHeight<=0 ) preferredHeight = defaultHeight;
		int preferredWidth = view.getPreferredWidth();
		if( preferredWidth<=0 ) preferredWidth = defaultWidth;
		setPreferredSize(new Dimension(preferredWidth,preferredHeight)); 
		anchorPoints = new ArrayList<AnchorPoint>();
	}

	/**
	 *  Create anchor points from the anchor descriptions. This default implementation
	 *  makes assumptions about preferred locations depending on connection type (signals
	 *  on top and text output on the bottom). These can be overridden using placement "hints".
	 *  For multiple connections on a side, ordering is in the order in which the connections 
	 *  have been defined.  It assumes the main UI has insets of INSET on all 4 sides. 
	 *  The anchor "leader" should be at least 10 pixels outside the component.
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
		getAnchorPoints().clear();
		Dimension sz = getPreferredSize();
		// These actually end up the number of connections on a side
		int rightCount = 0;
		int rightIndex = 0;
		int leftCount= 0;
		int leftIndex= 0;
		int topCount = 0;
		int topIndex = 0;
		int bottomCount= 0;
		int bottomIndex= 0;
		
		int index = 0;        // Count of descriptors
		hiddenIndex = -1;     // Unless set, nothing is hidden
		// Create counts for each side. There are both defaults and placement hints.
		for(ProcessAnchorDescriptor desc:block.getAnchors()) {
			if( desc.isHidden()) hiddenIndex = index;
			PlacementHint hint = desc.getHint();
			if(hint==null) hint = PlacementHint.UNSPECIFIED;
			if( hint.equals(PlacementHint.L)) leftCount++;
			else if(hint.equals(PlacementHint.R)) rightCount++;
			else if(hint.equals(PlacementHint.B)) bottomCount++;
			else if(hint.equals(PlacementHint.T)) topCount++;
			// Now consider the defaults by type and direction
			else if(desc.getConnectionType()==ConnectionType.SIGNAL &&
					desc.getType().equals(AnchorType.Terminus) ) {
				desc.setHint(PlacementHint.T);
				topCount++;
			}
			else if(desc.getConnectionType()==ConnectionType.TEXT &&
			        desc.getType().equals(AnchorType.Origin) ) {
				desc.setHint(PlacementHint.B);
				bottomCount++;
			}
			
			else if(desc.getType().equals(AnchorType.Origin)  ) {
				desc.setHint(PlacementHint.R);
				rightCount++;
			}
			else if(desc.getType().equals(AnchorType.Terminus))  {
				desc.setHint(PlacementHint.L);
				leftCount++;
			}
			index++;
		}
		//log.infof("%s.initAnchorPoints counts(tblr) %d,%d,%d,%d ...",TAG,topCount,bottomCount,leftCount,rightCount);
		
		// Calculate side segments for the interior block (sans borders)
		int inset = INSET+BORDER_WIDTH;   // Align the connections with the un-bordered block
		int interiorWidth = sz.width-2*inset;
		int interiorHeight= sz.height-2*inset;
		
		// The segments are the number of divisions on a side (usually one more than the connection count).
		// With exactly two, it looks better if we divide into quarters with the middle empty.
		// With exactly one on top or bottom, push to the rightmost quarter.
		int topSegments = topCount+1;
		if( topCount == 1 || topCount==2) topSegments = 4;
		int bottomSegments = bottomCount+1;
		if( bottomCount == 1 || bottomCount==2) bottomSegments = 4;
		int leftSegments = leftCount+1;
		if( leftCount == 2) leftSegments = 4;
		int rightSegments = rightCount+1;
		if( rightCount == 2) rightSegments = 4;
		
		
		// Re-iterate using the same criteria as above
		// NOTE: The anchor point should be on the component boundary. Stubs are drawn inward.
		for(ProcessAnchorDescriptor desc:block.getAnchors()) {
			// Top
			if(desc.getHint().equals(PlacementHint.T) ) {
				topIndex++;
				if(topCount==1) {
					if(desc.getType().equals(AnchorType.Terminus)) topIndex=1;
					else topIndex=3;
				}
				else if(topCount==2 && topIndex==2) topIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Terminus,
						desc.getConnectionType(),
						new Point(inset+(topIndex*interiorWidth)/topSegments,0),
						new Point(inset+(topIndex*interiorWidth)/topSegments,-SIGNAL_LEADER_LENGTH),
						new Rectangle((topIndex*interiorWidth)/topSegments,0,2*inset,2*inset),
						desc.isMultiple(),
						desc.getAnnotation()); 
				ap.setSide(AnchorSide.TOP);
				getAnchorPoints().add(ap);
			}
			// Bottom
			else if(desc.getHint().equals(PlacementHint.B) ) {
				bottomIndex++;
				if(bottomCount==1) {
					if(desc.getType().equals(AnchorType.Terminus)) topIndex=1;
					else bottomIndex=3;
				}
				else if(bottomCount==2 && bottomIndex==2) bottomIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(inset+bottomIndex*(interiorWidth)/bottomSegments,sz.height),
						new Point(inset+bottomIndex*(interiorWidth)/bottomSegments,sz.height+LEADER_LENGTH),
						new Rectangle(bottomIndex*(interiorWidth)/bottomSegments,sz.height-2*inset,2*inset,2*inset),
						desc.isMultiple(),
						desc.getAnnotation());   // Hotspot shape.
				ap.setSide(AnchorSide.BOTTOM);
				getAnchorPoints().add(ap);
			}
			// Left side
			else if( desc.getHint().equals(PlacementHint.L)  ) {
				leftIndex++;
				if(leftCount==2 && leftIndex==2) leftIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Terminus,
						desc.getConnectionType(),
						new Point(0,inset+leftIndex*interiorHeight/leftSegments),
						new Point(-LEADER_LENGTH,inset+leftIndex*interiorHeight/leftSegments),
						new Rectangle(0,leftIndex*interiorHeight/leftSegments,2*inset,2*inset),
						desc.isMultiple(),
						desc.getAnnotation());   // Hotspot shape.
				getAnchorPoints().add(ap);
				
			}
			// Right-side
			else {
				rightIndex++;
				if(rightCount==2 && rightIndex==2) rightIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(sz.width,inset+rightIndex*interiorHeight/rightSegments-1),
						new Point(sz.width+LEADER_LENGTH,inset+rightIndex*interiorHeight/rightSegments-1),
						new Rectangle(sz.width-2*inset,rightIndex*interiorHeight/rightSegments,2*inset,2*inset-1),
						desc.isMultiple(),
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
	
	// Force a repaint.
	@Override
	public void update() {
		log.debugf("%s.update %s ...",TAG,getBlock().getName());
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				repaint();
			}
		});
	}
	
	// The block has changed, re-configure. The base method recalculates 
	// anchor points. The only current use is when the user changes connection types.
	public void reconfigure() { 
		log.debugf("%s.reconfigure (%s)",TAG,getBlock().getName());
		initAnchorPoints();
		
	}
	
	// The only "state" associated with the anchor points is the
	// visibility of the signal stub. Recompute the hidden index
	// and repaint.
	@Override
	public void stateChanged(ChangeEvent event) {
		int index = 0;        // Count of descriptors
		hiddenIndex = -1;     // Unless set, nothing is hidden
		// Create counts for each side. There are both defaults and placement hints.
		for(ProcessAnchorDescriptor desc:block.getAnchors()) {
			if( desc.isHidden()) hiddenIndex = index;
			index++;
		}
		log.debugf("%s.stateChanged %s ...hidden = %d",TAG,getBlock().getName(),hiddenIndex);
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				repaint();
			}
		});
	}
	
	// For now we're only checking for a top anchor to be hidden.
	protected void drawAnchors(Graphics2D g,int xoffset,int yoffset) {
		// Preserve the original transform to roll back to at the end
		AffineTransform originalTx = g.getTransform();
		// Handle any offset of the block within the outer boundary
		g.translate(xoffset,yoffset);
		// Loop through the anchor points and draw squares for ports
		// We assume that the anchor points are in the same order as the anchor descriptions
		int index = 0;
		for( AnchorPoint ap:anchorPoints) {
			BasicAnchorPoint bap = (BasicAnchorPoint)ap;
			AnchorSide side = bap.getSide();
			int anchorWidth = anchorWidthForConnectionType(bap.getConnectionType());
			int anchorLength= INSET+BORDER_WIDTH;       // Draw edge to the boundary
			Point loc = bap.getAnchor();                // Center of the anchor point
			// Paint the rectangle
			if( bap.getConnectionType()==ConnectionType.DATA) g.setColor(getBackground());
			else if( index==hiddenIndex)                      g.setColor(getBackground());
			else     g.setColor(fillColorForConnectionType(bap.getConnectionType()));
			//log.infof("%s.drawAnchors index %d (hide %d)",TAG,index,hiddenIndex);
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
					g.drawLine(x,y+1, x+anchorLength+1, y+1);     
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
			index++;
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
		// Locked
		if(block.isLocked()) {
			Rectangle bounds = new Rectangle(3*(sz.width-2*INSET)/4,3*(sz.height-2*INSET)/4,BADGE_WIDTH,BADGE_HEIGHT);
			String path = "Block/icons/badges/lock.png";
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
	protected void drawEmbeddedText(Graphics2D g,int offset,int bordery) {
		String text = block.getEmbeddedLabel();
		if( text == null || text.length()==0 ) return;
		Dimension sz = getPreferredSize();
		String[] lines = text.split("\n");
		if( lines.length==1 ) lines = text.split("\\n");
		int lineCount = lines.length;
		int dy = 3*block.getEmbeddedFontSize()/4;
		int y = sz.height/2 - (lineCount-1)*dy/2;
		for( String line: lines) {
			paintTextAt(g,line,sz.width/2+offset,y,Color.BLACK,block.getEmbeddedFontSize());
			y+=dy;
		}
	}
	
	protected int anchorWidthForConnectionType(ConnectionType type) {
		int size = WorkspaceConstants.CONNECTION_WIDTH_SIGNAL;   // Thinnest
		if( type==ConnectionType.TRUTHVALUE ) size = WorkspaceConstants.CONNECTION_WIDTH_TRUTHVALUE;
		else if( type==ConnectionType.DATA  ) size = WorkspaceConstants.CONNECTION_WIDTH_DATA;
		else if( type==ConnectionType.TEXT  ) size = WorkspaceConstants.CONNECTION_WIDTH_TEXT;
		else if( type==ConnectionType.ANY  ) size = WorkspaceConstants.CONNECTION_WIDTH_TEXT;
		return size;
	}
	
	protected Color fillColorForConnectionType(ConnectionType type) {
		Color color = WorkspaceConstants.CONNECTION_BACKGROUND;   // Black
		if( block==null ) color = WorkspaceConstants.CONNECTION_BACKGROUND;  // An error
		else if( type==ConnectionType.SIGNAL) color = WorkspaceConstants.CONNECTION_FILL_SIGNAL;
		else if( block.getState().equals(TruthValue.UNSET)) color = WorkspaceConstants.CONNECTION_FILL_EMPTY;
		else if( type==ConnectionType.TRUTHVALUE ) color = WorkspaceConstants.CONNECTION_FILL_UNKNOWN;
		else if( type==ConnectionType.DATA  ) color = WorkspaceConstants.CONNECTION_FILL_DATA;
		else if( type==ConnectionType.TEXT  ) color = WorkspaceConstants.CONNECTION_FILL_TEXT;
		else if( type==ConnectionType.ANY  ) color = WorkspaceConstants.CONNECTION_FILL_TEXT;
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
