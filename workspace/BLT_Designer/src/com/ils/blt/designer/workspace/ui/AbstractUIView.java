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
	private LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private final ProcessBlockView block;
	private final List<AnchorPoint> anchorPoints;  // Entries are BasicAnchorPoint
	private BlockComponent blockComponent = null;
	protected final static int INSET = 6;
	protected final static int LEADER_LENGTH = 10;
	protected final static Color OUTLINE_COLOR = Color.BLACK;   // For stub
	protected final static float OUTLINE_WIDTH = 1.0f;          // For stub
	protected final static Color TEXT_COLOR = Color.BLACK;      // For embedded label
	protected final static Dimension EMBEDDED_IMAGE_SIZE = new Dimension(64,64);
	
	public AbstractUIView(ProcessBlockView view) {
		this.block = view;
		setOpaque(false);
		setPreferredSize(new Dimension(100,100));   // This can be overriden
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
			if(desc.getConnectionType()==ConnectionType.INFORMATIONAL &&
			   desc.getType()==	AnchorType.Terminus) continue;
			
			if(desc.getType()==	AnchorType.Origin ) inputCount++;
			else if(desc.getType()==AnchorType.Terminus ) outputCount++;
		}
		outputCount++;   // Now equals the number of segments on a side
		inputCount++;
		
		for(ProcessAnchorDescriptor desc:block.getAnchors()) {
			// Top left signal
			if(desc.getConnectionType()==ConnectionType.SIGNAL && desc.getType()==AnchorType.Terminus ) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Terminus,
						desc.getConnectionType(),
						new Point(INSET+(sz.width-2*INSET)/4,INSET+1),
						new Point(INSET+(sz.width-2*INSET)/4,-LEADER_LENGTH),
						new Rectangle((sz.width-2*INSET)/4,0,2*INSET,2*INSET));   // x,y,width,height. Hotspot shape.
				ap.setSide(AnchorSide.TOP);
				getAnchorPoints().add(ap);
			}
			// Top right signal
			else if(desc.getConnectionType()==ConnectionType.SIGNAL && desc.getType()==AnchorType.Origin ) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(INSET+3*(sz.width-2*INSET)/4,INSET+1),
						new Point(INSET+3*(sz.width-2*INSET)/4,-LEADER_LENGTH),
						new Rectangle(3*(sz.width-2*INSET)/4,0,2*INSET,2*INSET)); 
				ap.setSide(AnchorSide.TOP);
				getAnchorPoints().add(ap);
			}
			// Bottom right text
			else if(desc.getConnectionType()==ConnectionType.INFORMATIONAL && desc.getType()==AnchorType.Origin ) {
				inputIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(INSET+3*(sz.width-2*INSET)/4,sz.height-INSET),
						new Point(INSET+3*(sz.width-2*INSET)/4,sz.height+LEADER_LENGTH),
						new Rectangle(3*(sz.width-2*INSET)/4,sz.height-2*INSET,2*INSET,2*INSET));   // Hotspot shape.
				ap.setSide(AnchorSide.BOTTOM);
				getAnchorPoints().add(ap);
			}
			// Left side terminus - here we use the default behavior for side.
			else if( desc.getType()==AnchorType.Terminus  ) {
				outputIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Terminus,
						desc.getConnectionType(),
						new Point(INSET,outputIndex*sz.height/outputCount),
						new Point(-LEADER_LENGTH,outputIndex*sz.height/outputCount),
						new Rectangle(0,outputIndex*sz.height/outputCount-INSET,2*INSET,2*INSET));   // Hotspot shape.
				getAnchorPoints().add(ap);
				
			}
			// Right-side origin - also default behavior for side
			else if(desc.getType()==AnchorType.Origin ) {
				inputIndex++;
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(sz.width-INSET,inputIndex*sz.height/inputCount),
						new Point(sz.width+LEADER_LENGTH,inputIndex*sz.height/inputCount),
						new Rectangle(sz.width-2*INSET,inputIndex*sz.height/inputCount-INSET,2*INSET,2*INSET));
				getAnchorPoints().add(ap);
	
			}
		}
	}
	
	protected ProcessBlockView getBlock() { return this.block; }
	protected BlockComponent getBlockComponent() { return this.blockComponent; }
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
	
	/**
	 * Utility method to paint a text string. Font size is hardcoded.
	 * @param g
	 * @param text
	 * @param xpos center of the text
	 * @param ypos center of the text
	 * @param fill color of the text
	 */
	protected void paintTextAt(Graphics2D g, String text, float xpos, float ypos, Color fill,int fontSize) {
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


	protected void drawAnchors(Graphics2D g) {
		// Loop through the anchor points and draw squares for ports
		for( AnchorPoint ap:anchorPoints) {
			BasicAnchorPoint bap = (BasicAnchorPoint)ap;
			AnchorSide side = bap.getSide();
			int anchorWidth = anchorWidthForConnectionType(bap.getConnectionType());
			int anchorLength= INSET;  // Draw to the boundary
			Point loc = bap.getAnchor();   // Center of the anchor point
			// As a debugging aid - highlight the hotspot
			if( log.isDebugEnabled() ) {
				g.setPaint(Color.MAGENTA);
				Shape hotspot = bap.getHotSpot();
				g.fill(hotspot);
			}
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
			else {
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
					g.drawLine(x,y, x+anchorLength+1, y);
					g.drawLine(x,y+anchorWidth, x+anchorLength, y+anchorWidth);
				}
			}
		}
	}
	
	protected void drawEmbeddedIcon(Graphics2D g) {
		String iconPath = block.getEmbeddedIcon();
		if( iconPath == null || iconPath.length()==0 ) return;
	
		Image img = ImageLoader.getInstance().loadImage(iconPath,EMBEDDED_IMAGE_SIZE);
		ImageIcon icon = null;
		if( img !=null) icon = new ImageIcon(img);
		if( icon!=null ) {
			int x = (getPreferredSize().width - EMBEDDED_IMAGE_SIZE.width)/2;
			int y = (getPreferredSize().height - EMBEDDED_IMAGE_SIZE.height)/2;
			if( x>0 && y>0 ) {
				icon.paintIcon(getBlockComponent(), g, x, y);
			}
		}
		else {
			log.warnf("%s: drawEmbeddedIcon Missing icon at %s for %s",TAG,iconPath,block.getLabel());
		}
	}
	
	protected void drawEmbeddedText(Graphics2D g) {
		String text = block.getEmbeddedLabel();
		if( text == null || text.length()==0 ) return;
		Dimension sz = getPreferredSize();
		paintTextAt(g,text,sz.width/2,sz.height/2,Color.BLACK,block.getEmbeddedFontSize());
		
	}
	
	private int anchorWidthForConnectionType(ConnectionType type) {
		int size = WorkspaceConstants.CONNECTION_WIDTH_SIGNAL;   // Thinnest
		if( type==ConnectionType.TRUTHVALUE ) size = WorkspaceConstants.CONNECTION_WIDTH_TRUTHVALUE;
		else if( type==ConnectionType.DATA  ) size = WorkspaceConstants.CONNECTION_WIDTH_DATA;
		else if( type==ConnectionType.INFORMATIONAL  ) size = WorkspaceConstants.CONNECTION_WIDTH_INFORMATION;
		else if( type==ConnectionType.ANY  ) size = WorkspaceConstants.CONNECTION_WIDTH_INFORMATION;
		return size;
	}
	
	private Color fillColorForConnectionType(ConnectionType type) {
		Color color = WorkspaceConstants.CONNECTION_BACKGROUND;   // Black
		if( type==ConnectionType.TRUTHVALUE ) color = WorkspaceConstants.CONNECTION_FILL_TRUTHVALUE;
		else if( type==ConnectionType.DATA  ) color = WorkspaceConstants.CONNECTION_FILL_DATA;
		else if( type==ConnectionType.INFORMATIONAL  ) color = WorkspaceConstants.CONNECTION_FILL_INFORMATION;
		else if( type==ConnectionType.SIGNAL  )        color = WorkspaceConstants.CONNECTION_FILL_SIGNAL;
		else if( type==ConnectionType.ANY  ) color = WorkspaceConstants.CONNECTION_FILL_INFORMATION;
		return color;
	}

}
