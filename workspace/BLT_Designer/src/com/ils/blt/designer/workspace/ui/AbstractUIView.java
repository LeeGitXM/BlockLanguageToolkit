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
import java.util.Collection;
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
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
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
	private final List<AnchorDescriptor> anchorDescriptors;  // Entries are BasicAnchorPoint
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
		anchorDescriptors = new ArrayList<AnchorDescriptor>();
		anchorPoints = new ArrayList<AnchorPoint>();
	}

	/**
	 *  Create anchor points from the anchor descriptions. This default implementation
	 *  places at most one input on the left and one output on the right. It assumes the main UI has
	 *  insets of INSET on all 4 sides. The anchor "leader" should be at least 10 pixels outside the component.
	 *  The anchor locations are center points of of a square. The hotspot is twice the size of the visible square. 
	 *  
	 *  Note: This is NOT called from the constructor of the base class.
	 *        Call from the constructor of each sub-class.
	 */
	protected void initAnchorPoints() {
		Dimension sz = getPreferredSize();
		boolean hasTerminus = false;
		boolean hasOrigin  = false;
		
		for(ProcessAnchorDescriptor desc:block.getAnchors()) {
			// Create a local anchor array from the block
			anchorDescriptors.add(desc);
			// Left side terminus
			if( desc.getType()==AnchorType.Terminus  && ! hasTerminus) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Terminus,
						desc.getConnectionType(),
						new Point(INSET,sz.height/2),
						new Point(-LEADER_LENGTH,sz.height/2),
						new Rectangle(0,sz.height/2-INSET,2*INSET,2*INSET));   // x,y,width,height. Hotspot shape.
				getAnchorPoints().add(ap);
				hasTerminus = true;
			}
			// Right-side origin
			else if(desc.getType()==AnchorType.Origin  && ! hasOrigin) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						desc.getConnectionType(),
						new Point(sz.width-INSET,sz.height/2),
						new Point(sz.width+LEADER_LENGTH,sz.height/2),
						new Rectangle(sz.width-INSET,sz.height/2-INSET,2*INSET,2*INSET));
				getAnchorPoints().add(ap);
				hasOrigin = true;
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
	 * Utility method to paint a text string
	 * @param g
	 * @param text
	 * @param xpos
	 * @param ypos
	 * @param fill
	 */
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

	/**
	 * Make our own array as it seems like the generic arrays do
	 * not follow inheritance patterns
	 */
	@Override
	public Collection<AnchorDescriptor> getAnchors() {
		return anchorDescriptors;
	}
	
	protected void drawAnchors(Graphics2D g) {
		// Loop through the anchor points and draw 6x6 squares
		for( AnchorPoint ap:anchorPoints) {
			BasicAnchorPoint bap = (BasicAnchorPoint)ap;
			AnchorSide side = bap.getSide();
			int anchorSize = anchorSizeForConnectionType(bap.getConnectionType());
			Point loc = bap.getAnchor();   // Center of the anchor point
			int offset = anchorSize/2;
			int x = (loc.x>offset?loc.x-offset:0);
			int y = (loc.y>offset?loc.y-offset:0);
			// Paint the rectangle
			if( bap.getConnectionType()==ConnectionType.DATA) g.setColor(getBackground());
			else g.setColor(fillColorForConnectionType(bap.getConnectionType()));
			g.fillRect(x, y, anchorSize,anchorSize);
			
			Stroke stroke = new BasicStroke(OUTLINE_WIDTH,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
			g.setStroke(stroke);
			g.setPaint(OUTLINE_COLOR);
			// Now paint the border on 2 sides
			if( side==AnchorSide.TOP || side==AnchorSide.BOTTOM ) {
				g.drawLine(x+anchorSize,y, x+anchorSize, y+anchorSize);
				g.drawLine(x,y, x, y+anchorSize);
			}
			else {
				g.drawLine(x,y, x+anchorSize, y);
				g.drawLine(x,y+anchorSize, x+anchorSize, y+anchorSize);
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
	
	private int anchorSizeForConnectionType(ConnectionType type) {
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
		else if( type==ConnectionType.ANY  ) color = WorkspaceConstants.CONNECTION_FILL_INFORMATION;
		return color;
	}

}
