package com.ils.blt.designer.workspace.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JComponent;

import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.ProcessBlockView;
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
	private final ProcessBlockView block;
	private final List<AnchorPoint> anchorPoints;  // Entries are BasicAnchorPoint
	private BlockComponent blockComponent = null;
	protected final int ANCHOR_SIZE = 6;
	protected final int INSET = 10;
	
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
	 *  
	 *  Note: This is NOT called from the constructor of the base class.
	 *        Call from the constructor of each sub-class.
	 */
	protected void initAnchorPoints() {
		Dimension sz = getPreferredSize();
		boolean hasTerminus = false;
		boolean hasOrigin  = false;
		for(AnchorDescriptor desc:block.getAnchors()) {
			if( desc.getType()==AnchorType.Terminus  && ! hasTerminus) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Terminus,
						new Point(INSET,sz.height/2),
						new Point(-INSET,sz.height/2),
						new Rectangle(INSET,sz.height/2,INSET,INSET));
				getAnchorPoints().add(ap);
				hasTerminus = true;
			}
			else if(desc.getType()==AnchorType.Origin  && ! hasOrigin) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),block,AnchorType.Origin,
						new Point(sz.width-INSET,sz.height/2),
						new Point(sz.width+INSET,sz.height/2),
						new Rectangle(sz.width-3*INSET/2,(sz.height-INSET)/2,INSET,INSET));
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

	@Override
	public Collection<AnchorDescriptor> getAnchors() {
		return block.getAnchors();
	}
	
	protected void drawAnchors(Graphics2D g) {
		// Loop through the anchor points and draw 6x6 squares
		for( AnchorPoint ap:anchorPoints) {
			BasicAnchorPoint bap = (BasicAnchorPoint)ap;
			AnchorSide side = bap.getSide();
			Point loc = bap.getAnchor();   // Center of the anchor point
			int offset = ANCHOR_SIZE/2;
			int x = (loc.x>offset?loc.x-offset:0);
			int y = (loc.y>offset?loc.y-offset:0);
			// Paint the rectangle
			g.setColor(Color.LIGHT_GRAY);
			g.fillRect(x, y, ANCHOR_SIZE,ANCHOR_SIZE);
			
			// Now point the border on 3 sides
			/*
			switch(side) 
			{
				case TOP:
					loc = new Point(loc.x,loc.y);
					break;
				case RIGHT:
					loc = new Point(loc.x,loc.y);
					break;
				case LEFT:
					loc = new Point(loc.x,loc.y);
					break;
				case BOTTOM:
					loc = new Point(loc.x,loc.y);
					break;
				default:    // Don't offset
			}
			*/
		}
	}

}
