package com.ils.blt.designer.workspace.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeListener;

import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.designer.workspace.AttributeDisplayView;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;


/**
 * This serves as a base class for our custom attribute display 2D renders.
 */
@SuppressWarnings("serial")
public abstract class AbstractDisplayUIView extends JComponent implements BlockViewUI,ChangeListener {
	private static final String CLSS = "AbstractDisplayUIView";
	protected final ILSLogger log = LogMaker.getLogger(getClass().getPackage().getName());
	private final List<AnchorPoint> anchorPoints = new ArrayList<>();
	protected AttributeDisplayView display = null;
	protected BlockComponent blockComponent = null;
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
	 * Paint the property value of some block via it's ProcessAttribute display.
	 * @param view
	 * @param defaultWidth
	 * @param defaultHeight
	 */
	public AbstractDisplayUIView(AttributeDisplayView disp) {
		this.display = disp;
		display.getBlock().addChangeListener(this);
		setOpaque(true);
		int preferredHeight = display.getPreferredHeight();
		if( preferredHeight<=0 ) preferredHeight = BlockConstants.PREFERRED_ATTRIBUTE_HEIGHT;
		int preferredWidth = display.getPreferredWidth();
		if( preferredWidth<=0 ) preferredWidth = BlockConstants.PREFERRED_ATTRIBUTE_WIDTH;
		setPreferredSize(new Dimension(preferredWidth,preferredHeight)); 
	}


	protected AttributeDisplayView getDisplay() { return this.display; }
	public BlockComponent getBlockComponent() { return this.blockComponent; }
	
	@Override
	public void install(BlockComponent panel) {
		panel.setLayout(new BorderLayout());
		panel.add(this,BorderLayout.CENTER);
		blockComponent = panel;
	}

	@Override
	public List<AnchorPoint> getAnchorPoints() { return this.anchorPoints; }
	
	@Override
	protected abstract void paintComponent(Graphics _g);
	
	// Force a repaint.
	@Override
	public void update() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				repaint();
			}
		});
	}

}
