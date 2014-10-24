/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.client.component;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.Rectangle2D;

import javax.swing.border.BevelBorder;
import javax.swing.border.Border;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.vision.api.client.components.model.AbstractVisionComponent;
/**
 * This is a base class for our custom Vision components that use Prefuse to view stuff.
 */
public abstract class PrefuseViewerComponent extends AbstractVisionComponent {
	private static final long serialVersionUID = 6091339174784663157L;
	
	// Specify default block dimensions
	protected static final int DEFAULT_FRAME_HEIGHT  = 240;   
	protected static final int DEFAULT_FRAME_WIDTH   = 360;       
	
	protected static final Color BORDER_HIGHLIGHT_COLOR = new Color(250,250,250);
	protected static final Color BORDER_SHADOW_COLOR = new Color(10,10,10);
	protected static final int   BORDER_WIDTH = 6;       // For components where we draw the border
	
	protected static final Color DEFAULT_BACKGROUND_COLOR = new Color(250,250,250);

	 /** The foreground color is the filler color of the border of most blocks. */
	protected static final Color DEFAULT_FRAME_COLOR = Color.CYAN;
	protected static final Font DEFAULT_HEADING_FONT = new Font("Dialog", Font.PLAIN, 24); 
	protected static final Font DEFAULT_SUBHEADING_FONT = new Font("Dialog", Font.PLAIN, 18); 
	protected final LoggerEx log;
	protected Color frameColor = DEFAULT_FRAME_COLOR;
	protected Border border = null;
	
	/**
	 * Normal form of the constructor.
	 */
	public PrefuseViewerComponent() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		setOpaque(true);

		border = new BevelBorder(BevelBorder.LOWERED,BORDER_HIGHLIGHT_COLOR,BORDER_SHADOW_COLOR);
		
		setPreferredSize(new Dimension(DEFAULT_FRAME_WIDTH,DEFAULT_FRAME_HEIGHT));
		setBackground(DEFAULT_BACKGROUND_COLOR);
		setForeground(DEFAULT_FRAME_COLOR);
		setFont(DEFAULT_HEADING_FONT);
	}
}
