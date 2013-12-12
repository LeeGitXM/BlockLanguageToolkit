/**
 *   (c) 2012-2013  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.component;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.util.ResourceBundle;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.BundleUtil;

/**
 *  This is the JComponent that renders a CallbackBlock.
 */
public class DiagramAnalyzerComponent extends AbstractDiagnosticsComponent {
	private static final long serialVersionUID = 4408313516136446100L;
	private static String PREFIX = BLTProperties.BUNDLE_PREFIX;              // For bundle identification
	

	public DiagramAnalyzerComponent() {
		
		setName(BundleUtil.get().getString(PREFIX+"Callback.Name"));
		setHeading(BundleUtil.get().getString(PREFIX+"Callback.Name"));
		this.setOpaque(true);
		this.setBorder(border);
	}

	@Override
	public boolean isSquare() {return false; }
	@Override
	public void setHeading(String text) { heading = text; }

	/**
	 * Overriding paintComponent is how you make a component that has custom
	 * graphics.
	 */
	@Override
	protected void paintComponent(Graphics _g) {
		// Calling the super method effects an "erase".
		Graphics2D g = (Graphics2D) _g;

		// Preserve the original transform to roll back to at the end
		AffineTransform originalTx = g.getTransform();

		// Turn on anti-aliasing
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
		// Reserve room for the border.
		Insets borderInsets = border.getBorderInsets(this);
		// Calculate the inner area, compensating for borders
		Rectangle ifb = new Rectangle();   // Interior, frame and border
		ifb = SwingUtilities.calculateInnerArea(this,ifb);
		// Now translate so that 0,0 is is at the inner origin
		g.translate(ifb.x, ifb.y);
		
		// Create a rectangle that is component less border
		Rectangle fi = new Rectangle();
		fi.x = borderInsets.left;
		fi.y = borderInsets.top;
		fi.width = ifb.width - borderInsets.left - borderInsets.right;
		fi.height = ifb.height - borderInsets.top - borderInsets.bottom;
		g.setColor(getForeground());
		g.fillRoundRect(fi.x, fi.y,fi.width, fi.height, 
				DEFAULT_ROUNDING_ARC_WIDTH, DEFAULT_ROUNDING_ARC_HEIGHT);

		// Now paint the inner rectangle
		int fw = DEFAULT_FRAME_WIDTH;
		int fh = DEFAULT_FRAME_HEIGHT;
		Rectangle interior = new Rectangle(fi.x+fw,fi.y+fh,fi.width-(2*fw),fi.height-(2*fh));
		g.setColor(getBackground());
		g.fillRoundRect(interior.x, interior.y,interior.width, interior.height, 
				DEFAULT_ROUNDING_ARC_WIDTH/2, DEFAULT_ROUNDING_ARC_HEIGHT/2);
		
		// Set the font for drawing the Heading
		g.setFont(getFont());
		FontMetrics fm = g.getFontMetrics();

		// Calculate the x,y for the String's baseline in order to center it
		String text = getHeading();
		int stringWidth = fm.stringWidth(text);
		int xpos = (ifb.width - stringWidth) / 2;
		float ypos = (float) ifb.getHeight() / 2f; // Position of center of text
		paintTextAt(g, text, xpos, ypos, getHeadingColor());
	
		// Finally outline both sides of the frame
		float borderWidth = 1.0f;
		Stroke stroke = new BasicStroke(borderWidth,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		g.setPaint(Color.BLACK);
		g.draw(interior);
		fi.x = fi.x -1;
		fi.y = fi.y -1;
		fi.width = fi.width+2;
		fi.height = fi.height+2;
		g.draw(fi);
	
		// border.paintBorder(this, g, 0, 0,ifb.width, ifb.height);
		// Reverse any transforms we made
		g.setTransform(originalTx);
	}

	

}
