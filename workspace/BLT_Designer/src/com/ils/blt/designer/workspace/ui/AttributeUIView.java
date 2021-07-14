package com.ils.blt.designer.workspace.ui;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;

import javax.swing.SwingUtilities;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.designer.workspace.BlockAttributeView;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;


/**
 * Create a rectangular display similar to a "readout". The block has a large number of properties
 * related to formatting of the output.
 * 
 * There are no anchor points.
 */
public class AttributeUIView extends AbstractBlockUIView implements BlockViewUI {
	private static final long serialVersionUID = 2160868310475735865L; 
	private ProcessDiagramView diagram = null;
	private ProcessBlockView referenceBlock = null;
	private static final int DEFAULT_HEIGHT = 40;
	private static final int DEFAULT_WIDTH = 80;
	private BlockAttributeView attributeView;
	private String propertyName;
	private int height = DEFAULT_HEIGHT;
	private int width  = DEFAULT_WIDTH;
	private int offsetx = 0;
	private int offsety = 0;
	private final UtilityFunctions fncs;
	private BlockProperty valueProperty = null;  /// This is the watched property on the reference block
	
	// 
	public AttributeUIView(ProcessBlockView view) {
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		if( view instanceof BlockAttributeView ) {
			attributeView = (BlockAttributeView)view;
			diagram = attributeView.getDiagram();
			referenceBlock = attributeView.getReferenceBlock();
			propertyName = attributeView.getProperty(BlockConstants.ATTRIBUTTE_DISPLAY_PROPERTY).getValue().toString();
			if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_NAME)) {
				valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_NAME,referenceBlock.getName(),PropertyType.STRING,false);
			}
			else {
				valueProperty = referenceBlock.getProperty(propertyName);
			}
			
		}
		this.fncs = new UtilityFunctions();
		setOpaque(false);
	}

	
	
	@Override
	protected void paintComponent(Graphics _g) {
		// Calling the super method effects an "erase".
		Graphics2D g = (Graphics2D) _g;
		//log.infof("AttributeUIView.paintComponent %s ...(%d:%s)",getBlock().getName(),valueProperty.hashCode(),fncs.coerceToString(valueProperty.getValue()) );
		// Preserve the original transform to roll back to at the end
		AffineTransform originalTx = g.getTransform();
		Color originalBackground = g.getBackground();

		// Turn on anti-aliasing
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
		g.setPaint(Color.BLACK);
		
		// Calculate the inner area
		Rectangle ifb = new Rectangle();   // Interior, frame and border
		ifb = SwingUtilities.calculateInnerArea(this,ifb);
		// Now translate so that 0,0 is is at the inner origin
		if( referenceBlock!=null ) {
			ifb.x += 80;
			ifb.y += 40;
			//setLocation(referenceBlock.getLocation().x+50,referenceBlock.getLocation().y+30);
		}
		g.translate(ifb.x, ifb.y);

		// Now leave space for stubs and border
		int inset = INSET;
		ifb.x += inset;
		ifb.y += inset;
		ifb.width  -= 2*(inset);
		ifb.height -= 2*(inset);
		// Create a rectangle for the border that is within the insets. 
		// Use the upper left for light shading, the lower right for dark
		int[] xulvertices = new int[] {ifb.x,            ifb.x,ifb.x+ifb.width,ifb.x };
		int[] yulvertices = new int[] {ifb.y+ifb.height, ifb.y,ifb.y,ifb.y+ifb.height};
		Polygon fi = new Polygon(xulvertices,yulvertices,4);
		g.setColor(BORDER_LIGHT_COLOR);
		g.fillPolygon(fi);
		g.draw(fi);
		
		// This is a triangle (sort-of), the lower-right half. 
		int[] xlrvertices = new int[] {ifb.x,  ifb.x+BORDER_WIDTH,ifb.x+ifb.width-BORDER_WIDTH,           ifb.x+ifb.width,  ifb.x+ifb.width,ifb.x };
		int[] ylrvertices = new int[] {ifb.y+ifb.height,ifb.y+ifb.height-BORDER_WIDTH,ifb.y+BORDER_WIDTH, ifb.y,ifb.y+ifb.height,ifb.y+ifb.height};
		fi = new Polygon(xlrvertices,ylrvertices,6);
		g.setColor(BORDER_DARK_COLOR);
		g.fillPolygon(fi);
		g.draw(fi);

		ifb.x += BORDER_WIDTH;
		ifb.y += BORDER_WIDTH;
		ifb.width  -= 2*(BORDER_WIDTH);
		ifb.height -= 2*(BORDER_WIDTH);
		// Create a rectangle that is within the border boundaries
		int[] xvertices = new int[] {ifb.x, ifb.x+ifb.width,ifb.x+ifb.width,ifb.x };
		int[] yvertices = new int[] {ifb.y, ifb.y,ifb.y+ifb.height,ifb.y+ifb.height};
		fi = new Polygon(xvertices,yvertices,4);
		int rgb = block.getBackground();
		Color background = new Color(rgb);
		g.setColor(background); 
		g.fillPolygon(fi);
		// Outline the inner square
		g.setPaint(INSET_COLOR);
		g.draw(fi);

		// Reverse any transforms we made
		g.setTransform(originalTx);
		g.setBackground(originalBackground);
		// Update embedded text - the block formats the output
		// We are guaranteed that the property value is a qualified value.
		String value  = "not set";
		if( valueProperty!=null ) {
			value = fncs.coerceToString(valueProperty.getValue());   // Just to be safe
		}
		
		// NOTE* No longer assume 100px width.  TimeReadout is wider.  The old setting of 8 for small was unreadable
		// Set the font size based on the string length.
		// Assumes 100px block width
		int fontSize = 12;  // Small
		if( value.length()<7 ) fontSize = 14;
		else if( value.length()<13 ) fontSize = 12;
		
		block.setEmbeddedFontSize(fontSize);
		block.setEmbeddedLabel(value);
		drawEmbeddedText(g,0,0);
	}
}
