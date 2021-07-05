/**
  *  Copyright (c) 2021  ILS Automation. All rights reserved. 
 *  
 *  This replaces what was known as an attribute display in the old system.
 *  It is a dynamic text string that automatically updates when the property in the linked
 *  object gets a new value.   
 */
package com.ils.blt.designer.workspace.ui;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.Rectangle2D;

import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.AttributeDisplayView;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import java.awt.Color;

/** 
 * This is the renderer for an AttributeDisplayView. It shows the value of a single property.
 * The UI is the same for all instances, so some component attributes are simply hard-coded here.
 *  
 */
@SuppressWarnings("serial")
public class AttributeDisplayUIView extends AbstractDisplayUIView implements BlockViewUI  {
	private final float FONT_SIZE = 12f;
	private final ProcessBlockView block;
	private final String property;
	
	
	public AttributeDisplayUIView(AttributeDisplayView view) {
		super(view);
		this.log.infof("INITIALIZING an AttributeDisplayUIView for block %s (%s)", view.getBlock().getId().toString(),view.getPropertyName());
		this.property = view.getPropertyName();
		this.block = view.getBlock();
	}

	@Override
	public void install(BlockComponent panel) {
		super.install(panel);
		setSizeFromProperties();
	}

	// Set the display value and repaint. Does this ever get called?
	public void repaint() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				blockComponent.invalidate();
				blockComponent.repaint();
			}
		});
	}
	// Paint texdt string
	@Override
	protected void paintComponent(Graphics _g) {;
		Graphics2D g = (Graphics2D) _g;
		//this.log.infof("PAINT AttributeDisplayUIView for block %s (%s)", this.getDisplay().getBlock().getName(),"TODO");
		//	Border border = new LineBorder(Color.black,1);
		String text = property+": "+getValue();
		
		Color fill = Color.BLACK;
		Font font = g.getFont();
		font = font.deriveFont(FONT_SIZE);  // This is, presumably the correct way
		FontRenderContext frc = g.getFontRenderContext();
		GlyphVector vector = font.createGlyphVector(frc, text);
		Rectangle2D bounds = vector.getVisualBounds();
		// xpos, ypos are centers. Adjust to upper left.
		float xpos = 0f;
		float ypos = 0f;
		ypos += bounds.getHeight()/2f;
		xpos += bounds.getWidth()/2f;

		Shape textShape = vector.getOutline(xpos, ypos);
		g.setColor(fill);
		g.fill(textShape);
	}

	/** Set the visible box's size from the width and height block properties. */
	private void setSizeFromProperties() {
		int height = display.getPreferredHeight();
		int width = display.getPreferredWidth();
		blockComponent.setPreferredSize(new Dimension(width, height));
	}
	
	
	@Override
	public void stateChanged(ChangeEvent e) {
		// TODO Auto-generated method stub
		
	}
	
	private String getValue() {
		String text = "PROPERTY NOT FOUND";
		if( property.equalsIgnoreCase("Name")) {
			text = block.getName();
		}
		else {
			BlockProperty prop = block.getProperty(property);
			if( prop!=null ) text = prop.getValue().toString();
		}
		return text;
	}
}
