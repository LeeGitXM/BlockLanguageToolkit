/**
  *  Copyright (c) 2021  ILS Automation. All rights reserved. 
 *  
 *  This replaces what was known as an attribute display in the old system.
 *  It is a dynamic text string that automatically updates when the property in the linked
 *  object gets a new value.   
 */
package com.ils.blt.designer.workspace.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.util.Collection;

import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;

import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.designer.workspace.AttributeDisplayView;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;

/** 
 * This is the renderer for an AttributeDisplayView. It shows the value of a single property.
 * The UI is the same for all instances, so some component attributes are simply hard-coded here.
 *  
 */
@SuppressWarnings("serial")
public class AttributeDisplayUIView extends AbstractDisplayUIView implements BlockViewUI  {

	private final JLabel label;
	private String prefix = "";
	private String text = "";
	private String suffix = "";
	
	
	public AttributeDisplayUIView(AttributeDisplayView view) {
		super(view);
		this.log.infof("INITIALIZING an AttributeDisplayUIView for block %s (%s)", view.getBlock().getId().toString(),view.getPropertyName());
		this.label = new JLabel();
		label.setForeground(getForeground());
		label.setBackground(getBackground());
		label.setOpaque(true);
		label.setFont(getFont());
		label.setHorizontalAlignment(SwingConstants.LEFT);
	}

	@Override
	public void install(BlockComponent panel) {
		super.install(panel);
		// now that blockComponent is set, we can initialize its size
		setSizeFromProperties();
	}

	// Set the display value and repaint.
	public void setText(String text) {
		this.log.infof("Set TEXT AttributeDisplayUIView for block %s %s (%s)", this.getDisplay().getBlock().getName(),this.getDisplay().getPropertyName(),text);
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				blockComponent.invalidate();
				blockComponent.repaint();
			}
		});
	}
	
	@Override
	protected void paintComponent(Graphics g) {;
		this.log.infof("PAINT AttributeDisplayUIView for block %s (%s)", this.getDisplay().getBlock().getName(),text);
//		Border border = new LineBorder(Color.black,1);
		final String html = "<html>" + prefix + " " + text + " " + suffix + "</html>";
		
		// Display using the block properties:

		label.setText(html);
		label.validate();
		label.paint(g);
	}

	/** Set the visible box's size from the width and height block properties. */
	private void setSizeFromProperties() {
		/*
		int height = getHeightPropertyValue();
		int width = getWidthPropertyValue();
		blockComponent.setPreferredSize(new Dimension(width, height));
		*/
	}
	
	
	@Override
	public void stateChanged(ChangeEvent e) {
		// TODO Auto-generated method stub
		
	}
}
