/**
 *  Copyright (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.designer.workspace.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;

import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.ils.block.common.BlockConstants;
import com.ils.block.common.BlockProperty;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;

/** Draw a free-form text (or html) note in a box. */
@SuppressWarnings("serial")
public class NoteUIView extends AbstractUIView implements BlockViewUI, ChangeListener {

	private JLabel label = new JLabel();
	private BlockProperty textProperty;
	private BlockProperty widthProperty;
	private BlockProperty heightProperty;
	
	public NoteUIView(ProcessBlockView view) {
		super(view,0,0);
		setOpaque(false);
		initProperties();
		label.setHorizontalAlignment(SwingConstants.CENTER);
	}

	@Override
	public void install(BlockComponent panel) {
		super.install(panel);
		// now that blockComponent is set, we can initialize its size
		setSizeFromProperties();
	}
	
	private void initProperties() {
		// To save repeatedly picking through the property list, pull out
		// the ones we are interested in. We listen for changes so
		// we can promptly update the display
		for(BlockProperty property: block.getProperties()) {
			if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_TEXT)) {
				textProperty = property;
				textProperty.addChangeListener(this);
			}
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_WIDTH)) {
				widthProperty = property;
				widthProperty.addChangeListener(this);
			}
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_HEIGHT)) {
				heightProperty = property;
				heightProperty.addChangeListener(this);
			}
		}	
	}

	@Override
	protected void paintComponent(Graphics g) {
		// Get the block properties:
		String text =  textProperty.getValue().toString();
		int height = getHeightPropertyValue();
		int width = getWidthPropertyValue();
		// TODO: get these from block properties
		Border border = new LineBorder(Color.black,1);
		Font font = getFont();
		Color foreground = this.getForeground();
		Color background = this.getBackground();
		
		// Display using the block properties:
		label.setForeground(foreground);
		label.setBackground(background);
		label.setFont(font);
		label.setText(text);
		label.setSize(width, height); // TODO: should be setting this somewhere else than paint method?!
		label.setBorder(border);
		label.validate();
		label.paint(g);
	}

	private Integer getWidthPropertyValue() {
		return Integer.valueOf(widthProperty.getValue().toString());
	}

	private Integer getHeightPropertyValue() {
		return Integer.valueOf(heightProperty.getValue().toString());
	}

	/** Handle a change in properties that would affect the UI. */
	public void stateChanged(ChangeEvent e) {
		setSizeFromProperties();
		blockComponent.invalidate();
		blockComponent.repaint();
	}

	/** Set the visible box's size from the width and height block properties. */
	private void setSizeFromProperties() {
		int height = getHeightPropertyValue();
		int width = getWidthPropertyValue();
		blockComponent.setPreferredSize(new Dimension(width, height));
	}

}