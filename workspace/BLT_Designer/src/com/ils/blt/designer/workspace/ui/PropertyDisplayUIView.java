/**
 *  Copyright (c) 2014  ILS Automation. All rights reserved. 
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
import javax.swing.event.ChangeListener;

import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;

/** Draw a free-form text (or html) note in a box. */
@SuppressWarnings("serial")
public class PropertyDisplayUIView extends AbstractUIView implements BlockViewUI, ChangeListener {

	private JLabel label = new JLabel();
	private BlockProperty textProperty;
	private BlockProperty widthProperty;
	private BlockProperty heightProperty;
	private BlockProperty prefixProperty;
	private BlockProperty suffixProperty;
	private BlockProperty backgroundColorProperty;
	
	public PropertyDisplayUIView(ProcessBlockView view) {
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
		// Guarantee that the block has the required properties
		Collection<BlockProperty> properties = block.getProperties(); 
		boolean hasText  = false;
		boolean hasWidth = false;
		boolean hasHeight= false;
		boolean hasPrefix= false;
		boolean hasSuffix= false;
		boolean hasBackgroundColor= false;
		for(BlockProperty property: properties ) {
			if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_TEXT))        hasText = true;
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_WIDTH))  hasWidth= true;
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_HEIGHT)) hasHeight=true;
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_PREFIX)) hasPrefix=true;
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_SUFFIX)) hasSuffix=true;
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_BACKGROUND_COLOR))  hasBackgroundColor= true;
		}
		if(!hasText) properties.add(new BlockProperty(BlockConstants.BLOCK_PROPERTY_TEXT,new BasicQualifiedValue(""),PropertyType.STRING,true));
		if(!hasWidth) properties.add(new BlockProperty(BlockConstants.BLOCK_PROPERTY_WIDTH,new BasicQualifiedValue(new Integer(block.getPreferredWidth())),PropertyType.INTEGER,true));
		if(!hasHeight) properties.add(new BlockProperty(BlockConstants.BLOCK_PROPERTY_HEIGHT,new BasicQualifiedValue(new Integer(block.getPreferredHeight())),PropertyType.INTEGER,true));
		if(!hasPrefix) properties.add(new BlockProperty(BlockConstants.BLOCK_PROPERTY_PREFIX,new BasicQualifiedValue(""),PropertyType.STRING,true));
		if(!hasSuffix) properties.add(new BlockProperty(BlockConstants.BLOCK_PROPERTY_SUFFIX,new BasicQualifiedValue(""),PropertyType.STRING,true));
		if(!hasBackgroundColor) properties.add(new BlockProperty(BlockConstants.BLOCK_PROPERTY_BACKGROUND_COLOR,new BasicQualifiedValue(new Integer(block.getBackgroundColor())),PropertyType.COLOR,true));
	
		// To save repeatedly picking through the property list (we already did it once), pull out
		// the ones we are interested in. We listen for changes so we can promptly update the display
		for(BlockProperty property: properties ) {
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
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_PREFIX)) {
				prefixProperty = property;
				prefixProperty.addChangeListener(this);
			}
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_SUFFIX)) {
				suffixProperty = property;
				suffixProperty.addChangeListener(this);
			}
			else if(property.getName().equals(BlockConstants.BLOCK_PROPERTY_BACKGROUND_COLOR)) {
				backgroundColorProperty = property;
				backgroundColorProperty.addChangeListener(this);
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
//		Border border = new LineBorder(Color.black,1);
		Font font = getFont();
		Color foreground = this.getForeground();
		Color background = this.getBackground();
		Boolean opaque = false;
		String prefix = prefixProperty.getValue().toString();
		String suffix = suffixProperty.getValue().toString();

		String selectedBackgroundName = (String)backgroundColorProperty.getValue();
		
		if (selectedBackgroundName != null) {
			Color selectedBackground = null;
		    try {
		        selectedBackground = (Color)Color.class.getField(selectedBackgroundName.toUpperCase()).get(null);
		    } catch (IllegalArgumentException | IllegalAccessException | NoSuchFieldException | SecurityException e) {
				selectedBackground = null;
		    }
			if (selectedBackground != null) {
				background = selectedBackground;
			}
			background = selectedBackground;
			opaque = true;
		}
		
		final String finText = "<html>" + prefix + text + suffix + "</html>";
		
		// Display using the block properties:
		label.setForeground(foreground);
		label.setBackground(background);
		label.setOpaque(opaque);
		label.setFont(font);
		label.setText(finText);
		label.setSize(width, height); // TODO: should be setting this somewhere else than paint method?!
//		label.setBorder(border);
		label.validate();
		label.paint(g);
	}

	private Integer getWidthPropertyValue() {
		return Integer.valueOf(widthProperty.getValue().toString());
	}

	// NOTE: We rely on the editor to disallow invalid values.
	private Integer getHeightPropertyValue() {
		return Integer.valueOf(heightProperty.getValue().toString());
	}

	/** Handle a change in properties that would affect the UI. */
	public void stateChanged(ChangeEvent e) {
		setSizeFromProperties();
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				blockComponent.invalidate();
				blockComponent.repaint();
			}
		});
	}

	/** Set the visible box's size from the width and height block properties. */
	private void setSizeFromProperties() {
		int height = getHeightPropertyValue();
		int width = getWidthPropertyValue();
		blockComponent.setPreferredSize(new Dimension(width, height));
	}

}
