package com.ils.blt.designer.workspace.ui;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.images.ImageLoader;


/**
 * Create a circular "button" with a predefined 48x48 graphic. The first input anchor
 * creates an anchor point on the left. The first output anchor point creates an 
 * anchor point on the top.
 */
public class RoundUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 2190868310475735865L;
	private Icon icon = null;
	private static final int DEFAULT_HEIGHT = 58;
	private static final int DEFAULT_WIDTH  = 58;

	
	public RoundUIView(ProcessBlockView view) {
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		setOpaque(false);
		try {
			// Image is smaller to account for stubs
			Dimension IMAGE_SIZE = new Dimension(DEFAULT_WIDTH-10,DEFAULT_HEIGHT-10);
			Image img = ImageLoader.getInstance().loadImage("Block/icons/medium/round.png",IMAGE_SIZE);
			if( img !=null) icon = new ImageIcon(img);
		}
		catch(IllegalArgumentException iae) {     // Always get an error when in the migration tool
			log.infof("RoundUIView: Exception loading image (%s)",iae.getLocalizedMessage());
		}
		initAnchorPoints();
	}
	

	@Override
	protected void paintComponent(Graphics _g) {
		Graphics2D g = (Graphics2D)_g;
		if( icon!=null ) {
			icon.paintIcon(getBlockComponent(), g, INSET, INSET);
		}
		drawAnchors(g,0,0);
		drawBadges(g);
	}

}
