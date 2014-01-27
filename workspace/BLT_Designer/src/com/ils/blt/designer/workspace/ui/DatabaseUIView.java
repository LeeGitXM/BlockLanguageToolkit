package com.ils.blt.designer.workspace.ui;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.gui.IconUtil;

/**
 * Create a drum with a predefined 48x48 graphic. The first input anchor
 * creates an anchor point on the top. Any others are ignored.
 */
public class DatabaseUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 7095402409706582432L;
	private static final Dimension IMAGE_SIZE = new Dimension(48,48);
	private Icon icon = null;
	
	public DatabaseUIView(ProcessBlockView view) {
		super(view);
		setOpaque(false);
		setPreferredSize(new Dimension(58,58));   // 48 plus 10 for stub
		Image img = ImageLoader.getInstance().loadImage("Block/icons/48/database_48.png",IMAGE_SIZE);
		if( img !=null) icon = new ImageIcon(img);
		initAnchorPoints();
	}

	/**
	 *  Create a single anchor point on the top.
	 */
	protected void initAnchorPoints() {
		Dimension sz = getPreferredSize();
		int offset = ANCHOR_SIZE/2;
		for(AnchorDescriptor desc:getBlock().getAnchors()) {
			if( desc.getType()==AnchorType.Terminus) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),getBlock(),AnchorType.Terminus,
						new Point(sz.width/2 - offset,INSET-offset),
						new Point(sz.width/2 - offset,-10),
						new Rectangle((sz.width-offset -INSET)/2,INSET/2-offset,INSET,INSET));
				getAnchorPoints().add(ap);
				break;
			}
		}
	}
	@Override
	protected void paintComponent(Graphics _g) {
		Graphics2D g = (Graphics2D)_g;
		if( icon!=null ) {
			icon.paintIcon(getBlockComponent(), g, INSET/2, INSET);
		}
		drawAnchors(g);
	}
}
