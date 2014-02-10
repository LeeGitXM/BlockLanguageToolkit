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

/**
 * Create a drum with a predefined 48x48 graphic. The first input anchor
 * creates an anchor point on the top. Any others are ignored.
 */
public class IconUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 7095402409706582432L;
	private static final int DEFAULT_HEIGHT = 48;
	private static final int DEFAULT_WIDTH  = 48;
	private static final String DEFAULT_ICON_PATH  = "Block/icons/medium/database.png";
	private Icon icon = null;
	
	public IconUIView(ProcessBlockView view) {
		super(view);
		setOpaque(false);
		int preferredHeight = view.getPreferredHeight();
		if( preferredHeight<=0 ) preferredHeight = DEFAULT_HEIGHT;
		int preferredWidth = view.getPreferredWidth();
		if( preferredWidth<=0 ) preferredWidth = DEFAULT_WIDTH;
		Dimension imageSize = new Dimension(preferredWidth,preferredHeight);
		setPreferredSize(new Dimension(preferredWidth+INSET,preferredHeight+INSET));   // 48 plus INSET
		String iconPath = view.getIconPath();
		if( iconPath==null || iconPath.length()==0 ) iconPath = DEFAULT_ICON_PATH;
		
		Image img = ImageLoader.getInstance().loadImage(iconPath ,imageSize);
		if( img !=null) icon = new ImageIcon(img);
		initAnchorPoints();
	}

	/**
	 *  Create a single anchor point on the top.
	 */
	protected void initAnchorPoints() {
		Dimension sz = getPreferredSize();
		for(AnchorDescriptor desc:getBlock().getAnchors()) {
			if( desc.getType()==AnchorType.Terminus) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),getBlock(),AnchorType.Terminus,
						new Point(sz.width/2,INSET),
						new Point(sz.width/2,-LEADER_LENGTH),
						new Rectangle((sz.width-INSET)/2,INSET/2,2*INSET,2*INSET));
				ap.setSide(AnchorSide.TOP);
				getAnchorPoints().add(ap);
				break;
			}
		}
	}
	@Override
	protected void paintComponent(Graphics _g) {
		Graphics2D g = (Graphics2D)_g;
		if( icon!=null ) {
			icon.paintIcon(getBlockComponent(), g, INSET, INSET);
		}
		drawAnchors(g);
	}
}
