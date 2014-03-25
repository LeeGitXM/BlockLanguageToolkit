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
import com.ils.blt.designer.workspace.ProcessAnchorDescriptor;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.connection.ConnectionType;
import com.inductiveautomation.ignition.client.images.ImageLoader;
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
		super(view,DEFAULT_WIDTH,DEFAULT_HEIGHT);
		setOpaque(false);
		int preferredHeight = view.getPreferredHeight();
		if( preferredHeight<=0 ) preferredHeight = DEFAULT_HEIGHT;
		int preferredWidth = view.getPreferredWidth();
		if( preferredWidth<=0 ) preferredWidth = DEFAULT_WIDTH;
		Dimension imageSize = new Dimension(preferredWidth,preferredHeight-2*INSET);
		// Only consider an inset on the height, not width
		setPreferredSize(new Dimension(preferredWidth,preferredHeight)); 
		String iconPath = view.getIconPath();
		if( iconPath==null || iconPath.length()==0 ) iconPath = DEFAULT_ICON_PATH;
		
		Image img = ImageLoader.getInstance().loadImage(iconPath ,imageSize);
		if( img !=null) icon = new ImageIcon(img);
		initAnchorPoints();
	}

	/**
	 *  Create a single anchor point on the top for data, if defined. 
	 *  Create a single anchor on the bottom for a signal (either direction).
	 *  The icon will be centered.
	 */
	protected void initAnchorPoints() {
		Dimension sz = getPreferredSize();
		for(ProcessAnchorDescriptor desc:getBlock().getAnchors()) {
			if( desc.getType()==AnchorType.Terminus && desc.getConnectionType()!=ConnectionType.SIGNAL) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),getBlock(),AnchorType.Terminus,
						desc.getConnectionType(),
						new Point(sz.width/2,INSET),
						new Point(sz.width/2,-LEADER_LENGTH),
						new Rectangle(sz.width/2-INSET,0,2*INSET,2*INSET));
				ap.setSide(AnchorSide.TOP);
				getAnchorPoints().add(ap);
				break;     // We  only allow one
			}
			else if( desc.getConnectionType()==ConnectionType.SIGNAL) {
				BasicAnchorPoint ap = new BasicAnchorPoint(desc.getDisplay(),getBlock(),desc.getType(),
						desc.getConnectionType(),
						new Point(sz.width/2,sz.height-INSET),
						new Point(sz.width/2,sz.height+SIGNAL_LEADER_LENGTH),
						new Rectangle(sz.width/2-INSET,sz.height-2*INSET,2*INSET,2*INSET));
				ap.setSide(AnchorSide.BOTTOM);
				getAnchorPoints().add(ap);
				break;     // We only allow one
			}
		}
	}
	@Override
	protected void paintComponent(Graphics _g) {
		Graphics2D g = (Graphics2D)_g;
		if( icon!=null ) {
			icon.paintIcon(getBlockComponent(), g, 0, INSET);
		}
		drawAnchors(g);
	}
}
