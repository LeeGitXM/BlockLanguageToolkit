package com.ils.blt.designer.workspace.ui;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.Icon;

import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.gui.IconUtil;


/**
 * Create a circular "button" with a predefined 48x48 graphic. The first input anchor
 * creates an anchor point on the left. The first output anchor point creates an 
 * anchor point on the top.
 */
public class RoundUIView extends AbstractUIView implements BlockViewUI {
	private static final long serialVersionUID = 2130868310475735865L;
	private Icon icon = null;
	
	public RoundUIView(ProcessBlockView view) {
		super(view);
		setOpaque(false);
		setPreferredSize(new Dimension(68,68));   // 48 plus 10 for stubs
		icon = IconUtil.getRootIcon(RoundUIView.class, "entry_48.png");
		initAnchorPoints();
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
