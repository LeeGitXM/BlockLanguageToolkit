package com.ils.blt.designer.workspace.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JComponent;

import com.ils.blt.designer.workspace.BasicAnchorPoint;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;



public class TagwriterUIView extends JComponent implements BlockViewUI {

	private final List<BasicAnchorPoint> anchorPoints;
	
	public TagwriterUIView(ProcessBlockView view) {
		setOpaque(false);
		setPreferredSize(new Dimension(100,100));
		anchorPoints = new ArrayList<BasicAnchorPoint>();
		BasicAnchorPoint ap = new BasicAnchorPoint("out",view,AnchorType.Origin,new Point(95,50),new Point(110,50),new Rectangle(90,45,10,10));
		anchorPoints.add(ap);
		
	}

	@Override
	public void install(BlockComponent panel) {
		panel.setLayout(new BorderLayout());
		panel.add(this,BorderLayout.CENTER);
	}

	@Override
	protected void paintComponent(Graphics _g) {
		Graphics2D g = (Graphics2D)_g;
		g.setColor(Color.green);
		g.fillOval(0, 0, 100, 100);
		g.setColor(Color.black);
		g.fillRect(90, 45, 10,10);
	}

	@Override
	public Collection<AnchorDescriptor> getAnchors() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<AnchorPoint> getAnchorPoints() {
		// TODO Auto-generated method stub
		return null;
	}

}
