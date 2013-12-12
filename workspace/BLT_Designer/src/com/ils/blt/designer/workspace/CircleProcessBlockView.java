package com.ils.blt.designer.workspace;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;

import javax.swing.JComponent;

import com.ils.blt.common.serializable.SerializableBlock;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;



public class CircleProcessBlockView extends ProcessBlockView {
	private CircleUI ui ;
	private List<AnchorPoint> anchorPoints;
	
	public CircleProcessBlockView() {
		init();
	}
	
	public CircleProcessBlockView(SerializableBlock sb) {
		super(sb);
		init();
	}
	
	private void init() {
		ui = new CircleUI();
		anchorPoints = new ArrayList<AnchorPoint>();
		anchorPoints.add(new BasicAnchorPoint("out", this, AnchorType.Origin, new Point(95,50), new Point(110,50), new Rectangle(90,45,10,10)));
	}
	@Override
	public void initUI(BlockComponent panel) {
		panel.setLayout(new BorderLayout());
		panel.add(ui,BorderLayout.CENTER);
	}

	@Override
	public Collection<AnchorPoint> getAnchorPoints() {
		return anchorPoints;
	}
	
	private class CircleUI extends JComponent {
		public CircleUI() {
			setOpaque(false);
			setPreferredSize(new Dimension(100,100));
		}
		@Override
		protected void paintComponent(Graphics _g) {
			Graphics2D g = (Graphics2D)_g;
			g.setColor(Color.green);
			g.fillOval(0, 0, 100, 100);
			g.setColor(Color.black);
			g.fillRect(90, 45, 10,10);
		}
		
	}
	
	private class BasicAnchorPoint extends AnchorPoint {
		private final Point anchor;
		private final Point pathLeader;
		private final Shape hotspot;
		
		public BasicAnchorPoint(Object id, Block block, AnchorType type,Point anch, Point path, Shape spot) {
			super(id, block, EnumSet.of(type));
			anchor = anch;
			pathLeader = path;
			hotspot = spot;
			
		}

		public Point getAnchor() {
			return anchor;
		}

		public Point getPathLeader() {
			return pathLeader;
		}

		public Shape getHotSpot() {
			return hotspot;
		}
		

	}
}
