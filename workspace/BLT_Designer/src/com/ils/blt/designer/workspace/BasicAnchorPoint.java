package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.awt.Shape;
import java.util.EnumSet;

import com.ils.blt.designer.workspace.ui.AnchorSide;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;

/**
 * Use this class to create anchor points on ProcessDiagramViews. In addition to 
 * the standard attributes, this anchor point adds side, data type.
 */
public class BasicAnchorPoint extends AnchorPoint {
	private final Point anchor;
	private final Point pathLeader;
	private final Shape hotspot;
	private AnchorSide side;
	
	
	public BasicAnchorPoint(Object id, Block block, AnchorType type,Point anch, Point leader, Shape spot) {
		super(id, block, EnumSet.of(type));
		anchor = anch;
		pathLeader = leader;
		hotspot = spot;
		// Default behavior for side. This can be updated.
		if( type==AnchorType.Origin) side = AnchorSide.RIGHT;
		else side = AnchorSide.LEFT;	
	}

	public Point getAnchor() { return anchor; }
	public Point getPathLeader() { return pathLeader; }
	public Shape getHotSpot() { return hotspot; }
	public AnchorSide getSide() {return side;}
	public void setSide(AnchorSide side) {this.side = side;}
}
