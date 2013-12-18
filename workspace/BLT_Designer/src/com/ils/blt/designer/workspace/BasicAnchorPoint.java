package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.awt.Shape;
import java.util.EnumSet;

import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;

/**
 * Use this class for all anchor points on ProcessDiagramViews. In addition to 
 * the standard attributes, this anchor point adds data type.
 */
public class BasicAnchorPoint extends AnchorPoint {
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
