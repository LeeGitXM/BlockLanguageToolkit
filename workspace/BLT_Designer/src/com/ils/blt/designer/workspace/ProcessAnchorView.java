package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.EnumSet;

import com.ils.block.common.AnchorDirection;
import com.ils.blt.common.serializable.SerializableAnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;



public class ProcessAnchorView extends AnchorPoint {
	private Point anchor = null;
	private Shape hotSpot = null;
	private Point pathLeader = null;
	
	public ProcessAnchorView(Object id, ProcessBlockView block,AnchorType type) {
		super(id,block,EnumSet.of(type));
	}
	
	public ProcessAnchorView(ProcessBlockView block,SerializableAnchorPoint sap) {
		super(sap.getId(),block,EnumSet.of(sap.getDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin));
		this.hotSpot = new Rectangle(sap.getHotSpotX(),sap.getHotSpotY(),sap.getHotSpotWidth(),sap.getHotSpotHeight());
		this.anchor = new Point(sap.getAnchorX(),sap.getAnchorY());
		this.pathLeader = new Point(sap.getPathLeaderX(),sap.getPathLeaderY());		
	}

	@Override
	public Point getAnchor() {
		return anchor;
	}

	@Override
	public Shape getHotSpot() {
		return hotSpot;
	}

	@Override
	public Point getPathLeader() {
		return pathLeader;
	}
	

}
