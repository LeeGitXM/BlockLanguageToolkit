package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.UUID;

import javax.swing.Icon;
import javax.swing.JPanel;

import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlock;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.BasicBlockUI;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.AbstractBlock;



public class ProcessBlockView extends AbstractBlock {
	private final UUID uuid;
	private Collection<AnchorDescriptor> anchors;
	private Point location = new Point(0,0);
	private final UI ui ;
	
	public ProcessBlockView() {
		uuid = UUID.randomUUID();
		anchors = new ArrayList<AnchorDescriptor>();
		anchors.add( new AnchorDescriptor(AnchorType.Terminus,"in","IN") );
		anchors.add( new AnchorDescriptor(AnchorType.Origin,"out","OUT") );
		ui = new UI();
	}
	
	public ProcessBlockView(SerializableBlock sb) {
		this.uuid = sb.getId();
		this.anchors = new ArrayList<AnchorDescriptor>();
		for( SerializableAnchor sa:sb.getAnchors() ) {
			anchors.add( new AnchorDescriptor(sa.getType(),sa.getId(),sa.getDisplay()) );
		}
		this.location = sb.getLocation();
		ui = new UI();
	}
	
	@Override
	public Block copy(Map<UUID, UUID> arg0) {
		// TODO Auto-generated method stub
		return null;
	}
	
	public Collection<AnchorDescriptor> getAnchors() { return anchors; }

	@Override
	public Collection<AnchorPoint> getAnchorPoints() {
		return ui.getAnchorPoints();
	}

	@Override
	public AnchorPoint getDefaultDropAnchor() {
		return ui.getAnchorPoints().get(0);
	}

	@Override
	public UUID getId() {
		return uuid;
	}

	// Location is the upper left.
	@Override
	public Point getLocation() {
		return location;
	}

	@Override
	public void initUI(BlockComponent arg) {
		ui.install(arg);
	}

	@Override
	public void setLocation(Point loc) {
		location = loc;
		fireBlockMoved();
		
	}

	private class UI extends BasicBlockUI {
		
		public UI() {
			super(ProcessBlockView.this);
		}

		@Override
		protected Collection<AnchorDescriptor> getAnchors() {
			return anchors;
		}

		@Override
		protected Icon getIcon() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		protected String getTitle() {
			return "BLOCK";
		}

		@Override
		protected boolean isDetailsPanelRequired() {
			// TODO Auto-generated method stub
			return true;
		}
		@Override
		protected void initDetailsPanel(JPanel details) {
			// TODO Auto-generated method stub
			super.initDetailsPanel(details);
		}
	}
}
