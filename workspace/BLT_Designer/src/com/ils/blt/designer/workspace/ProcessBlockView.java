package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.UUID;

import com.ils.block.common.BlockStyle;
import com.ils.block.common.BlockDescription;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.designer.workspace.ui.BlockViewUI;
import com.ils.blt.designer.workspace.ui.UIFactory;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.AbstractBlock;

/**
 * This is the class that describes all blocks that appear in a
 * diagram in the Designer. Different block shapes and characteristics
 * are provided by swapping out different UI rendering classes.
 */

public class ProcessBlockView extends AbstractBlock {
	private static final String TAG = "ProcessBlockView";
	private final UUID uuid;
	private Collection<AnchorDescriptor> anchors;
	private Point location = new Point(0,0);
	private final UIFactory factory = new UIFactory() ;
	private BlockViewUI ui = null;
	private BlockStyle style = BlockStyle.BASIC;
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
	/**
	 * Constructor: Used when a new block is created from the palette.
	 */
	public ProcessBlockView(BlockDescription descriptor) {
		uuid = UUID.randomUUID();
		this.style = descriptor.getStyle();
		this.anchors = new ArrayList<AnchorDescriptor>();
		log.infof("%s: Created view by descriptor", TAG);
	}
	
	public ProcessBlockView(SerializableBlock sb) {
		this.uuid = sb.getId();
		this.anchors = new ArrayList<AnchorDescriptor>();
		for( SerializableAnchor sa:sb.getAnchors() ) {
			anchors.add( new AnchorDescriptor(sa.getType(),sa.getId(),sa.getDisplay()) );
		}
		this.location = sb.getLocation();
		log.infof("%s: Created view by serializable block", TAG);
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
		return ui.getAnchorPoints().iterator().next();
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
	public void initUI(BlockComponent blk) {
		log.infof("%s: initUI", TAG);
		ui = factory.getUI(style, this);
		log.infof("%s: installUI", TAG);
		ui.install(blk);
	}

	@Override
	public void setLocation(Point loc) {
		location = loc;
		fireBlockMoved();
		
	}
}
