package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.UUID;

import com.ils.block.common.AnchorPrototype;
import com.ils.block.common.AnchorDirection;
import com.ils.block.common.BlockDescription;
import com.ils.block.common.BlockState;
import com.ils.block.common.BlockStyle;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.designer.workspace.ui.BlockViewUI;
import com.ils.blt.designer.workspace.ui.UIFactory;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.BlockComponent;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
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
	private final String className;
	private String label;                          // Text to display on the block
	private BlockState state = BlockState.IDLE;   // Block execution state
	private String statusText;                    // Auxiliary text to display
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
		this.className = descriptor.getBlockClass();
		this.label = descriptor.getLabel();
		this.style = descriptor.getStyle();
		this.anchors = new ArrayList<AnchorDescriptor>();
		for( AnchorPrototype ad:descriptor.getAnchors() ) {
			log.infof("%s: Creating anchor descriptor %s", TAG,ad.getName());
			anchors.add( new AnchorDescriptor((ad.getAnchorDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
					UUID.randomUUID(),ad.getName()) );
		}
		log.infof("%s: Created view by descriptor (%d anchors)", TAG, anchors.size());
	}
	
	public ProcessBlockView(SerializableBlock sb) {
		this.uuid = sb.getId();
		this.className = sb.getClassName();
		this.anchors = new ArrayList<AnchorDescriptor>();
		for( SerializableAnchor sa:sb.getAnchors() ) {
			log.infof("%s: Creating anchor view %s", TAG,sa.getDisplay());
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
	public String getClassName() { return className; }

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

	public String getLabel() {return label;}
	public void setLabel(String label) {this.label = label;}
	public String getStatusText() { return statusText; }
	public void setStatusText(String statusText) { this.statusText = statusText; }

	@Override
	public void initUI(BlockComponent blk) {
		log.debugf("%s: initUI", TAG);
		ui = factory.getUI(style, this);
		ui.install(blk);
	}

	@Override
	public void setLocation(Point loc) {
		location = loc;
		fireBlockMoved();
		
	}
}
