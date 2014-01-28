package com.ils.blt.designer.workspace;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.UUID;

import com.ils.block.common.AnchorDirection;
import com.ils.block.common.AnchorPrototype;
import com.ils.block.common.BlockDescriptor;
import com.ils.block.common.BlockProperty;
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
	private String embeddedIcon="";               // 32x32 icon to place in block in designer
	private String embeddedLabel="";              // Label place in block in designer
	private String label;                         // Text to display on the block
	private BlockState state = BlockState.IDLE;   // Block execution state
	private String statusText;                    // Auxiliary text to display
	private Collection<AnchorDescriptor> anchors;
	private Collection<BlockProperty> properties;
	private Point location = new Point(0,0);
	private final UIFactory factory = new UIFactory() ;
	private BlockViewUI ui = null;
	private BlockStyle style = BlockStyle.BASIC;
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	
	/**
	 * Constructor: Used when a new block is created from the palette.
	 */
	public ProcessBlockView(BlockDescriptor descriptor) {
		uuid = UUID.randomUUID();
		this.className = descriptor.getBlockClass();
		this.label = descriptor.getLabel();
		this.embeddedIcon = descriptor.getEmbeddedIcon();
		this.embeddedLabel= descriptor.getEmbeddedLabel();
		this.state = BlockState.STOPPED;
		this.statusText = "";
		this.style = descriptor.getStyle();

		this.anchors = new ArrayList<AnchorDescriptor>();
		for( AnchorPrototype ad:descriptor.getAnchors() ) {
			log.infof("%s: Creating anchor descriptor %s", TAG,ad.getName());
			anchors.add( new AnchorDescriptor((ad.getAnchorDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
					UUID.randomUUID(),ad.getName()) );
		}
		this.properties = new ArrayList<BlockProperty>();
		log.infof("%s: Created %s (%s) view from descriptor (%d anchors)", TAG, className, style.toString(),anchors.size());
	}
	
	public ProcessBlockView(SerializableBlock sb) {
		this.uuid = sb.getId();
		this.className = sb.getClassName();
		this.embeddedIcon = sb.getEmbeddedIcon();
		this.embeddedLabel= sb.getEmbeddedLabel();
		this.style = sb.getStyle();
		this.label = sb.getLabel();
		this.state = BlockState.PAUSED;
		this.statusText = sb.getStatusText();
		this.anchors = new ArrayList<AnchorDescriptor>();
		if(sb.getAnchors()!=null ) {
			for( SerializableAnchor sa:sb.getAnchors() ) {
				log.infof("%s: Creating anchor view %s", TAG,sa.getDisplay());
				anchors.add( new AnchorDescriptor((sa.getDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),sa.getId(),sa.getDisplay()) );
			}
		}
		this.properties = new ArrayList<BlockProperty>();
		if(sb.getProperties()!=null ) {
			for(BlockProperty bp:sb.getProperties()) {
				properties.add(bp);
			} 
		}
		this.location = new Point(sb.getX(),sb.getY());
		log.infof("%s: Created %s (%s) view from serializable block", TAG, className, style.toString());
	}
	
	@Override
	public Block copy(Map<UUID, UUID> arg0) {
		log.infof("%s: copy ...", TAG);
		return null;
	}
	
	public Collection<AnchorDescriptor> getAnchors() { return anchors; }
	public Collection<BlockProperty> getProperties() { return properties; }
	public void setProperties(Collection<BlockProperty> props) { this.properties = props; }
	public String getClassName() { return className; }

	@Override
	public Collection<AnchorPoint> getAnchorPoints() {
		return ui.getAnchorPoints();
	}

	/** Simply return the first in the list. */
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
	public void setLocation(Point loc) {
		location = loc;
		fireBlockMoved();
	}

	public String getLabel() {return label;}
	public void setLabel(String label) {this.label = label;}
	public String getStatusText() { return statusText; }
	public void setStatusText(String statusText) { this.statusText = statusText; }
	public BlockStyle getStyle() { return style; }
	public void setStyle(BlockStyle s) { this.style = s; }
	public String getEmbeddedIcon() {return embeddedIcon;}
	public void setEmbeddedIcon(String embeddedIcon) {this.embeddedIcon = embeddedIcon;}
	public String getEmbeddedLabel() {return embeddedLabel;}
	public void setEmbeddedLabel(String embeddedLabel) {this.embeddedLabel = embeddedLabel;}

	@Override
	public void initUI(BlockComponent blk) {
		log.debugf("%s: initUI", TAG);
		ui = factory.getUI(style, this);
		ui.install(blk);
	}
}
