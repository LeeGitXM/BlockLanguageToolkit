package com.ils.blt.designer.workspace;

import java.awt.Color;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;

import com.ils.block.common.AnchorDirection;
import com.ils.block.common.AnchorPrototype;
import com.ils.block.common.BlockDescriptor;
import com.ils.block.common.BlockProperty;
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
	private final static Random random = new Random();
	private Collection<ProcessAnchorDescriptor> anchors;
	private int background = Color.white.getRGB();
	private final String className;
	private int    embeddedFontSize = 24;         // Size of font for interior label
	private String embeddedIcon="";               // 32x32 icon to place in block in designer
	private String embeddedLabel="";              // Label place in block in designer
	private final UIFactory factory = new UIFactory() ;
	private String iconPath="";                   // Path to icon that is the entire block
	private String name = null;                         // Text to display on the block
	private Point location = new Point(0,0);
	private final LoggerEx log = LogUtil.getLogger(getClass().getPackage().getName());
	private int preferredHeight = 0;              // Size the view to "natural" size
	private int preferredWidth  = 0;              // Size the view to "natural" size
	private Collection<BlockProperty> properties;
	private boolean receiveEnabled = false;
	private String statusText;                    // Auxiliary text to display
	private BlockStyle style = BlockStyle.SQUARE;
	private boolean transmitEnabled = false;
	private BlockViewUI ui = null;
	private final UUID uuid;
	
	
	/**
	 * Constructor: Used when a new block is created from the palette. 
	 *              Create a pseudo-random name.
	 */
	public ProcessBlockView(BlockDescriptor descriptor) {
		this.uuid = UUID.randomUUID();
		this.background = descriptor.getBackground();
		this.className = descriptor.getBlockClass();
		this.embeddedIcon = descriptor.getEmbeddedIcon();
		this.embeddedLabel= descriptor.getEmbeddedLabel();
		this.embeddedFontSize= descriptor.getEmbeddedFontSize();
		this.iconPath = descriptor.getIconPath();
		this.preferredHeight = descriptor.getPreferredHeight();
		this.preferredWidth = descriptor.getPreferredWidth();
		this.statusText = "";
		this.style = descriptor.getStyle();
		this.receiveEnabled = descriptor.isReceiveEnabled();
		this.transmitEnabled= descriptor.isTransmitEnabled();

		this.anchors = new ArrayList<ProcessAnchorDescriptor>();
		for( AnchorPrototype ap:descriptor.getAnchors() ) {
			log.debugf("%s: Creating anchor descriptor %s", TAG,ap.getName());
			anchors.add( new ProcessAnchorDescriptor((ap.getAnchorDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
					ap.getConnectionType(),UUID.randomUUID(),ap.getName()) );
		}
		this.properties = new ArrayList<BlockProperty>();
		log.debugf("%s: Created %s (%s) view from descriptor (%d anchors)", TAG, className, style.toString(),anchors.size());
		createPseudoRandomName();
	}
	
	public ProcessBlockView(SerializableBlock sb) {
		this.uuid = sb.getId();
		this.background = sb.getBackground();
		this.className = sb.getClassName();
		this.embeddedIcon = sb.getEmbeddedIcon();
		this.embeddedLabel= sb.getEmbeddedLabel();
		this.embeddedFontSize = sb.getEmbeddedFontSize();
		this.iconPath = sb.getIconPath();
		this.preferredHeight = sb.getPreferredHeight();
		this.preferredWidth = sb.getPreferredWidth();
		this.style = sb.getStyle();
		this.name = sb.getName();
		this.statusText = sb.getStatusText();
		this.receiveEnabled  = sb.isReceiveEnabled();
		this.transmitEnabled = sb.isTransmitEnabled();
		this.anchors = new ArrayList<ProcessAnchorDescriptor>();
		if(sb.getAnchors()!=null ) {
			for( SerializableAnchor sa:sb.getAnchors() ) {
				log.debugf("%s: Creating anchor view %s", TAG,sa.getDisplay());
				anchors.add( new ProcessAnchorDescriptor((sa.getDirection()==AnchorDirection.INCOMING?AnchorType.Terminus:AnchorType.Origin),
						sa.getConnectionType(),sa.getId(),sa.getDisplay()) );
			}
		}
		this.properties = new ArrayList<BlockProperty>();
		if(sb.getProperties()!=null ) {
			for(BlockProperty bp:sb.getProperties()) {
				properties.add(bp);
			} 
		}
		this.location = new Point(sb.getX(),sb.getY());
		log.debugf("%s: Created %s %s (%s) view from serializable block", TAG, className, sb.getId().toString(),style.toString());
	}
	
	// Note: This does not set connection type
	private SerializableAnchor convertAnchorToSerializable(ProcessAnchorDescriptor anchor) {
		SerializableAnchor result = new SerializableAnchor();
		result.setDirection(anchor.getType()==AnchorType.Origin?AnchorDirection.OUTGOING:AnchorDirection.INCOMING);
		result.setDisplay(anchor.getDisplay());
		result.setId(anchor.getId());
		result.setParentId(getId());
		result.setConnectionType(anchor.getConnectionType());
		return result;
	}
	
    public SerializableBlock convertToSerializable() {
		SerializableBlock result = new SerializableBlock();
		result.setId(getId());
		result.setClassName(getClassName());
		result.setEmbeddedIcon(getEmbeddedIcon());
		result.setEmbeddedLabel(getEmbeddedLabel());
		result.setEmbeddedFontSize(getEmbeddedFontSize());
		result.setIconPath(getIconPath());
		result.setName(getName());
		result.setStyle(getStyle());
		result.setReceiveEnabled(isReceiveEnabled());
		result.setTransmitEnabled(isTransmitEnabled());
		result.setX(getLocation().x);
		result.setY(getLocation().y);
		
		List<SerializableAnchor> anchors = new ArrayList<SerializableAnchor>();
		for( AnchorDescriptor anchor:getAnchors()) {
			anchors.add(convertAnchorToSerializable((ProcessAnchorDescriptor)anchor));
		}
		result.setAnchors(anchors.toArray(new SerializableAnchor[anchors.size()]));
		if( getProperties()!=null ) {
			log.tracef("%s.convertToSerializable: %s has %d properties",TAG,getClassName(),getProperties().size());
			log.info(getProperties().toString());
			result.setProperties(getProperties().toArray(new BlockProperty[getProperties().size()]));
		}
		else {
			log.warnf("%s.convertToSerializable: %s has no properties",TAG,getClassName());
		}
		
		return result;
	}
	@Override
	public Block copy(Map<UUID, UUID> arg0) {
		log.infof("%s: copy ...", TAG);
		return null;
	}
	@Override
	public Collection<AnchorPoint> getAnchorPoints() {
		if( ui==null ) ui = factory.getUI(style, this);
		return ui.getAnchorPoints();	
	}
	public Collection<ProcessAnchorDescriptor> getAnchors() { return anchors; }
	public int getBackground() { return background;}
	public String getClassName() { return className; }

	/** Do not define a default. Rely on drop targets. */
	@Override
	public AnchorPoint getDefaultDropAnchor() {return null;}
	public int getEmbeddedFontSize() {return embeddedFontSize;}
	public String getEmbeddedIcon() {return embeddedIcon;}
	public String getEmbeddedLabel() {return embeddedLabel;}
	public String getIconPath() {return iconPath;}
	@Override
	public UUID getId() { return uuid; }
	public String getName() {return name;}
	// Location is the upper left.
	@Override
	public Point getLocation() {
		return location;
	}
	public int getPreferredHeight() {return preferredHeight;}
	public int getPreferredWidth() {return preferredWidth;}
	public Collection<BlockProperty> getProperties() { return properties; }
	public String getStatusText() { return statusText; }
	public BlockStyle getStyle() { return style; }
	@Override
	public void initUI(BlockComponent blk) {
		ui = factory.getUI(style, this);
		ui.install(blk);
	}
	public boolean isReceiveEnabled() {return receiveEnabled;}
	public boolean isTransmitEnabled() {return transmitEnabled;}
	public void setEmbeddedFontSize(int size) {this.embeddedFontSize = size;}
	public void setEmbeddedIcon(String embeddedIcon) {this.embeddedIcon = embeddedIcon;}
	public void setEmbeddedLabel(String embeddedLabel) {this.embeddedLabel = embeddedLabel;}
	public void setIconPath(String iconPath) {this.iconPath = iconPath;}
	public void setName(String label) {this.name = label;}
	@Override
	public void setLocation(Point loc) {
		location = loc;
		fireBlockMoved();
	}
	public void setPreferredHeight(int preferredHeight) {this.preferredHeight = preferredHeight;}
	public void setPreferredWidth(int preferredWidth) {this.preferredWidth = preferredWidth;}
	public void setProperties(Collection<BlockProperty> props) { this.properties = props; }
	public void setReceiveEnabled(boolean receiveEnabled) {this.receiveEnabled = receiveEnabled;}
	public void setBackground(int b)  { this.background = b; }
	public void setStatusText(String statusText) { this.statusText = statusText; }
	public void setStyle(BlockStyle s) { this.style = s; }
	public void setTransmitEnabled(boolean transmitEnabled) {this.transmitEnabled = transmitEnabled;}
	
	/**
	 * Create a name that is highly likely to be unique within the diagram.
	 * The name can be user-modified at any time. If we really need a uniqueness,
	 * use the block's UUID.
	 */
	private void createPseudoRandomName() {
		String root = className;
		int pos = className.lastIndexOf(".");
		if( pos>=0 )  root = className.substring(pos+1);
		name = String.format("%s-%d", root.toUpperCase(),random.nextInt(1000));
	}
}
