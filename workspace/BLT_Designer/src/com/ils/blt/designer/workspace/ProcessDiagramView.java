package com.ils.blt.designer.workspace;

import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.common.AnchorDirection;
import com.ils.block.common.BlockProperty;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableAnchorPoint;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.inductiveautomation.ignition.common.util.AbstractChangeable;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.blockui.AnchorDescriptor;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorType;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.LookupConnection;

/**
 * This class represents a diagram in the designer.
 */
public class ProcessDiagramView extends AbstractChangeable implements BlockDiagramModel {
	private static final String TAG = "DiagnosticsWorkspace";
	private static LoggerEx log = LogUtil.getLogger(ProcessDiagramView.class.getPackage().getName());
	private Map<UUID,ProcessBlockView> blockMap = new HashMap<UUID,ProcessBlockView>();
	private List<Connection> connections = new ArrayList<Connection>();
	private Dimension diagramSize = new Dimension(800,600);
	private final long resourceId;
	private final String name;
	
	public ProcessDiagramView(long resId,String nam) {
		this.resourceId = resId;
		this.name = nam;
	}
	
	public static ProcessDiagramView createDiagramView(long resid,SerializableDiagram diagram) {
		ProcessDiagramView diagramView = new ProcessDiagramView(resid,diagram.getName());
		HashMap<UUID,ProcessBlockView> blockMap = new HashMap<UUID,ProcessBlockView>();

		for( SerializableBlock sb:diagram.getBlocks()) {
			ProcessBlockView pbv = new ProcessBlockView(sb);
			blockMap.put(sb.getId(), pbv);
			diagramView.addBlock(pbv);
		}

		for( SerializableConnection scxn:diagram.getConnections() ) {
			SerializableAnchorPoint a = scxn.getBeginAnchor();
			SerializableAnchorPoint b = scxn.getEndAnchor();
			if( a!=null && b!=null ) {
				ProcessBlockView blocka = blockMap.get(a.getParentId());
				ProcessBlockView blockb = blockMap.get(b.getParentId());
				if( blocka!=null && blockb!=null) {
					AnchorPoint origin = new ProcessAnchorView(blocka,a);
					AnchorPoint terminus = new ProcessAnchorView(blockb,b);
					diagramView.addConnection(origin,terminus);   // AnchorPoints
				}

				else {
					log.warnf("%s: createDiagramView: Failed to find block for anchor point %s or %s",TAG,a,b);
				}
			}
			else {
				log.warnf("%s: createDiagramView: Connection %s has no anchor points",TAG,scxn.toString());
			}
		}
		return diagramView;
	}
	
	
	@Override
	public void addBlock(Block blk) {
		if( blk instanceof ProcessBlockView) {
			blockMap.put(blk.getId(), (ProcessBlockView)blk);
			fireStateChanged();
		}
	}
	/**
	 * Create a POJO object from this model suitable for XML serialization.
	 * @return an equivalent serializable diagram.
	 */
	public SerializableDiagram createSerializableRepresentation() {
		SerializableDiagram diagram = new SerializableDiagram();
		diagram.setName(name);
		List<SerializableBlock> sblocks = new ArrayList<SerializableBlock>();
		for( ProcessBlockView blk:blockMap.values()) {
			sblocks.add(convertBlockViewToSerializable(blk));
		}
		diagram.setBlocks(sblocks.toArray(new SerializableBlock[sblocks.size()]));
		
		List<SerializableConnection> scxns = new ArrayList<SerializableConnection>();
		for( Connection cxn:connections) {
			scxns.add(convertConnectionToSerializable(cxn));
		}
		diagram.setConnections(scxns.toArray(new SerializableConnection[scxns.size()]));
		return diagram;
	}

	@Override
	public void addConnection(AnchorPoint begin, AnchorPoint end) {
		connections.add(new LookupConnection(this,begin,end));
		fireStateChanged();

	}

	@Override
	public void deleteBlock(Block blk) {
		blockMap.remove(blk.getId());
		fireStateChanged();
	}

	@Override
	public void deleteConnection(AnchorPoint begin, AnchorPoint end) {
		for(Connection cxn:connections) {
			if( cxn.getOrigin()==begin && cxn.getTerminus()==end) {
				connections.remove(cxn);
				fireStateChanged();
				break;
			}
		}
	}

	@Override
	public Block getBlock(UUID key) {
		return blockMap.get(key);
	}

	@Override
	public Iterable<? extends Block> getBlocks() {
		return blockMap.values();
	}

	@Override
	public UUID getConnectedSetRoot() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<Connection> getConnections() {
		return connections;
	}

	@Override
	public String getDiagramName() {
		return name;
	}

	@Override
	public Dimension getDiagramSize() {
		return diagramSize;
	}

	@Override
	public long getResourceId() {
		return resourceId;
	}

	@Override
	public void setDiagramSize(Dimension dim) {
		diagramSize = dim;
		fireStateChanged();
	}
	
	// ====================== Serialization Helper Methods ===================
	private SerializableAnchor convertAnchorToSerializable(AnchorDescriptor anchor,ProcessBlockView block) {
		SerializableAnchor result = new SerializableAnchor();
		result.setDirection(anchor.getType()==AnchorType.Origin?AnchorDirection.OUTGOING:AnchorDirection.INCOMING);
		result.setDisplay(anchor.getDisplay());
		result.setId(anchor.getId());
		result.setParentId(block.getId());
		return result;
	}
	private SerializableAnchorPoint convertAnchorPointToSerializable(AnchorPoint anchor) {
		SerializableAnchorPoint result = new SerializableAnchorPoint();
		if(anchor.isConnectorOrigin()) result.setDirection(AnchorDirection.OUTGOING);
		else result.setDirection(AnchorDirection.INCOMING);
		result.setId(anchor.getId());
		result.setParentId(anchor.getBlock().getId());
		result.setAnchorX(anchor.getAnchor().x);
		result.setAnchorY(anchor.getAnchor().y);
		result.setHotSpot(anchor.getHotSpot().getBounds());
		result.setPathLeaderX(anchor.getPathLeader().x);
		result.setPathLeaderY(anchor.getPathLeader().y);
		return result;
	}
	private SerializableBlock convertBlockViewToSerializable(ProcessBlockView block) {
		SerializableBlock result = new SerializableBlock();
		result.setId(block.getId());
		result.setClassName(block.getClassName());
		result.setEmbeddedIcon(block.getEmbeddedIcon());
		result.setEmbeddedLabel(block.getEmbeddedLabel());
		result.setLabel(block.getLabel());
		result.setStyle(block.getStyle());
		result.setX(block.getLocation().x);
		result.setY(block.getLocation().y);
		
		List<SerializableAnchor> anchors = new ArrayList<SerializableAnchor>();
		for( AnchorDescriptor anchor:block.getAnchors()) {
			anchors.add(convertAnchorToSerializable(anchor,block));
		}
		result.setAnchors(anchors.toArray(new SerializableAnchor[anchors.size()]));
		if( block.getProperties()!=null ) {
			log.tracef("%s: convertBlockViewToSerializable: %s has %d properties",TAG,block.getClassName(),block.getProperties().size());
			result.setProperties(block.getProperties().toArray(new BlockProperty[block.getProperties().size()]));
		}
		else {
			log.warnf("%s: convertBlockViewToSerializable: %s has no properties",TAG,block.getClassName());
		}
		
		return result;
	}
	
	private SerializableConnection convertConnectionToSerializable(Connection cxn) {
		SerializableConnection result = new SerializableConnection();
		if( cxn.getOrigin()!=null && cxn.getTerminus()!=null ) {	
			result.setBeginBlock(cxn.getOrigin().getBlock().getId()); 
			result.setEndBlock(cxn.getTerminus().getBlock().getId());
			result.setBeginAnchor(convertAnchorPointToSerializable(cxn.getOrigin()));
			result.setEndAnchor(convertAnchorPointToSerializable(cxn.getTerminus()));
		}
		else {
			log.warnf("%s: convertConnectionToSerializable: connection missing terminus or origin (%s)",TAG,cxn.getClass().getName());
		}
		return result;
	}
	
}
