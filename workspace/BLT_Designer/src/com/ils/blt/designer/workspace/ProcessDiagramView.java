package com.ils.blt.designer.workspace;

import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.serializable.SerializableAnchorPoint;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.inductiveautomation.ignition.common.util.AbstractChangeable;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Block;
import com.inductiveautomation.ignition.designer.blockandconnector.model.BlockDiagramModel;
import com.inductiveautomation.ignition.designer.blockandconnector.model.Connection;
import com.inductiveautomation.ignition.designer.blockandconnector.model.impl.LookupConnection;

/**
 * This class represents a diagram in the designer.
 */
public class ProcessDiagramView extends AbstractChangeable implements BlockDiagramModel {
	private static final String TAG = "ProcessDiagramView";
	private static LoggerEx log = LogUtil.getLogger(ProcessDiagramView.class.getPackage().getName());
	private final Map<UUID,ProcessBlockView> blockMap = new HashMap<UUID,ProcessBlockView>();
	private List<Connection> connections = new ArrayList<Connection>();
	private Dimension diagramSize = new Dimension(800,600);
	private final long resourceId;
	private final String name;
	
	public ProcessDiagramView(long resId,String nam) {
		this.resourceId = resId;
		this.name = nam;
	}
	/**
	 * Constructor: Create an instance given a SerializableDiagram
	 * @param resid
	 * @param diagram
	 */
	public ProcessDiagramView (long resid,SerializableDiagram diagram) {
		this(resid,diagram.getName());

		for( SerializableBlock sb:diagram.getBlocks()) {
			ProcessBlockView pbv = new ProcessBlockView(sb);
			blockMap.put(sb.getId(), pbv);
			log.warnf("%s: createDiagramView: Added %s to map",TAG,sb.getId().toString());
			this.addBlock(pbv);
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
					this.addConnection(origin,terminus);   // AnchorPoints
				}
				else {
					if( blocka==null ) {
						log.warnf("%s: createDiagramView: Failed to find block %s for begin anchor point %s",TAG,a.getParentId(),a);
					}
					if( blockb==null ) {
						log.warnf("%s: createDiagramView: Failed to find block %s for end anchor point %s",TAG,b.getParentId(),b);
					}
				}
			}
			else {
				log.warnf("%s: createDiagramView: Connection %s missing one or more anchor points",TAG,scxn.toString());
			}
		}
	}
	
	
	@Override
	public void addBlock(Block blk) {
		if( blk instanceof ProcessBlockView) {
			blockMap.put(blk.getId(), (ProcessBlockView)blk);
			fireStateChanged();
		}
	}
	/**
	 * Create a POJO object from this model suitable for JSON serialization.
	 * @return an equivalent serializable diagram.
	 */
	public SerializableDiagram createSerializableRepresentation() {
		SerializableDiagram diagram = new SerializableDiagram();
		diagram.setName(name);
		List<SerializableBlock> sblocks = new ArrayList<SerializableBlock>();
		for( ProcessBlockView blk:blockMap.values()) {
			SerializableBlock sb = blk.convertToSerializable();
			sblocks.add(sb);
		}
		diagram.setBlocks(sblocks.toArray(new SerializableBlock[sblocks.size()]));
		
		
		
		// As we iterate the connections, update SerializableAnchors with connection types
		List<SerializableConnection> scxns = new ArrayList<SerializableConnection>();
		for( Connection cxn:connections) {
			SerializableConnection scxn = convertConnectionToSerializable(cxn);
			// Set the connection type to the begin block type
			ProcessBlockView beginBlock = blockMap.get(scxn.getBeginBlock());
			if( beginBlock!=null ) {
				String port = scxn.getBeginAnchor().getId().toString();
				boolean found = false;
				for(ProcessAnchorDescriptor desc:beginBlock.getAnchors()) {
					if( desc.getDisplay().equalsIgnoreCase(port) ) {
						found = true;
						scxn.setType(desc.getConnectionType());
					}
				}
				if( !found ) log.warnf("%s.createSerializableRepresentation: unable to find %s port in begin block",TAG,port);
			}
			else {
				log.warnf("%s.createSerializableRepresentation: begin block lookup failed",TAG);
			}
			scxns.add(scxn);
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
		
	// NOTE: This does not set connection type
	private SerializableConnection convertConnectionToSerializable(Connection cxn) {
		SerializableConnection result = new SerializableConnection();
		if( cxn.getOrigin()!=null && cxn.getTerminus()!=null ) {	
			result.setBeginBlock(cxn.getOrigin().getBlock().getId()); 
			result.setEndBlock(cxn.getTerminus().getBlock().getId());
			result.setBeginAnchor(new SerializableAnchorPoint(cxn.getOrigin()));
			result.setEndAnchor(new SerializableAnchorPoint(cxn.getTerminus()));
		}
		else {
			log.warnf("%s.convertConnectionToSerializable: connection missing terminus or origin (%s)",TAG,cxn.getClass().getName());
		}
		return result;
	}
	
}
