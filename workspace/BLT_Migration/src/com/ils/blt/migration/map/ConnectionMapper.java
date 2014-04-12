package com.ils.blt.migration.map;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.common.AnchorDirection;
import com.ils.blt.common.serializable.SerializableAnchor;
import com.ils.blt.common.serializable.SerializableAnchorPoint;
import com.ils.blt.common.serializable.SerializableBlock;
import com.ils.blt.common.serializable.SerializableConnection;
import com.ils.blt.common.serializable.SerializableDiagram;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.blt.designer.workspace.ui.BlockViewUI;
import com.ils.blt.designer.workspace.ui.UIFactory;
import com.ils.blt.migration.G2Block;
import com.ils.blt.migration.G2Anchor;
import com.ils.blt.migration.G2Diagram;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.blockandconnector.model.AnchorPoint;

/**
 * Determine connections based on port descriptions of blocks. While
 * we're at to we set the block anchors as well.
 */
public class ConnectionMapper {
	private final String TAG = "ConnectionMapper";
	private final LoggerEx log;
	private final Map<String,SerializableAnchor> anchorMap;   // Key is UUID:port
	private final Map<String,SerializableBlock> blockMap;     // Key is UUID
	private final UIFactory factory;

	/** 
	 * Constructor: 
	 */
	public ConnectionMapper() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		anchorMap = new HashMap<String,SerializableAnchor>();
		blockMap = new HashMap<String,SerializableBlock>();
		factory = new UIFactory();
	}

	/**
	 * There is a 1-1 mapping of the ports defined on G2 block
	 * with the anchors defined in an Ignition block.
	 * 
	 * @param g2block the input block from G2
	 * @param iblock outgoing Ignition equivalent
	 */
	public void setAnchors(G2Block g2block,SerializableBlock iblock) {
		G2Anchor[] g2cxns = g2block.getConnections();
		List<SerializableAnchor> anchorList = new ArrayList<SerializableAnchor>();
		for( G2Anchor g2cxn:g2cxns ) {
			SerializableAnchor anchor = new SerializableAnchor();
			anchor.setConnectionType(g2cxn.getConnectionType());
			anchor.setDirection(g2cxn.getAnchorDirection());
			anchor.setDisplay(g2cxn.getPort());
			anchor.setId(UUID.randomUUID());
			anchor.setParentId(iblock.getId());
			String key = makeAnchorMapKey(iblock.getId(),anchor.getDisplay());
			if( anchorMap.get(key)==null ) {   // Weed out duplicates
				anchorMap.put(key, anchor);	
				log.tracef("%s: anchorMap key = %s",TAG,key);
				anchorList.add(anchor);
			}
		}
		SerializableAnchor[] anchors = anchorList.toArray(new SerializableAnchor[anchorList.size()]);
		iblock.setAnchors(anchors);
		blockMap.put(iblock.getId().toString(), iblock);
		log.debugf("%s.setAnchors: blockMap key = %s",TAG,iblock.getId().toString());
	}

	/**
	 * Analyze the blocks in the G2Diagram and deduce connections based on block Ids.
	 * Turn the resulting map into Ignition connections. Add to the Ignition diagram.
	 * Rely on maps already created by "setAnchors" method.
	 */
	public void createConnections(G2Diagram g2diagram,SerializableDiagram diagram) {
		// On the G2 side, connections are defined with each port on a block.
		// We therefore get duplicates. Use a map to sort out the differences.
		// Key is name(from):name(to). The G2 names are unique within a knowledge base
		Map<String,SerializableConnection> connectionMap = new HashMap<String,SerializableConnection>();
		
		for(G2Block g2block:g2diagram.getBlocks()) {
			for(G2Anchor g2anchor:g2block.getConnections()) {
				String key = "";
				if( g2anchor.getAnchorDirection().equals(AnchorDirection.INCOMING)) {
					key = g2anchor.getBlockName()+":"+g2block.getName();
					log.tracef("%s.createConnections: connectionMap INCOMING key = %s",TAG,key);
				}
				else {
					key = g2block.getName()+":"+g2anchor.getBlockName();
					log.tracef("%s: connectionMap OUTGOING key = %s",TAG,key);
				}
				SerializableConnection cxn = connectionMap.get(key);
				if( cxn==null ) {
					// We haven't seen this before ...
					cxn = new SerializableConnection();
					connectionMap.put(key, cxn);
					log.tracef("%s.createConnections: connectionMap ----- was new entry",TAG);
					cxn.setType(g2anchor.getConnectionType());
				}
				// Set to or from blocks
				// Then create AnchorPoint for the end where we know the port name
				String port = g2anchor.getPort();
				if( g2anchor.getAnchorDirection().equals(AnchorDirection.INCOMING)) {
					cxn.setBeginBlock(UUID.nameUUIDFromBytes(g2anchor.getBlockName().getBytes()));
					cxn.setEndBlock(UUID.nameUUIDFromBytes(g2block.getUuid().getBytes()));
					setEndAnchorPoint(cxn,cxn.getEndBlock(),port);
				}
				else {
					cxn.setBeginBlock(UUID.nameUUIDFromBytes(g2block.getUuid().getBytes()));
					cxn.setEndBlock(UUID.nameUUIDFromBytes(g2anchor.getBlockName().getBytes()));
					setBeginAnchorPoint(cxn,cxn.getBeginBlock(),port);
				}	
			}
		}
		// Finally walk the map and add connections to the diagram.
		Collection<SerializableConnection> collection = connectionMap.values();
		SerializableConnection[] connections = collection.toArray(new SerializableConnection[collection.size()]);
		diagram.setConnections(connections);
	}
	
	// Set anchor point at origin
	private void setBeginAnchorPoint(SerializableConnection cxn,UUID blockId,String port) {
		String key = makeAnchorMapKey(blockId,port);
		SerializableAnchor anchor = anchorMap.get(key);
		if( anchor!=null ) {
			SerializableBlock block = blockMap.get(blockId.toString());
			if( block!=null) {
				ProcessBlockView blockView = new ProcessBlockView(block);
				BlockViewUI ui = factory.getUI(block.getStyle(), blockView);
				Collection<AnchorPoint>anchorPoints = ui.getAnchorPoints();
				// Look for the specific anchorPoint - in our usage, the id is the port name
				AnchorPoint pt = null;
				for( AnchorPoint ap:anchorPoints ) {
					if( ap.getId().toString().equals(port)) {
						pt = ap;
						break;
					}
				}
				if( pt!=null ) {
					SerializableAnchorPoint sap = createSerializableAnchorPoint(pt);
					cxn.setBeginAnchor(sap);
				}
				else {
					System.err.println(TAG+".setBeginAnchorPoint: Port lookup failed for "+blockId+" ("+port+")");
				}
			}
			else {
				System.err.println(TAG+".setBeginAnchorPoint: Block lookup failed for "+blockId);
			}
		}
		else {
			System.err.println(TAG+".setBeginAnchorPoint: Anchor lookup failed for "+key);
		}
	}

	// Set anchor point at terminus
	private void setEndAnchorPoint(SerializableConnection cxn,UUID blockId,String port) {
		String key = makeAnchorMapKey(blockId,port);
		SerializableAnchor anchor = anchorMap.get(key);
		if( anchor!=null ) {
			SerializableBlock block = blockMap.get(blockId.toString());
			if( block!=null) {
				ProcessBlockView blockView = new ProcessBlockView(block);
				BlockViewUI ui = factory.getUI(block.getStyle(), blockView);
				Collection<AnchorPoint>anchorPoints = ui.getAnchorPoints();
				// Look for the specific anchorPoint - in our usage, the id is the port name
				AnchorPoint pt = null;
				for( AnchorPoint ap:anchorPoints ) {
					if( ap.getId().toString().equals(port)) {
						pt = ap;
						break;
					}
				}
				if( pt!=null ) {
					SerializableAnchorPoint sap = createSerializableAnchorPoint(pt);
					cxn.setEndAnchor(sap);
				}
				else {
					System.err.println(TAG+".setEndAnchorPoint: Port lookup failed for "+blockId+" ("+port+")");
				}
			}
			else {
				System.err.println(TAG+".setEndAnchorPoint: Block lookup failed for "+blockId);
			}
		}
		else {
			System.err.println(TAG+".setEndAnchorPoint: Anchor lookup failed for "+key);
		}
	}
	
	/**
	 * NOTE: This would normally be an alternative constructor for SerializableAnchorPoint.
	 *        Problem is that we need to keep that class free of references to Designer-only
	 *        classes (e.g. AnchorPoint).
	 * @see ProcessDiagramView
	 * @param anchor
	 */
	private SerializableAnchorPoint createSerializableAnchorPoint(AnchorPoint anchor) {
		SerializableAnchorPoint sap = new SerializableAnchorPoint();
		if(anchor.isConnectorOrigin()) sap.setDirection(AnchorDirection.OUTGOING);
		else sap.setDirection(AnchorDirection.INCOMING);
		sap.setId(anchor.getId());
		sap.setParentId(anchor.getBlock().getId());
		sap.setAnchorX(anchor.getAnchor().x);
		sap.setAnchorY(anchor.getAnchor().y);
		sap.setHotSpot(anchor.getHotSpot().getBounds());
		sap.setPathLeaderX(anchor.getPathLeader().x);
		sap.setPathLeaderY(anchor.getPathLeader().y);
		return sap;
	}
	
	/**
	 * Create the key for lookup in the anchorMap
	 */
	private String makeAnchorMapKey(UUID id, String port) {
		String key = id.toString()+":"+port;
		return key;
	}
}
	

