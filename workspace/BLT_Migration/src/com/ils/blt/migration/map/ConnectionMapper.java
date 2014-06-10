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
import com.ils.blt.migration.G2Anchor;
import com.ils.blt.migration.G2Block;
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
	private final Map<String,SerializableBlock> blockFromG2Map;     // Key is G2 block UUID
	private final UIFactory factory;
	// These are used for connection post resolution
	private final Map<String,ConnectionPostEntry> sinkPosts;
	private final Map<String,ConnectionPostEntry> sourcePosts;
	private final Map<String,SerializableConnection> inCxnByBlockName;    // Keyed by the block they connect to
	private final Map<String,SerializableConnection> outCxnByBlockName;
	/** 
	 * Constructor: 
	 */
	public ConnectionMapper() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		anchorMap = new HashMap<String,SerializableAnchor>();
		blockMap = new HashMap<String,SerializableBlock>();
		blockFromG2Map = new HashMap<String,SerializableBlock>();
		factory = new UIFactory();
		sinkPosts = new HashMap<String,ConnectionPostEntry>();
		sourcePosts = new HashMap<String,ConnectionPostEntry>();
		inCxnByBlockName = new HashMap<String,SerializableConnection>();
		outCxnByBlockName = new HashMap<String,SerializableConnection>();
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
				log.tracef("%s.setAnchors: anchorMap key = %s",TAG,key);
				anchorList.add(anchor);
			}
		}
		SerializableAnchor[] anchors = anchorList.toArray(new SerializableAnchor[anchorList.size()]);
		iblock.setAnchors(anchors);
		blockMap.put(iblock.getId().toString(), iblock);
		blockFromG2Map.put(g2block.getUuid(), iblock);
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
		// Key is name(from):name(to). The G2 names are unique within a knowledge base.
		Map<String,SerializableConnection> connectionMap = new HashMap<String,SerializableConnection>();
		
		for(G2Block g2block:g2diagram.getBlocks()) {
			for(G2Anchor g2anchor:g2block.getConnections()) {
				String key = "";
				if( g2anchor.getAnchorDirection().equals(AnchorDirection.INCOMING)) {
					key = g2anchor.getBlockName()+":"+g2block.getName();
					log.infof("%s.createConnections: connectionMap INCOMING key = %s",TAG,key);
				}
				else {
					key = g2block.getName()+":"+g2anchor.getBlockName();
					log.infof("%s: connectionMap OUTGOING key = %s",TAG,key);
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
					/*
					cxn.setBeginBlock(UUID.nameUUIDFromBytes(g2anchor.getBlockName().getBytes()));
					cxn.setEndBlock(UUID.nameUUIDFromBytes(g2block.getUuid().getBytes()));
					*/
					//G2Block g2BeginBlock = blockByName(g2anchor.getBlockName())
					SerializableBlock beginBlock = blockFromG2Map.get(g2block.getUuid());
					cxn.setEndBlock(beginBlock.getId());
					SerializableBlock endBlock = blockFromG2Map.get(g2block.getUuid());
					cxn.setEndBlock(endBlock.getId());
					setEndAnchorPoint(cxn,cxn.getEndBlock(),port);
				}
				else {
					/*
					cxn.setBeginBlock(UUID.nameUUIDFromBytes(g2block.getUuid().getBytes()));
					cxn.setEndBlock(UUID.nameUUIDFromBytes(g2anchor.getBlockName().getBytes()));
					*/
					SerializableBlock beginBlock = blockFromG2Map.get(g2block.getUuid());
					cxn.setBeginBlock(beginBlock.getId());
					cxn.setEndBlock(UUID.nameUUIDFromBytes(g2anchor.getBlockName().getBytes()));
					setBeginAnchorPoint(cxn,cxn.getBeginBlock(),port);
				}	
				log.infof("%s.createConnections: connection %s",TAG,cxn.toString());
			}
		}
		// Finally walk the map and add connections to the diagram. Before doing this cull out any that
		// are incomplete. They will be incomplete because the anchor points are incomplete.
		// NOTE: G2 connection posts don't have stubs.
		Collection<SerializableConnection> collection = connectionMap.values();
		Collection<SerializableConnection> toDelete = new ArrayList<SerializableConnection>();
		// TODO
		for(SerializableConnection cxn:collection) {
			System.err.println(cxn.toString());
		
			if( cxn.getBeginAnchor()==null) {
				SerializableBlock sb = blockMap.get(cxn.getEndBlock().toString());
				if( sb!=null ) {
					if( sb.getClassName().endsWith("Connection")) {
						
					}
					else {
						
					}
				}
				else {
					log.warnf("%s.createConnections: Null end block found in connection");
				}
			}
			else if(cxn.getEndAnchor()==null) {
				SerializableBlock sb = blockMap.get(cxn.getBeginBlock().toString());
				if( sb!=null ) {
					if( sb.getClassName().endsWith("Connection")) {
						
					}
					else {
						
					}
				}
				else {
					log.warnf("%s.createConnections: Null begin block found in connection");
				}
			}
		}
		for(SerializableConnection sc:toDelete) {
			collection.remove(sc);
		}
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
					if( ap.getId() !=null ) {
						if( ap.getId().toString().equals(port)) {
							pt = ap;
							break;
						}
					}
					else {
						System.err.println(TAG+".setEndAnchorPoint: No port name  ("+ap.toString()+")");
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
	
	/**
	 * Reconcile links through connection posts that (probably) span diagrams. Create
	 * connections between the posts and the block connected to it on the same diagram.
	 */
	public void reconcileUnresolvedConnections() {
		// Loop over all the sink posts
		// TODO
		for( ConnectionPostEntry sink:sinkPosts.values()) {
			// Find the matching pair
			ConnectionPostEntry source = sourcePosts.get(sink.getName());
			if( source!=null ) {
				// Create source to block connection
				/*
				G2Anchor sourceAnchor = inAnchorsByBlockName.get(source.getTarget().getName());
				if( sourceAnchor!=null ) {
					SerializableConnection sourceConnection = new SerializableConnection();
					sourceConnection.setType(sourceAnchor.getConnectionType());
					sourceConnection.setBeginBlock(source.getPost().getId());
					setBeginAnchorPoint(sourceConnection,source.getPost().getId(),"out");
					sourceConnection.setEndBlock(source.getTarget().getId());
					setEndAnchorPoint(sourceConnection,source.getTarget().getId(),sourceAnchor.getPort());
					//source.getParent().addConnection(sourceConnection);
				}
				else {
					System.err.println(String.format("%s: performSpecialHandlingOnApplication failed to find target anchor for source %s",TAG,source.getName()));
				}
				*/
				// Create block to sink connection
				/*
				G2Anchor sinkAnchor = outAnchorsByBlockName.get(sink.getTarget().getName());
				if( sinkAnchor!=null ) {
					SerializableConnection sinkConnection = new SerializableConnection();
					sinkConnection.setType(sinkAnchor.getConnectionType());
					sinkConnection.setBeginBlock(sink.getTarget().getId());
					setBeginAnchorPoint(sinkConnection,sink.getTarget().getId(),sinkAnchor.getPort());
					sinkConnection.setEndBlock(sink.getPost().getId());
					setEndAnchorPoint(sinkConnection,sink.getPost().getId(),"in");
					//sink.getParent().addConnection(sinkConnection);
				}
				else {
					System.err.println(String.format("%s: performSpecialHandlingOnApplication failed to find target anchor for sink %s",TAG,sink.getName()));
				}
				*/
			}
			else {
				System.err.println(String.format("%s: performSpecialHandlingOnApplication no matching source for sink post name %s",TAG,sink.getName()));
			}
		}
	}
	
	/**
	 * This class is used in connection post resolution.
	 */
	private class ConnectionPostEntry {
		private final String name;   // Name of the post
		private final SerializableBlock post;
		private final SerializableBlock target;       
		private final SerializableDiagram parent;    
 
		public ConnectionPostEntry( String name,SerializableBlock post,SerializableDiagram parent,SerializableBlock target) {
			this.name = name;
			this.post = post;
			this.parent = parent;
			this.target = target;
		}
		public String getName() { return this.name; }
		public SerializableBlock getPost() {return post;}
		public SerializableBlock getTarget() {return target;}
		public SerializableDiagram getParent() {return parent;}
		
	}
}
	

