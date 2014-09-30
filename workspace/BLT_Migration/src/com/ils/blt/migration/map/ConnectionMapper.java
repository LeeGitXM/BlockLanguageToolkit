package com.ils.blt.migration.map;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.connection.ConnectionType;
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
	private final Map<String,List<SerializableConnection>> connectionMap;
	private final Map<UUID,SerializableDiagram> diagramForBlockId;   // Key is block UUID
	private final UIFactory factory;
	// These are used for connection post resolution
	private final List<ConnectionPostEntry> sinkPosts;
	private final List<ConnectionPostEntry> sourcePosts;
	private final Map<UUID,AnchorPointEntry> anchorPointForSinkBlock; // Unresolved anchorPoints
	private final Map<UUID,AnchorPointEntry> anchorPointForSourceBlock;
	/** 
	 * Constructor: 
	 */
	public ConnectionMapper() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		anchorMap = new HashMap<String,SerializableAnchor>();
		blockMap = new HashMap<String,SerializableBlock>();
		connectionMap = new HashMap<String,List<SerializableConnection>>();
		diagramForBlockId = new HashMap<UUID,SerializableDiagram>();
		factory = new UIFactory();
		sinkPosts = new ArrayList<ConnectionPostEntry>();
		sourcePosts = new ArrayList<ConnectionPostEntry>();
		anchorPointForSinkBlock = new HashMap<UUID,AnchorPointEntry>();
		anchorPointForSourceBlock = new HashMap<UUID,AnchorPointEntry>();
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
		log.debugf("%s.setAnchors: blockMap key = %s",TAG,iblock.getId().toString());
	}

	/**
	 * Analyze the blocks in the G2Diagram and deduce connections based on block Ids.
	 * Turn the resulting map into Ignition connections. Add to the Ignition diagram.
	 * Rely on maps already created by "setAnchors" method.
	 */
	public void createConnectionSegments(G2Diagram g2diagram,SerializableDiagram diagram) {
		// On the G2 side, connections are defined with each port on a block.
		// We therefore get duplicates. Use a map to sort out the differences.
		// Key is name(from):name(to). The G2 names are unique within a knowledge base.
		for(G2Block g2block:g2diagram.getBlocks()) {
			for(G2Anchor g2anchor:g2block.getConnections()) {
				String key = "";
				if( g2anchor.getAnchorDirection().equals(AnchorDirection.INCOMING)) {
					key = makeConnectionMapKey(diagram.getName(),g2anchor.getBlockName(),g2block.getName());
					log.tracef("%s.createConnectionSegments: connectionMap INCOMING key = %s",TAG,key);
				}
				else {
					key = makeConnectionMapKey(diagram.getName(),g2block.getName(),g2anchor.getBlockName());
					log.tracef("%s.createConnectionSegments: connectionMap OUTGOING key = %s",TAG,key);
				}
				SerializableConnection cxn = getConnectionFromFragment(key,g2anchor);

				// Set begin or end block depending on the direction,
				// then create AnchorPoint for the end where we know the port name
				String port = g2anchor.getPort();
				if( g2anchor.getAnchorDirection().equals(AnchorDirection.INCOMING)) {
					cxn.setBeginBlock(UUID.nameUUIDFromBytes(g2anchor.getUuid().getBytes()));
					cxn.setEndBlock(UUID.nameUUIDFromBytes(g2block.getUuid().getBytes()));
					setEndAnchorPoint(cxn,cxn.getEndBlock(),port);
				}
				else {
					cxn.setBeginBlock(UUID.nameUUIDFromBytes(g2block.getUuid().getBytes()));
					cxn.setEndBlock(UUID.nameUUIDFromBytes(g2anchor.getUuid().getBytes()));
					setBeginAnchorPoint(cxn,cxn.getBeginBlock(),port);
				}	
				log.debugf("%s.createConnectionSegments: connection=%s",TAG,cxn.toString());
			}
		}
		// Walk the diagram an create a lookup of diagram by blockId
		for( SerializableBlock blk:diagram.getBlocks()) {
			diagramForBlockId.put(blk.getId(), diagram);
		}
	}

	/**
	 *  Now that we have filled all the various lookup tables, walk the map and add connections to the diagrams.
	 *   Before doing this cull out any that are incomplete. They will be incomplete because the anchor points do not have stubs.
	 * NOTE: G2 connection posts don't have stubs.
	 */ 
	public void createConnections() {	
		Collection<List<SerializableConnection>> collections = connectionMap.values();
		for( List<SerializableConnection> list:collections) {
			for(SerializableConnection cxn:list) {
				SerializableAnchorPoint beginAnchor = cxn.getBeginAnchor();
				SerializableAnchorPoint endAnchor = cxn.getEndAnchor();
				SerializableBlock beginBlock = null;
				UUID beginBlockId = cxn.getBeginBlock();
				if(beginBlockId!=null ) {
					beginBlock = blockMap.get(beginBlockId.toString());
				}
				
				SerializableBlock endBlock = null;
				UUID endBlockId = cxn.getEndBlock();
				if(endBlockId!=null ) {
					endBlock = blockMap.get(endBlockId.toString());
				}

				// Check for error - shouldn't happen
				if(beginBlock!=null && endBlock!=null &&
						(beginAnchor!=null || endAnchor!=null) ) {
					// There are 4 special cases relating to connection posts
					if(beginAnchor==null || endAnchor==null)  {
						if(endAnchor!=null) {
							if(endBlock.getClassName().endsWith("Connection")) {
								sinkPosts.add(new ConnectionPostEntry(
										endBlock,
										diagramForBlockId.get(endBlock.getId()),
										beginBlock,
										endAnchor.getDirection()));
							}
							else {
								log.debugf("%s.createConnections: anchorPointForSource: %s (%s)",TAG,endBlock.getId().toString(),endBlock.getName());
								anchorPointForSourceBlock.put(endBlock.getId(),
										new AnchorPointEntry(endAnchor,cxn.getType()));
							}
						}
						else if(beginAnchor!=null){
							if(beginBlock.getClassName().endsWith("Connection")) {
								sourcePosts.add(new ConnectionPostEntry(
										beginBlock,
										diagramForBlockId.get(beginBlock.getId()),
										endBlock,
										beginAnchor.getDirection()));
							}
							else {
								log.debugf("%s.createConnections: anchorPointForSink: %s (%s)",TAG,beginBlock.getId().toString(),beginBlock.getName());
								anchorPointForSinkBlock.put(beginBlock.getId(),
										new AnchorPointEntry(beginAnchor,cxn.getType()));
							}
						}
					}
					else {
						// Normal complete connection - both blocks on same diagram
						diagramForBlockId.get(beginBlock.getId()).addConnection(cxn);
						log.debugf("%s.createConnections: NORMAL::%s",TAG,cxn.toString());;
					}
				}
				else {
					log.warnf("%s.createConnections: Incomplete connection=%s (ignored)",TAG,cxn.toString());
				}
			}
		}
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
	 * Create the key for lookup in the anchorMap. We are looking to hookup connections
	 * duplicated between the same blocks on the same diagram.
	 * @param diagram name of the diagram
	 */
	private String makeConnectionMapKey(String diagram, String fromName,String toName) {
		String key = String.format("%s~%s::%s",
				diagram,fromName,toName );
		return key;
	}
	
	/**
	 * Determine whether or not the specified connection fragment starts a new 
	 * connection or completes an old one.
	 */
	private SerializableConnection getConnectionFromFragment(String key,G2Anchor anchor) {
		List<SerializableConnection> list = connectionMap.get(key);
		SerializableConnection cxn = null;
		if( list == null ) {
			list = new ArrayList<SerializableConnection>();
			connectionMap.put(key, list);
		}
		AnchorDirection dir = anchor.getAnchorDirection();
		// For INCOMING, we are prepared to set the END block
		// If we find a connection that has this deficiency, then return it.
		for( SerializableConnection connection:list) {
			if(dir.equals(AnchorDirection.INCOMING)) {
				if(connection.getEndAnchor()==null) {
					return connection;
				}
			}
			else {
				if(connection.getBeginAnchor()==null) {
					return connection;
				}
			}
		}
		// If we get this far, make a new connection
		cxn = new SerializableConnection();
		cxn.setType(anchor.getConnectionType());
		list.add(cxn);
		return cxn;
	}
	
	/**
	 * Reconcile links through connection posts that (probably) span diagrams. Create
	 * connections between the posts and the block connected to it on the same diagram.
	 */
	public void reconcileUnresolvedConnections() {
		// Loop over all the sink posts
		for( ConnectionPostEntry sink:sinkPosts) {
			// Find the matching block:post
			AnchorPointEntry ape = anchorPointForSinkBlock.get(sink.getTarget().getId());
			if( ape!=null ) {
				// Create block to sink post connection
				SerializableConnection sinkConnection = new SerializableConnection();
				sinkConnection.setType(ape.getConnectionType());
				sinkConnection.setBeginBlock(sink.getTarget().getId());
				sinkConnection.setBeginAnchor(ape.getPoint());
				sinkConnection.setEndBlock(sink.getPost().getId());
				setEndAnchorPoint(sinkConnection,sink.getPost().getId(),"in");
				sink.getParent().addConnection(sinkConnection);
				log.debugf("%s.reconcileUnresolvedConnections: SINK::%s",TAG,sinkConnection);
			}
			else {
				log.warnf("%s.reconcileUnresolvedConnections: Block to sink=%s (ignored)",
						TAG,sink.getTarget().getId().toString());
			}
		}
		// Loop over all the source posts
		for( ConnectionPostEntry source:sourcePosts) {
			// Find the matching block:post
			AnchorPointEntry ape = anchorPointForSourceBlock.get(source.getTarget().getId());
			if( ape!=null ) {
				// Create block to sink post connection
				SerializableConnection sourceConnection = new SerializableConnection();
				sourceConnection.setType(ape.getConnectionType());
				sourceConnection.setBeginBlock(source.getPost().getId());
				setBeginAnchorPoint(sourceConnection,source.getPost().getId(),"out");
				sourceConnection.setEndBlock(source.getTarget().getId());
				sourceConnection.setEndAnchor(ape.getPoint());
				source.getParent().addConnection(sourceConnection);
				log.debugf("%s.reconcileUnresolvedConnections: SOURCE::%s",TAG,sourceConnection);
			}
			else {
				log.warnf("%s.reconcileUnresolvedConnections: Block to source=%s (ignored)",
						TAG,source.getTarget().getId().toString());
			}
		}
	}
	
	/**
	 * This class is used in connection post resolution.
	 */
	private class AnchorPointEntry {
		private final SerializableAnchorPoint point; 
		private final ConnectionType type;
 
		public AnchorPointEntry(SerializableAnchorPoint pt,ConnectionType ct) {
			this.point = pt;
			this.type  = ct;
		}
		public SerializableAnchorPoint getPoint() {return point;}
		public ConnectionType getConnectionType() { return type; }
	}
	/**
	 * This class is used in connection post resolution.
	 */
	private class ConnectionPostEntry {
		private final SerializableBlock post;
		private final SerializableBlock target;       
		private final SerializableDiagram parent; 
		private final AnchorDirection direction;
 
		public ConnectionPostEntry(SerializableBlock post,SerializableDiagram parent,SerializableBlock target,AnchorDirection dir) {
			this.post = post;
			this.parent = parent;
			this.target = target;
			this.direction = dir;
		}
		public SerializableBlock getPost() {return post;}
		public SerializableBlock getTarget() {return target;}
		public SerializableDiagram getParent() {return parent;}
		public AnchorDirection getDirection() { return direction; }
	}
}
	

