package com.ils.blt.migration.map;

import java.util.Collection;
import java.util.HashMap;
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
import com.ils.blt.migration.G2Connection;
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
		G2Connection[] g2cxns = g2block.getConnections();
		SerializableAnchor[] anchors = new SerializableAnchor[g2cxns.length];
		int index = 0;
		for( G2Connection g2cxn:g2block.getConnections() ) {
			SerializableAnchor anchor = new SerializableAnchor();
			anchor.setConnectionType(g2cxn.getType());
			anchor.setDirection(g2cxn.getDirection());
			anchor.setDisplay(g2cxn.getPort());
			anchor.setId(UUID.randomUUID());
			anchor.setParentId(iblock.getId());
			String key = iblock.getId().toString()+":"+anchor.getDisplay();
			anchorMap.put(key, anchor);	
			log.debugf("%s: anchorMap key = %s",TAG,key);
			anchors[index] = anchor;
			index++;
		}
		iblock.setAnchors(anchors);
		blockMap.put(iblock.getId().toString(), iblock);
		log.debugf("%s: blockMap key = %s",TAG,iblock.getId().toString());
	}

	/**
	 * Analyze the blocks in the G2Diagram and deduce connections based on block Ids.
	 * Turn the resulting map into Ignition connections. Add to the Ignition diagram.
	 * Rely on maps already created by "setAnchors" method.
	 */
	public void createConnections(G2Diagram g2diagram,SerializableDiagram diagram) {
		// On the G2 side, connections are defined with each port on a block.
		// We therefore get duplicates. Use a map to sort out the differences.
		// Key is UUID(from):UUID(to)
		Map<String,SerializableConnection> connectionMap = new HashMap<String,SerializableConnection>();
		
		for(G2Block g2block:g2diagram.getBlocks()) {
			for(G2Connection g2cxn:g2block.getConnections()) {
				String key = "";
				if( g2cxn.getDirection()==AnchorDirection.INCOMING) {
					key = g2cxn.getBlock().toString()+":"+g2block.getId().toString();
					log.debugf("%s: connectionMap INCOMING key = %s",TAG,key);
				}
				else {
					key = g2block.getId().toString()+":"+g2cxn.getBlock().toString();
					log.debugf("%s: connectionMap OUTGOING key = %s",TAG,key);
				}
				SerializableConnection cxn = connectionMap.get(key);
				if( cxn==null ) {
					// We haven't seen this before ...
					cxn = new SerializableConnection();
					connectionMap.put(key, cxn);
					log.debugf("%s: connectionMap ----- was new entry",TAG);
					cxn.setType(g2cxn.getType());
					// Set to - from blocks
					// Then create AnchorPoint for the end where we know the port name
					String port = g2cxn.getPort();
					if( g2cxn.getDirection()==AnchorDirection.INCOMING) {
						cxn.setBeginBlock(g2cxn.getBlock());
						cxn.setEndBlock(g2block.getId());
						setEndAnchorPoint(cxn,cxn.getEndBlock(),port);
					}
					else {
						cxn.setBeginBlock(g2block.getId());
						cxn.setEndBlock(g2cxn.getBlock());
						setBeginAnchorPoint(cxn,cxn.getBeginBlock(),port);
					}
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
		String key = blockId.toString()+":"+port;
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
					SerializableAnchorPoint sap = ProcessDiagramView.convertAnchorPointToSerializable(pt);
					cxn.setBeginAnchor(sap);
				}
				else {
					System.err.println(TAG+".createConnections: Port lookup failed for "+blockId+" ("+port+")");
				}
			}
			else {
				System.err.println(TAG+".createConnections: Block lookup failed for "+blockId);
			}
		}
		else {
			System.err.println(TAG+".createConnections: Anchor lookup failed for "+blockId+" ("+port+")");
		}
	}

	// Set anchor point at terminus
	private void setEndAnchorPoint(SerializableConnection cxn,UUID blockId,String port) {
		String key = blockId.toString()+":"+port;
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
					SerializableAnchorPoint sap = ProcessDiagramView.convertAnchorPointToSerializable(pt);
					cxn.setEndAnchor(sap);
				}
				else {
					System.err.println(TAG+".createConnections: Port lookup failed for "+blockId+" ("+port+")");
				}
			}
			else {
				System.err.println(TAG+".createConnections: Block lookup failed for "+blockId);
			}
		}
		else {
			System.err.println(TAG+".createConnections: Anchor lookup failed for "+blockId+" ("+port+")");
		}
	}
}
	

